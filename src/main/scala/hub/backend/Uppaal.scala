package hub.backend

import hub._
import hub.analyse._
import hub.common.FormulaException
import ifta._

/**
  * Created by guillecledou on 2019-09-27
  */

/**
  * A Representation of uppaal ta for hubs
  * @param locs locations
  * @param init initial location
  * @param clocks set of clocks
  * @param edges set of transitions
  * @param inv invariants assigned to locations
  * @param committed set of committed states
  * @param act2locs a map from actions to a set of (committed) locations where is true that the action just executed
  */
case class Uppaal(locs:Set[Int],init:Int,clocks:Set[String],edges:Set[UppaalEdge],inv:Map[Int,ClockCons],
                  committed:Set[Int],act2locs:Map[Int,Set[Int]],initVal:Map[Var,Expr],name:String) {}

case class UppaalEdge(from:Int,to:Int,ports:Set[Int],ccons:ClockCons,creset:Set[String],guard:Guard,upd:Update) {}

object Uppaal {

  def emptyAutomata = Uppaal(Set(),0,Set(),Set(),Map(),Set(),Map(),Map(),"")

  /**
    * Uppaal reserved keywords
    */
  private val kw = Set("chan", "clock", "bool", "int", "commit", "const",
    "urgent", "broadcast", "init", "process", "state", "guard", "sync", "assign",
    "system", "trans", "deadlock", "and", "or", "xor", "not", "imply", "true", "false",
    "for", "forall", "exists", "while", "do", "if", "else", "return", "typedef",
    "struct", "rate", "before_update", "after_update", "meta", "priority",
    "progress", "scalar", "select", "void", "default","switch","case","continue","break")

  /**
    * Given an Uppaal ta returns xml model
    * @param hub uppaal ta
    * @return a string with the uppaal model (xml)
    */
  def apply(uppaal:Uppaal):String = {

    apply(Set(uppaal))

  }

  /**
    * Given an Uppaal ta returns xml model
    * @param hub uppaal ta
    * @return a string with the uppaal model (xml)
    */
  def apply(uppaals:Set[Uppaal]):String = {

    // all clocks to declare (everything global for now to simplify naming)
    val clocks:Set[String] = uppaals.flatMap(u=>u.clocks)
    // get all variables to declare
    var vars:Set[Var] = uppaals.flatMap(u=> u.edges.map(e=>e.upd).flatMap(u=>u.vars))
    // ignore for know other variables that were not created for verification purposes
    var vvars:Set[Var] = vars.filter(v=>v.name.startsWith("v"))
    var portvars:Set[Var] = vars.filter(v=>v.name.startsWith("port"))
    // initialize variables (just ports for now)
    var initVars = uppaals.flatMap(u=>u.initVal).filter(i=> i._1.name.startsWith("port"))
    portvars = portvars -- initVars.map(i=>i._1)

    s"""<?xml version="1.0" encoding="utf-8"?>
       |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
       |<nta>
       |<declaration>
       |// Place global declarations here.
       |// clocks:
       |${if (clocks.nonEmpty) clocks.mkString("clock ", ",", ";") else ""}
       |// variables:
       |${mkVariables(vvars++portvars)}
       |
       |${if (initVars.nonEmpty) initVars.map(i => "bool " + Show(Asg(i._1,i._2))).mkString("",";\n",";\n") else ""}
       |// Channels (actions)
       |</declaration>
       |${uppaals.map(mkTemplate).mkString("\n")}
       |<system>
       |// Place template instantiations here.
       |// todo when incorporating variables
       |// List one or more processes to be composed into a system.
       |${uppaals.map(_.name).mkString("system ",",",";")}
       |</system>
       |<queries>
       |</queries>
       |</nta>""".stripMargin

  }


  /**
    * Translates a hub automata into a Uppaal timed automata and returns xml model
    * @param hub hub automata
    * @return a string with the uppaal model (xml)
    */
  def apply(hub:HubAutomata): String = {
    val ta = mkTimeAutomata(hub)
    //println("Updates:\n"+ta.edges.map(e => Show(e.upd)).mkString("\n"))
    apply(ta)
  }

  /**
    * Given a hub automata, creates an Uppaal timed automata with committed states in between each original transition
    *
    * @param hub hub automata
    * @return uppaal timed automata
    */
  def mkTimeAutomata(hubAut:HubAutomata):Uppaal = {

    val hub = Simplify(hubAut)

    var newedges = Set[UppaalEdge]()
    var committed = Set[Int]()
    var act2locs = Map[Int,Set[Int]]()
    var maxloc = hub.sts.max
    var initVal:Set[(Var,Expr)] = Set()

    for ((from,to,acts,cc,cr,g,u)  <- hub.getTransitions) {
      var names = acts.map(hub.getPortName)
      // set all true to all variables named as the actions (to keep track of when an action fire)
      var tacts = names.map(a => Asg(Var("port"+a),Val(1))).foldRight[Update](Noop)(_&_)
      // set false for all variables that do not belong to acts
      var facts = (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var("port"+a),Val(0))).foldRight[Update](Noop)(_&_)
      // first part of the edge, to a new committed state
      newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,tacts & facts)//u)
      // second part of the edge, from the new committed state
      newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,Noop)//executions)
      // accumulate new committed state
      committed += (maxloc+1)
      // add a new map from acts to the new committed state (state where those acts are true)
      act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
      // new max location number
      maxloc +=1
      // initialize port variables to true if this edge goes out of the initial location
//      if(from==hub.init)
//        initVal++= names.map(a=>(Var("port"+a),Val(1)))
    }
    val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

    Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal, "Hub")

  }

  /**
    * Given a formula and a hub automaton, create an uppaal model with the necessary structures to verify such formula
    *
    * @param tf temporal formula
    * @param hub hub automaton from which to create the model
    * @return Uppaal model
    */
  def fromFormula(tf:TemporalFormula,hub:HubAutomata):Set[Uppaal] = tf match {
//    case f@Until(f1,f2) => fromUntil(f,Simplify(hub))
    case f@Eventually(Action(a), Until(f2,Action(b))) => fromEventuallyUntil(f,Simplify(hub))
    case f@Eventually(f1, Until(f2,f3)) =>
        throw new FormulaException("Only an action is supported on the left and right side of an eventually until clause")
    case f@Eventually(Action(name),Before(f2,f3)) => Set(fromEventuallyBefore(f,Simplify(hub)))
    case f@Eventually(f1,Before(f2,f3)) =>
      throw new FormulaException("Only an action is supported on the left side of an eventually before clause")
    case formula => Set(mkTimeAutomata(Simplify(hub)))

  }

  private def fromUntil(f:Until,hub:HubAutomata):Set[Uppaal] =  f match {
    case Until(f1,f2) =>
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

      val (facts,fccons) = collectFirstOf(f2)

      for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
        var names = acts.map(hub.getPortName)
        // set all true to all variables named as the actions (to keep track of when an action fire)
        var trueActs = names.map(a => Asg(Var("port"+a),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts
        var falseActs = (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var("port"+a),Val(0))).foldRight[Update](Noop)(_&_)
        // set to true all variables that capture first_a for a an action in acts
        var firstUpds = names.intersect(facts).map(a => Asg(Var("vfirst"+a),Val(1))).foldRight[Update](Noop)(_&_)
        // first part of the edge, to a new committed state
        newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,firstUpds & trueActs & falseActs)//u)
        // second part of the edge, from the new committed state
        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,Noop)
        // committed states accumulated
        committed += (maxloc+1)
        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        // new max location number
        maxloc +=1
        // initialize port variables to true if this edge goes out of the initial location
//        if(from==hub.init)
//          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")



      // main uppaal automaton for the hub
      val hubAut = Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal.toMap,"Hub")

      //observer automaton for each clock constraint that needs to be track when is first satisfied
      val observers:Set[Uppaal] = fccons.map(mkObserver)

      observers+hubAut
  }

  private def fromEventuallyUntil(f:Eventually,hub:HubAutomata):Set[Uppaal] =  f match {
    case Eventually(Action(a),Until(f2,Action(b))) =>
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

//      val (facts,fccons) = collectFirstOf(f3)

      for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
        var names = acts.map(hub.getPortName)
        // set all true to all variables named as the actions (to keep track of when an action fire)
        var trueActs = names.map(a => Asg(Var("port"+a),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts
        var falseActs = (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var("port"+a),Val(0))).foldRight[Update](Noop)(_&_)
        // set to true all variables that capture first_a for a an action in acts
//        var firstUpds = names.intersect(facts).map(a => Asg(Var("vfirst"+a),Val(1))).foldRight[Update](Noop)(_&_)
        var firstUpds = Asg(Var("vfirst"+b),Val(1))
        // if in this edge action a happens then reset first of
        var resetfirsts = if (names.contains(b)) Asg(Var("vfirst"+b),Val(0)) else Noop
        // first part of the edge, to a new committed state
        newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g, firstUpds & trueActs & falseActs)//u)
        // second part of the edge, from the new committed state
        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,resetfirsts)
        // committed states accumulated
        committed += (maxloc+1)
        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        // new max location number
        maxloc +=1
        // initialize port variables to true if this edge goes out of the initial location
        //        if(from==hub.init)
        //          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

      // main uppaal automaton for the hub
      val hubAut = Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal.toMap,"Hub")

      //observer automaton for each clock constraint that needs to be track when is first satisfied
//      val observers:Set[Uppaal] = fccons.map(mkObserver)

//      observers+hubAut
      Set(hubAut)
  }


  private def fromEventuallyBefore(tf:Eventually,hub:HubAutomata):Uppaal = tf match {
    case Eventually(Action(name),Before(f2,f3))=>
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

      for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
        var names:Set[String] = acts.map(hub.getPortName)
        // set true to all variables named as the actions (to keep track of when an action fire)
        var tacts = names.map(a => Asg(Var("port"+a),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts
        var facts = (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var("port"+a),Val(0))).foldRight[Update](Noop)(_&_)
        // experimenting with setting if an action executed before another action executes
        var executions:Update = Noop
        if (names.contains(name)) { // condition c is satisfied in  c --> c1 before c2, reset everything
          executions = (for(a1<-hub.ports;a2<-hub.ports;if a1!=a2) yield {
            Asg(Var("v"+portToString(a1)+"_"+portToString(a2)),Val(0))
          }).foldRight[Update](Noop)(_&_)
        } else { // condition c is not satisfied in  c --> c1 before c2, updated executions accordingly
          executions = (for( a<-acts;other<-hub.ports; if a!=other) yield {
            Asg(Var("v"+portToString(a)+"_"+portToString(other)),Val(1))
          }).foldRight[Update](Noop)(_&_)
          executions = executions & (for(a<-acts;other<-hub.ports; if a!=other) yield {
            Asg(Var("v"+portToString(other)+"_"+portToString(a)),Val(0))
          }).foldRight[Update](Noop)(_&_)
        }
        // first part of the edge
        newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,tacts & facts)//u)
        // second part of the edge
        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,executions)
        // accumulate committed states
        committed += (maxloc+1)
        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        // new max location
        maxloc +=1
        // initialize port variables to true if this edge goes out of the initial location
        //        if(from==hub.init)
        //          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

      Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal,"Hub")
  }

  /**
    * Create an observer automaton to keep track of when cc becomes true for the first time
    *
    * @param cc clock constraint of the form >= or ==
    * @return and uppaal automaton
    */
  private def mkObserver(cc:ClockCons):Uppaal = {
    // invariant constraint for the initial state of the observer
    val invCC:ClockCons = cc match {
      case GE(c,n) => LE(c,n)
      case ET(c,n) => LE(c,n)
      case _ => throw new FormulaException("Only clock constraints of the form >= or == are allowed in an observer invariant")
    }
    // variable name for the clock constraint and the observer
    val ccname = cc2Name(cc)
    // edge for the observer
    val edge:UppaalEdge = UppaalEdge(0,1,Set(),cc,Set(),Ltrue,Asg(Var(ccname),Val(1)))
    // observer
    Uppaal(Set(0,1),0,cc.clocks.toSet,Set(edge),Map(0->invCC),Set(),Map(),Map(),"Observer"+ccname)
  }

  /**
    * Translated a simple clock constraint into a variable name
    *
    * @param cc clock constraint of the form <=,<,=,>,>=
    * @return cc as a string for a variable name
    */
  private def cc2Name(cc: ClockCons):String = cc match {
    case LE(c,n) => "v"+c+"le"+n
    case LT(c,n) => "v"+c+"lt"+n
    case ET(c,n) => "v"+c+"et"+n
    case GT(c,n) => "v"+c+"gt"+n
    case GE(c,n) => "v"+c+"ge"+n
    case CAnd(c,n) => throw new FormulaException("Only a simple clock constraints can be translated into a variable name")
  }

  /**
    * Collect all actions and clock constraints that will need to be created
    * to track the first time they get satisfied
    *
    * @param f temporal formula
    * @return a pair of set of actions and set of clocks
    */
  private def collectFirstOf(f:StFormula):(Set[String],Set[ClockCons]) = f match {
    case Action(a) => (Set(a),Set())
    case Deadlock => (Set(),Set())
    case TFTrue => (Set(),Set())
    case DGuard(g) => (Set(),Set()) // for now ignore data guards
    case CGuard(g) => (Set(),collectCCons(g))
    case Not(f1) => collectFirstOf(f1)
    case And(f1, f2) =>
      val (a1,cc1) = collectFirstOf(f1)
      val (a2,cc2) = collectFirstOf(f2)
      (a1++a2,cc1++cc2)
    case Or(f1, f2) =>
      val (a1,cc1) = collectFirstOf(f1)
      val (a2,cc2) = collectFirstOf(f2)
      (a1++a2,cc1++cc2)
    case Imply(f1, f2) =>
      val (a1,cc1) = collectFirstOf(f1)
      val (a2,cc2) = collectFirstOf(f2)
      (a1++a2,cc1++cc2)
    case DoingAction(_) => throw new FormulaException("Doing clauses are not allow inside a until formula")
    case Before(_, _) => throw new FormulaException("Before clauses are not allow inside a until formula")
    case Until(_, _)  => throw new FormulaException("Until clauses are not allow inside a until formula")
  }


  /**
    * Collect all clock constraints that need to be tracked
    * to know when they become true.
    *
    * @param cc a clock constraint
    * @return
    */
  private def collectCCons(cc:ClockCons):Set[ClockCons] = cc match {
    case CTrue => Set()
    case GE(c,n) => Set(cc)
    case ET(c,n) => Set(cc)
    case CAnd(cc1,cc2) => collectCCons(cc1) ++ collectCCons(cc2)
    case _ => throw new FormulaException("Only clock constraints of the form >= or ==, and their conjuction are allowed in a until clause")
  }


  /**
    * @param formula temporal formula with syntactic sugar
    * @param act2locs a map from action names to locations where those actions execute
    * @param act2port a map from an action name to the corresponding int port number
    * @return a temporal logic suitable for uppaal
    */
  def toUppaalFormula(formula:TemporalFormula, act2locs:Map[String,Set[Int]], act2port:Map[String,Int],initState:Int=0):UppaalFormula = {
    def tf2UF(tf: TemporalFormula): UppaalFormula = tf match {
      case AA(sf) => UAA(stf2UStF(sf))
      case AE(sf) => UAE(stf2UStF(sf))
      case EA(sf) => UEA(stf2UStF(sf))
      case EE(sf) => UEE(stf2UStF(sf))
      case Eventually(Action(a), Before(f1,f2)) => UEventually(stf2UStF(Action(a)),stf2UStF(Before(f1,f2)))
      case Eventually(Action(a), Until(f1,Action(b))) => UEventually(stf2UStF(Action(a)),UImply(UNot(mkFirstOf(Action(b))),stf2UStF(f1)))
      case Eventually(f1, f2) if f1.hasUntil || f2.hasUntil => throw new RuntimeException("Until clauses inside eventually clause can have the form a --> f until b")
      case Eventually(f1, f2) if f1.hasBefore || f2.hasBefore => throw new RuntimeException("Before clauses inside eventually clause can have the form a --> c before b")
      case Eventually(f1, f2) => UEventually(stf2UStF(f1),stf2UStF(f2))
//      case Until(f1,f2) => UAA(UImply(UNot(mkFirstOf(f2)),stf2UStF(f1)))
    }

    def mkFirstOf(f:StFormula):UppaalStFormula = f match {
      case Deadlock       => UDeadlock
      case Nothing        => UDGuard(act2port.map(a => Pred("==",List(Var("vfirst"+a._1),Val(0)))).foldRight[Guard](Ltrue)(_&&_))
      case TFTrue         => UTrue
      case Action(a)      => UDGuard(Pred("==",List(Var("vfirst"+a),Val(1))))
      case DGuard(g)      => UDGuard(g) //todo: see what this should actually be when adding data guards to uppaal
      case CGuard(c)      =>
        val ccons = collectCCons(c)
        val constraint:Guard = ccons.map(cc=>Pred("==",List(Var(cc2Name(cc)),Val(1)))).foldRight[Guard](Ltrue)(_&&_)
        UDGuard(constraint)
      case Not(f1)        => UNot(mkFirstOf(f1))
      case And(f1,f2)     => UAnd(mkFirstOf(f1),mkFirstOf(f2))
      case Or(f1,f2)      => UOr(mkFirstOf(f1),mkFirstOf(f2))
      case Imply(f1,f2)   => UImply(mkFirstOf(f1),mkFirstOf(f2))
      case DoingAction(a) => throw new FormulaException("Cannot make a firstTime clause of a Doing an action clause.")
      case Until(f1,f2)   => throw new FormulaException("Cannot make a firstTime clause of a Until clause.")
      case Before(f1,f2)  => throw new FormulaException("Cannot make a firstTime clause of a Before clause.")

    }

    def stf2UStF(st: StFormula): UppaalStFormula = st match {
      case Deadlock => UDeadlock
      case Nothing        => UDGuard(act2port.map(a => Pred("==",List(Var("port"+a._1),Val(0)))).foldRight[Guard](Ltrue)(_&&_))
      case TFTrue => UTrue
      case Action(a) =>
//        val locs: Set[UppaalStFormula] = act2locs.getOrElse(a, Set()).map(l => Location("Hub.L" + portToString(l)))
//        if (locs.nonEmpty) locs.foldRight[UppaalStFormula](UNot(UTrue))(_ || _) else throw new RuntimeException("Action name not found: " + a)
//        UDGuard(Pred("==", List(Var("port" + a), Val(1))))
        if (act2port.isDefinedAt(a))
          UCGuard(ET("t"+act2port(a),0))
        else throw new FormulaException("Unknown port name: " + a)
      case DoingAction(a) => UDGuard(Pred("==", List(Var("port" + a), Val(1))))
      case DGuard(g) => UDGuard(g)
      case CGuard(g) => UCGuard(expandCCons(g,act2port))
      case Can(f1) => UOr(Location("Hub.L"+portToString(initState)),stf2UStF(f1)) //mkCan(f1)
      case Not(f1) => UNot(stf2UStF(f1))
      case And(f1, f2) => UAnd(stf2UStF(f1),stf2UStF(f2))
      case Or(f1, f2) => UOr(stf2UStF(f1),stf2UStF(f2))
      case Imply(f1, f2) => UImply(stf2UStF(f1),stf2UStF(f2))
      case Before(Action(a1), Action(a2)) =>
        var a1n = if (act2port.isDefinedAt(a1)) act2port(a1) else throw new FormulaException("Unknown port name: " + a1)
        var a2n = if (act2port.isDefinedAt(a2)) act2port(a2) else throw new FormulaException("Unknown port name: " + a2)
        val res = stf2UStF(And(DGuard(Pred("==", List(Var("v" + a1n + "_" + a2n), Val(1)))), Action(a2)))
        res
      case Before(f1, f2) =>
        throw new FormulaException("Only single actions are supported on an eventually before clause")
    }

//    def mkCan(sf: StFormula):UppaalStFormula = sf match {
//      case Action(a) =>  UOr(Location("Hub.L"+portToString(initState)),UDGuard(Pred("==", List(Var("port" + a), Val(1)))))
//      case And(f1,f2)=> UAnd(mkCan(f1),mkCan(f2))
//      case Or(f1,f2) => UOr(mkCan(f1),mkCan(f2))
//      case Imply(f1,f2) => UImply(mkCan(f1),mkCan(f2))
//      case Not(f1) => UNot(mkCan(f1))
//      case _ => throw new FormulaException("Only simple logic expressions over actions are allowed inside a Can clause - in: "+Show(sf))
//    }

    tf2UF(formula)
  }


  /**
    * Expand clock constraints of the form action.t to the actual clock associanted to the action based on act2port
    * @param cc clock constraint
    * @param act2port a map from an action name to the corresponding int port number (used as clock name: "t"+portname
    * @return a suitable clock constraint for uppaal
    */
  def expandCCons(cc:ClockCons,act2port:Map[String,Int]):ClockCons = cc match {
    case CTrue => cc
    case LE(c,n) => LE(changeClock(c,act2port),n)
    case GE(c,n) => GE(changeClock(c,act2port),n)
    case ET(c,n) => ET(changeClock(c,act2port),n)
    case LT(c,n) => LT(changeClock(c,act2port),n)
    case GT(c,n) => GT(changeClock(c,act2port),n)
    case CAnd(c1,c2) => CAnd(expandCCons(c1,act2port),expandCCons(c2,act2port))
  }

  /**
    * Given a clock name, if it has syntactic sugar (name.t) returns the actual clock name associated to name
    * otherwise returns the same named
    * @param c clock name
    * @param act2port a map from an action name to the corresponding int port number
    * @return the actual name of the clock
    */
  private def changeClock(c:String,act2port:Map[String,Int]):String = {
    if (!c.endsWith(".t")) c
    else if (act2port.isDefinedAt(c.dropRight(2))) "t"+portToString(act2port(c.dropRight(2)))
    else throw new RuntimeException("Action name not found: "+c)
  }

  /**
    * Helper to build locations and clocks associated to ports,
    * if the port is negative it returns the negative as a _ so that Uppaal recognizes it.
    * @param port
    * @return
    */
  private def portToString(port:Int):String = if (port>=0) port.toString() else "_"+Math.abs(port).toString

  private def mkTemplate(uppaal:Uppaal):String = {
    val name = uppaal.name

    s"""<template>
       |<name x="5" y="5">${name}</name>
       |<declaration>
       |// Place local declarations here.""".stripMargin +
//       |// clocks:
//       |${if (uppaal.clocks.nonEmpty) uppaal.clocks.mkString("clock ", ",", ";") else ""}
//       |// variables:
//       |${mkVariables(uppaal)}
       s"""
       |</declaration>
       |
       |// Locations
       |${uppaal.locs.map(loc => mkLocation(loc,uppaal.inv.getOrElse(loc, CTrue),uppaal.committed contains loc)).mkString("\n\n")}
       |<init ref="id${uppaal.init}"/>"
       |
       |// Transitions
       |${uppaal.edges.map(mkTransition).mkString("\n\n")}
       |</template>
     """.stripMargin
  }

  private def mkVariables(uppaal: Uppaal):String = {
    var vs:Set[Var] = uppaal.edges.map(e => e.upd).flatMap(u=>u.vars)
    // for now only auxiliary variables to verify things like a doesn't executes more than once before b
    // ignore other variables for now
    vs = vs.filter(v=>v.name.startsWith("v"))
    //if (vs.nonEmpty) "int[0,2] "+vs.map(Show(_)).mkString("",",",";") else ""
    if (vs.nonEmpty) "bool "+vs.map(Show(_)).mkString("",",",";") else ""
  }

  private def mkVariables(vars: Set[Var]):String = {
    // all booleans for know
    if (vars.nonEmpty) "bool "+vars.map(Show(_)).mkString("",",",";") else ""
  }

  private def mkLocation(loc:Int,inv:ClockCons,committed:Boolean):String = {
    s"""<location id="id$loc" x="${loc*100}" y="0">
       |<name x="${loc*100-10}" y="-34">L${if (loc>=0) loc else "_"+(-1*loc)}</name>
       |${if (inv==CTrue) "" else mkInvariant(loc*100-10,inv)}
       |${if (committed) "<committed/>" else ""}
       |</location>""".stripMargin
  }

  private def mkInvariant(i:Int,cc:ClockCons): String =
    s"""<label kind="invariant" x="$i" y="11">${mkCC(cc)}</label>"""

  private def mkCC(cc:ClockCons): String = cc match {
    case CTrue => "true"
    case ET(c, n) => c+"=="+n
    case LT(c, n) => c+"&lt;"+n
    case GT(c, n) => c+"&gt;"+n
    case LE(c, n) => c+"&lt;="+n
    case GE(c, n) => c+"&gt;="+n
    case CAnd(cc1, cc2) => mkCC(cc1)+" &amp;&amp; "+ mkCC(cc2)
  }

  private def mkTransition(e:UppaalEdge):String = {
    s"""<transition>
       |<source ref="id${e.from}"/>
       |<target ref="id${e.to}"/>
       |${if (e.ccons==CTrue /*&& e.guard==LTrue*/) "" else mkGuard(e.from,e.ccons,e.guard)}""".stripMargin +
       //|${mkActLabel(e)}
       s"""|${mkEdgeUpd(e)}
       |</transition>""".stripMargin
  }

  private def mkEdgeUpd(e:UppaalEdge):String = {
    val supd = Simplify(e.upd)
    if (e.creset.isEmpty) mkUpdates(e.from,mkUpd(supd))
    else if (supd == Noop) mkUpdates(e.from,e.creset.map(_+":=0").mkString(", "))
    else mkUpdates(e.from,e.creset.map(_+":=0").mkString(", ")+" , "+mkUpd(supd))
  }

  private def mkUpd(u:Update):String = Show(u)

  private def mkGuard(from:Int,cc:ClockCons,g:Guard): String =
    s"""<label kind="guard" x="${from*100+25}" y="-34">${mkCCG(cc,g)}</label>"""

  private def mkCCG(cc:ClockCons,g:Guard):String = //if (cc==CTrue) "" else mkCC(cc)
    if      (cc==CTrue) Show.showUppaalGuard(g)
    else if (g==Ltrue) mkCC(cc)
    else mkCC(cc)+" &amp;&amp; "+Show.showUppaalGuard(g)


  private def mkUpdates(from:Int,upds:String): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${upds}</label>"""

}
