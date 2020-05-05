package hub.backend

import hub._
import hub.analyse._
import hub.common.FormulaException

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
case class Uppaal(locs:Set[Int],init:Int,clocks:Set[String],edges:Set[UppaalEdge],inv:Map[Int,CCons],
                  committed:Set[Int],act2locs:Map[Int,Set[Int]],initVal:Map[Var,Expr],name:String) {}

case class UppaalEdge(from:Int,to:Int,ports:Set[String],ccons:CCons,creset:Set[String],guard:Guard,upd:Update) {}

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

  def saveWithQueries(uppaal: Uppaal,queries:List[UppaalFormula]):String =
    apply(Set(uppaal),queries)

  def saveWithQueries(uppaals: Set[Uppaal],queries:List[UppaalFormula]):String =
    apply(uppaals,queries)

  /**
    * Given an Uppaal ta returns xml model
    * @param hub uppaal ta
    * @return a string with the uppaal model (xml)
    */
  def apply(uppaals:Set[Uppaal],queries:List[UppaalFormula]=List()):String = {
    // all clocks to declare (everything global for now to simplify naming)
    val clocks:Set[String] = uppaals.flatMap(u=>u.clocks)
    // get all variables to declare
    var vars:Set[Var] = uppaals.flatMap(u=> u.edges.map(e=>e.upd).flatMap(u=>u.vars))
    // ignore for know other variables that were not created for verification purposes
    var vvars:Set[Var] = vars.filter(v=>v.name.startsWith("done"))
    var portvars:Set[Var] = vars.filter(v=>v.name.startsWith("P"))
    var intvars:Set[Var] = vars.filter(v=>v.name.startsWith("since"))
    // initialize variables (just ports for now)
    var initVars = uppaals.flatMap(u=>u.initVal).filter(i=> i._1.name.startsWith("P"))
    portvars = portvars -- initVars.map(i=>i._1)
    val channels:Set[String] = uppaals.flatMap(u=> u.edges).flatMap(e=>e.ports).filter(p=> p.endsWith("!") || p.endsWith("?"))
    val simpleCh:Set[String] = channels.filterNot(_.startsWith("priority"))
    var priorityCh:List[String] = channels.filter(_.startsWith("priority")).map(_.dropRight(1)).toList
    priorityCh = priorityCh.sortBy(p=> p.drop(8).toInt).reverse
    s"""<?xml version="1.0" encoding="utf-8"?>
       |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
       |<nta>
       |<declaration>
       |// Place global declarations here.
       |// clocks:
       |${if (clocks.nonEmpty) clocks.mkString("clock ", ",", ";") else ""}
       |// variables:
       |${mkVariables(vvars++portvars++intvars)}
       |
       |${if (initVars.nonEmpty) initVars.map(i => "bool " + Show(Asg(i._1,i._2))).mkString("",";\n",";\n") else ""}
       |// Channels (actions)
       |${if (simpleCh.nonEmpty) simpleCh.map(_.dropRight(1)).mkString("chan ",",",";") else ""}
       |// Broadcast Channels (actions)
       |${if (priorityCh.nonEmpty) priorityCh.mkString("broadcast chan ", ",", ";") else ""}
       |// Channels priority
       |${if (priorityCh.size>1) s"chan priority ${priorityCh.mkString(""," &lt; "," ;")}" else ""}
       |</declaration>
       |${uppaals.map(mkTemplate).mkString("\n")}
       |<system>
       |// Place template instantiations here.
       |// todo when incorporating variables
       |// List one or more processes to be composed into a system.
       |${uppaals.map(_.name).mkString("system ",",",";")}
       |</system>
       |<queries>
       |${if (queries.nonEmpty) mkQueries(queries) else ""}
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
    * Given a hub automata, creates an Uppaal timed automata (without committed)
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


    for ((from,to,prio,acts,cc,cr,g,u)  <- hub.getTransitions) {
      var names = acts.map(hub.getPortName)
      // set all true to all variables named as the actions (to keep track of when an action fire)
      var tacts = names.map(a => Asg(Var(port(a)),Val(1))).foldRight[Update](Noop)(_&_)
      // set false for all variables that do not belong to acts unless tau transition
      var facts = if (acts.isEmpty) Noop else (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var(port(a)),Val(0))).foldRight[Update](Noop)(_&_)
      // set to true all variables that capture first_a for a an action in acts
      //var firstUpds = names.map(a => Asg(Var(done(a)),Val(1))).foldRight[Update](Noop)(_&_)
//      // check if a fired action belongs to a task action and add a dummy broadcast to force priority
//      val brcast:Set[String] = if ((hub.taskPort._1++hub.taskPort._2).intersect(acts).nonEmpty) Set(broadcast()) else Set()
      // add a dummy broadcast chan based on the priority of the edge
      // first part of the edge, to a new committed state
      newedges += UppaalEdge(from,to,names.map(a=>chan(a))+priority(prio),cc,cr++names.map(a => clock(a)),g,tacts & facts)//u)
      // second part of the edge, from the new committed state
//      newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,names.map(a => clock(a)),Ltrue,firstUpds)//executions)
      // accumulate new committed state
//      committed += (maxloc+1)
      // add a new map from acts to the new committed state (state where those acts are true)
//      act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
      // new max location number
//      maxloc +=1
      // initialize port variables to true if this edge goes out of the initial location
//      if(from==hub.init)
//        initVal++= names.map(a=>(Var("port"+a),Val(1)))
    }
    val actclocks = hub.ports.map(p => clock(hub.getPortName(p)))

    Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal, "Hub")
  }

  private def mkWithCommitted(hub:HubAutomata):Uppaal = {
    var newedges = Set[UppaalEdge]()
    var committed = Set[Int]()
    var act2locs = Map[Int,Set[Int]]()
    var maxloc = hub.sts.max
    var initVal:Set[(Var,Expr)] = Set()

    for ((from,to,prio,acts,cc,cr,g,u)  <- hub.getTransitions) {
      var names = acts.map(hub.getPortName)
      // set all true to all variables named as the actions (to keep track of when an action fire)
      var tacts = names.map(a => Asg(Var(port(a)),Val(1))).foldRight[Update](Noop)(_&_)
      // set false for all variables that do not belong to acts unless tau transition
      var facts = if (acts.isEmpty) Noop else (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var(port(a)),Val(0))).foldRight[Update](Noop)(_&_)
      // set to true all variables that capture first_a for a an action in acts
      var firstUpds = names.map(a => Asg(Var(done(a)),Val(1))).foldRight[Update](Noop)(_&_)
      // add a dummy broadcast chan based on the priority of the edge
      // first part of the edge, to a new committed state
      newedges += UppaalEdge(from,maxloc+1,names.map(a=>chan(a))+priority(prio),cc,cr,g, tacts & facts)//u)
      // second part of the edge, from the new committed state
      newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,names.map(a => clock(a)),Ltrue,firstUpds)//executions)
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
    val actclocks = hub.ports.map(p => clock(hub.getPortName(p)))

    Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal, "Hub")
  }

  /**
    * Given a formula and a hub automaton, create an uppaal model with the necessary structures to verify such formula
    *
    * @param tf temporal formula
    * @param hub hub automaton from which to create the model
    * @return Uppaal model
    */
  def fromFormula(tf:TemporalFormula,hub:HubAutomata):Set[Uppaal] = {
    if (validFormula(tf,hub))
      tf match {
    //    case f@Until(f1,f2) => fromUntil(f,Simplify(hub))
      case f if f.hasWaits || f.hasDone => Set(mkWithCommitted(Simplify(hub)))
      case Every(a,b) => Set(fromEvery(a,b,Simplify(hub)))//fromEventuallyUntil(Eventually(a,Until(Not(a),b)),Simplify(hub))
      case f@EveryAfter(a,b,t) => Set(fromEvery(a,b,Simplify(hub))) //fromEventuallyUntil(Eventually(a,Until(Not(a),b)),Simplify(hub)) //fromEveryAfter(f,Simplify(hub))
      case f@Eventually(Action(a), Until(f2,Action(b))) => fromEventuallyUntil(f,Simplify(hub))
      case f@Eventually(f1, Until(f2,f3)) =>
          throw new FormulaException("Only an action is supported on the left and right side of an eventually until clause")
//      case f@Eventually(Action(name),Before(f2,f3)) => Set(fromEventuallyBefore(f,Simplify(hub)))
      case f@Eventually(f1,Before(f2,f3)) =>
        throw new FormulaException("Only an action is supported on the left side of an eventually before clause")
      case formula => Set(mkTimeAutomata(Simplify(hub)))
    }
    else throw new FormulaException("Unkown action names in formula "+ Show(tf))
  }

  private def validFormula(tf:TemporalFormula,hub:HubAutomata):Boolean = {
    val hubPorts = hub.ports.map(hub.getPortName)
    tf.actions.forall(a => hubPorts.contains(a))
  }

  // make queries template to save queries in xml file
  private def mkQueries(queries:List[UppaalFormula]):String = {
    if (queries.isEmpty) "" else queries.map(mkQuery).mkString("","\n","")
  }
  // make query template to save query in xml file todo: do proper <= >= for xml.
  private def mkQuery(query:UppaalFormula):String = {
    s"""<query>
       |<formula>${mkUppaalFormula(query)}</formula>
       |<comment></comment>
       |</query>
     """.stripMargin
  }

  private def mkUppaalFormula(f:UppaalFormula): String = f match {
    case UAA(sf) => "A[] " + mkUppaalStFormula(sf)
    case UAE(sf) => "A&lt;&gt; " + mkUppaalStFormula(sf)
    case UEA(sf) => "E[] " + mkUppaalStFormula(sf)
    case UEE(sf) => "E&lt;&gt; " + mkUppaalStFormula(sf)
    case UEventually(f1,f2) => mkUppaalStFormula(f1) + " --&gt; " + mkUppaalStFormula(f2)
  }

  private def mkUppaalStFormula(uf:UppaalStFormula):String = uf match {
    case UDeadlock => "deadlock"
    case UTrue => "true"
    case Location(l) => l
    case UDGuard(g) => mkUFormulaGuard(g)
    case UCGuard(cc) => mkCC(cc)
    case UNot(f1) => "not("+ mkUppaalStFormula(f1) + ")"
    case UAnd(f1,f2) => parShow(f1) + " and " + parShow(f2)
    case UOr(f1,f2) =>  parShow(f1) + " or " + parShow(f2)
    case UImply(f1,f2) => parShow(f1) + " imply " + parShow(f2)
  }

  private def parShow(f: UppaalStFormula): String = f match {
    case UDeadlock | UTrue | UNot(_) => mkUppaalStFormula(f)
    case _ => "("+mkUppaalStFormula(f)+")"
  }

  private def mkUFormulaGuard(g:Guard):String = Simplify(g) match {
    case Ltrue => "true"
    case LNot(Ltrue) => "false"
    case LOr(g1, g2) => mkUFormulaGuard(g1) + " or " + mkUFormulaGuard(g2)
    case LAnd(g1, g2) => mkUFormulaGuard(g1) + "  and " + mkUFormulaGuard(g2)
    case LNot(g) => s"!(${mkUFormulaGuard(g)})"
    case Pred(name, a1::a2::Nil) if  Set("<=","<","==",">",">=","+","-").contains(name) =>
      "("+Show(a1)+ " " + mkUFormulaGuardFun(name) + " " + Show(a2)+")"
    case Pred(name,param) =>
      s"$name(${param.map(p => p match {
        case Fun(n,p) => mkUFormulaGuardFun(n)
        case _ => Show(p)
          // Guillermina: missing the other cases?
      }).mkString(",")})"
  }

  private def mkUFormulaGuardFun(name: String): String = name match {
    case "<=" => " &lt;= "
    case "<"  => " &lt;"
    case ">=" => " &gt;= "
    case ">"  => " &gt; "
    case _ => " " + name + " "
  }

  private def mkUFormulaExpr(e:Expr):String = e match {
    case Fun(n,a1::a2::Nil) if  Set("<=","<","==",">",">=","+","-").contains(n) => // cannot call other kind of functions
      "("+mkUFormulaExpr(a1)+ " " + mkUFormulaGuardFun(n) + " " + mkUFormulaExpr(a2)+")"
    case Fun(n,args) => throw new FormulaException(s"Functions not allow in Uppaal TCTL formula, but ${Show(e)} found")
    case _ => Show(e)
  }

  private def fromEvery(pre:Action,post:Action,hub:HubAutomata):Uppaal = {
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

      for ((from,to,prio,acts,cc,cr,g,u)  <-hubedges) {
        var names:Set[String] = acts.map(hub.getPortName)
        // set true to all variables named as the actions (to keep track of when an action fire)
        var tacts = names.map(a => Asg(Var(port(a)),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts unless tau transiton
        var facts = if (acts.isEmpty) Noop else (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var(port(a)),Val(0))).foldRight[Update](Noop)(_&_)
        // experimenting with setting if an action executed before another action executes
        var executions,executions1,executions2:Update = Noop
        //val prePort = hub.ports.find(a=> hub.getPortName(a)== pre.name).get
        //val postPort = hub.ports.find(a=> hub.getPortName(a)== post.name).get
        (names.contains(pre.name),names.contains(post.name)) match {
          case (true,false) =>
            executions = increaseSince(pre.name,post.name)
          case (false,true) =>
            executions = decreaseSince(pre.name,post.name)
          case (true,true) =>
            executions1 = increaseSince(pre.name,post.name)
            executions2 = decreaseSince(pre.name,post.name)
          case _ => ()
        }
        // set to true all variables that capture first_a for a an action in acts
        var firstUpds = names.map(a => Asg(Var(done(a)),Val(1))).foldRight[Update](Noop)(_&_)
        // add a dummy broadcast chan based on the priority of the edge
        // first part of the edge
        newedges += UppaalEdge(from,maxloc+1,names.map(a=>chan(a))+priority(prio),cc,cr,g,tacts & facts & executions1)//u)
//        newedges += UppaalEdge(from,to,acts.map(a=>"ch"+portToString(a)),cc,cr++acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),g,tacts & facts & executions)//u)
//        // second part of the edge
        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,names.map(a => clock(a)),Ltrue,firstUpds & executions & executions2)
//        // accumulate committed states
        committed += (maxloc+1)
//        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
//        // new max location
        maxloc +=1
//        // initialize port variables to true if this edge goes out of the initial location
//        //        if(from==hub.init)
//        //          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => clock(hub.getPortName(p)))

      Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal,"Hub")
  }

  private def increaseSince(a:String,b:String):Update =
    Asg(Var(since(a,b)), Fun("&lt;?",List(Fun("+",List(Var(since(a,b)),Val(1))),Val(2))))

  private def decreaseSince(a:String,b:String):Update =
     Asg(Var(since(a,b)),Fun("&gt;?",List(Fun("-",List(Var(since(a,b)),Val(1))),Val(0))))

  private def done(a:String):String = "done"+a

  private def port(a:String):String = "P"+a

  private def since(a:String,b:String):String = "since"+a+"_"+b

  private def chan(a:String):String = "ch"+a

  private def broadcast():String = "broadcastSync!"

  private def clock(a:String):String = "t"+a

  private def priority(i:Int):String = "priority"+Math.abs(i)+"!"

  //  private def mkSinceName(a:Int,b:Int):String =
  //    "int"+portToString(a)+"_"+portToString(b)

  //private def mkDoneName(a:Int):String = "vfirst"+portToString(a) //todo: change to done

  // todo: addapt or drop
  private def fromUntil(f:Until,hub:HubAutomata):Set[Uppaal] =  f match {
    case Until(f1,f2) =>
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

      val (facts,fccons) = collectFirstOf(f2)

      for ((from,to,prio,acts,cc,cr,g,u)  <-hubedges) {
        var names = acts.map(hub.getPortName)
        // set all true to all variables named as the actions (to keep track of when an action fire)
        var trueActs = names.map(a => Asg(Var(port(a)),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts unless tau transition
        var falseActs = if (acts.isEmpty) Noop else (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var(port(a)),Val(0))).foldRight[Update](Noop)(_&_)
        // set to true all variables that capture first_a for a an action in acts
        var firstUpds = names.intersect(facts).map(a => Asg(Var(done(a)),Val(1))).foldRight[Update](Noop)(_&_)
        // first part of the edge, to a new committed state
        newedges += UppaalEdge(from,to,names.map(a=>chan(a)),cc,cr++names.map(a => clock(a)),g,firstUpds & trueActs & falseActs)//u)
        // second part of the edge, from the new committed state
//        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,Noop)
        // committed states accumulated
//        committed += (maxloc+1)
        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
//        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        // new max location number
//        maxloc +=1
        // initialize port variables to true if this edge goes out of the initial location
//        if(from==hub.init)
//          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => clock(hub.getPortName(p)))

      //observer automaton for each clock constraint that needs to be track when is first satisfied
      val observers:Set[Uppaal] = fccons.map(cc=> mkObserver(cc))

      // main uppaal automaton for the hub
      val hubAut = Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal.toMap,"Hub")

      observers+hubAut
  }

  // todo: addapt or drop
  private def fromEventuallyUntil(f:Eventually,hub:HubAutomata):Set[Uppaal] =  f match {
    case Eventually(Action(a),Until(f2,Action(b))) =>
      val hubedges = hub.getTransitions

      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
//      var maxloc = hub.sts.max
      var initVal:Set[(Var,Expr)] = Set()

//      val (facts,fccons) = collectFirstOf(f3)

      for ((from,to,prio,acts,cc,cr,g,u)  <-hubedges) {
        var names = acts.map(hub.getPortName)
        // set all true to all variables named as the actions (to keep track of when an action fire)
        var trueActs = names.map(a => Asg(Var(port(a)),Val(1))).foldRight[Update](Noop)(_&_)
        // set false for all variables that do not belong to acts unless tau transition
        var falseActs = if (acts.isEmpty) Noop else (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var(port(a)),Val(0))).foldRight[Update](Noop)(_&_)
        // set to true all variables that capture first_a for a an action in acts
//        var firstUpds = names.intersect(facts).map(a => Asg(Var("vfirst"+a),Val(1))).foldRight[Update](Noop)(_&_)
        var firstUpds = if (names.contains(b)) Asg(Var(done(b)),Val(1)) else Noop
//        println("firstUps: "+ firstUpds)
        // if in this edge action a happens then reset first of
        var resetfirsts = if (names.contains(a)) Asg(Var(done(b)),Val(0)) else Noop
//        println("resetFirst: "+ resetfirsts)
        // first part of the edge, to a new committed state
        newedges += UppaalEdge(from,to,names.map(a=>chan(a)),cc,cr++names.map(a => chan(a)),g, resetfirsts & trueActs & firstUpds & falseActs)//u)
        // second part of the edge, from the new committed state
//        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,resetfirsts)
        // committed states accumulated
//        committed += (maxloc+1)
        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
//        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        // new max location number
//        maxloc +=1
        // initialize port variables to true if this edge goes out of the initial location
        //        if(from==hub.init)
        //          initVal++= names.map(a=>(Var("port"+a),Val(1)))
      }
      val actclocks = hub.ports.map(p => clock(hub.getPortName(p)))

      // main uppaal automaton for the hub
      val hubAut = Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal.toMap,"Hub")

      //observer automaton for each clock constraint that needs to be track when is first satisfied
//      val observers:Set[Uppaal] = fccons.map(mkObserver)

//      observers+hubAut
      Set(hubAut)
    case _ => throw new FormulaException(s"${Show(f)} is not a valid formula")
  }


//  private def fromEventuallyBefore(tf:Eventually,hub:HubAutomata):Uppaal = tf match {
//    case Eventually(Action(name),Before(f2,f3))=>
//      val hubedges = hub.getTransitions
//
//      var newedges = Set[UppaalEdge]()
//      var committed = Set[Int]()
//      var act2locs = Map[Int,Set[Int]]()
//      var maxloc = hub.sts.max
//      var initVal:Set[(Var,Expr)] = Set()
//
//      for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
//        var names:Set[String] = acts.map(hub.getPortName)
//        // set true to all variables named as the actions (to keep track of when an action fire)
//        var tacts = names.map(a => Asg(Var("port"+a),Val(1))).foldRight[Update](Noop)(_&_)
//        // set false for all variables that do not belong to acts unless tau transition
//        var facts = if (acts.isEmpty) Noop else  (hub.ports.map(hub.getPortName) -- names).map(a => Asg(Var("port"+a),Val(0))).foldRight[Update](Noop)(_&_)
//        // experimenting with setting if an action executed before another action executes
//        var executions:Update = Noop
//        if (names.contains(name)) { // condition c is satisfied in  c --> c1 before c2, reset everything
//          executions = (for(a1<-hub.ports;a2<-hub.ports;if a1!=a2) yield {
//            Asg(Var("v"+portToString(a1)+"_"+portToString(a2)),Val(0))
//          }).foldRight[Update](Noop)(_&_)
//        } else { // condition c is not satisfied in  c --> c1 before c2, updated executions accordingly
//          executions = (for( a<-acts;other<-hub.ports; if a!=other) yield {
//            Asg(Var("v"+portToString(a)+"_"+portToString(other)),Val(1))
//          }).foldRight[Update](Noop)(_&_)
//          executions = executions & (for(a<-acts;other<-hub.ports; if a!=other) yield {
//            Asg(Var("v"+portToString(other)+"_"+portToString(a)),Val(0))
//          }).foldRight[Update](Noop)(_&_)
//        }
//        // set to true all variables that capture first_a for a an action in acts
//        var firstUpds = names.map(a => Asg(Var("vfirst"+a),Val(1))).foldRight[Update](Noop)(_&_)
//        // first part of the edge
//        newedges += UppaalEdge(from,maxloc+1,acts.map(a=>"ch"+portToString(a)),cc,cr,g,firstUpds & tacts & facts)//u)
//        // second part of the edge
//        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,executions)
//        // accumulate committed states
//        committed += (maxloc+1)
//        // keep track of actions to locations where those actions just executed (i.e. new committed state created)
//        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
//        // new max location
//        maxloc +=1
//        // initialize port variables to true if this edge goes out of the initial location
//        //        if(from==hub.init)
//        //          initVal++= names.map(a=>(Var("port"+a),Val(1)))
//      }
//      val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")
//
//      Uppaal(hub.sts++committed,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs,hub.initVal++initVal,"Hub")
//  }

  /**
    * Create an observer automaton to keep track of when cc becomes true for the first time
    *
    * @param cc clock constraint of the form >= or ==
    * @return and uppaal automaton
    */
  //todo: mkObserver needs to be revised now that clock constraints support other clocks.
  private def mkObserver(cc:CCons,withReset:Boolean=false):Uppaal = {
    // invariant constraint for the initial state of the observer
    val invCC:CCons = cc match {
      case GE(c,CInt(n)) => LE(c,CInt(n))
      case ET(c,CInt(n)) => LE(c,CInt(n))
      case _ => throw new FormulaException("Only clock constraints of the form >= or == are allowed in an observer invariant")
    }
    // variable name for the clock constraint and the observer
    val ccname = cc2Name(cc)
    // edges for the observer
    var edges:Set[UppaalEdge] = Set(UppaalEdge(0,1,Set(),cc,Set(),Ltrue,Asg(Var(ccname),Val(1))))

    if (withReset) {
      edges += UppaalEdge(0,0,Set("reset"+ccname+"?"),CTrue,Set(),Ltrue,Noop)
      edges += UppaalEdge(1,0,Set("reset"+ccname+"?"),CTrue,Set(),Ltrue,Asg(Var(ccname),Val(0)))
    }

    // observer
    Uppaal(Set(0,1),0,cc.clocks.toSet,edges,Map(0->invCC),Set(),Map(),Map(),"Observer"+ccname)
  }

  /**
    * Translated a simple clock constraint into a variable name
    *
    * @param cc clock constraint of the form <=,<,=,>,>=
    * @return cc as a string for a variable name
    */
  private def cc2Name(cc: CCons):String = cc match {
    case LE(c,n) => "v"+c+"le"+cexpr2Name(n)
    case LT(c,n) => "v"+c+"lt"+cexpr2Name(n)
    case ET(c,n) => "v"+c+"et"+cexpr2Name(n)
    case GT(c,n) => "v"+c+"gt"+cexpr2Name(n)
    case GE(c,n) => "v"+c+"ge"+cexpr2Name(n)
    case CAnd(c,n) => throw new FormulaException("Only a simple clock constraints can be translated into a variable name")
  }

  private def cexpr2Name(ce:ClockExpr):String = ce match {
    case CMinus(c,i) => s"${c}m${i}"
    case CPlus(c,i) => s"${c}p${i}"
    case _ => Show(ce)
  }

  /**
    * Collect all actions and clock constraints that will need to be created
    * to track the first time they get satisfied
    *
    * @param f temporal formula
    * @return a pair of set of actions and set of clocks
    */
  private def collectFirstOf(f:StFormula):(Set[String],Set[CCons]) = f match {
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
//    case DoingAction(_) => throw new FormulaException("Doing clauses are not allow inside a until formula")
//    case Before(_, _) => throw new FormulaException("Before clauses are not allow inside a until formula")
//    case Until(_, _)  => throw new FormulaException("Until clauses are not allow inside a until formula")
    case _ => throw new FormulaException(s"${Show(f)} is not allowed inside an until formula")
  }


  /**
    * Collect all clock constraints that need to be tracked
    * to know when they become true.
    *
    * @param cc a clock constraint
    * @return
    */
  private def collectCCons(cc:CCons):Set[CCons] = cc match {
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
  def toUppaalFormula(formula:TemporalFormula, act2locs:Map[String,Set[Int]], act2port:Map[String,Int],initState:Int=0):List[UppaalFormula] = {
    def tf2UF(tf: TemporalFormula): List[UppaalFormula] = tf match {
      case AA(sf) => List(UAA(stf2UStF(sf)))
      case AE(sf) => List(UAE(stf2UStF(sf)))
      case EA(sf) => List(UEA(stf2UStF(sf)))
      case EE(sf) => List(UEE(stf2UStF(sf)))
      case Every(a,b)=>
        val locsOfB: Set[UppaalStFormula] = act2locs.getOrElse(b.name, Set()).map(l => Location("Hub.L" + portToString(l)))
        var part1:UppaalStFormula = UTrue
        if (locsOfB.nonEmpty) {
          part1 = locsOfB.foldRight[UppaalStFormula](UNot(UTrue))(_ || _)
          part1 = UImply(part1,UDGuard(Pred("<=",List(Var(since(a.name,b.name)),Val(1)))))
        } else throw new FormulaException("Action name not found: " + b)
        List(UAA(part1),UEventually(stf2UStF(a),stf2UStF(b))) //UEventually(stf2UStF(DoingAction(a.name)),stf2UStF(DoingAction(b.name))))
      case EveryAfter(a,b,t) =>
        val locsOfB: Set[UppaalStFormula] = act2locs.getOrElse(b.name, Set()).map(l => Location("Hub.L" + portToString(l)))
        var part1:UppaalStFormula = UTrue
        var part2:UppaalStFormula = UTrue
        if (locsOfB.nonEmpty) {
          val locs = locsOfB.foldRight[UppaalStFormula](UNot(UTrue))(_ || _)
          //part1 = UImply(locs,UAnd(UDGuard(Pred("<=",List(Var(since(a.name,b.name)),Val(1)))),UCGuard(GE(clock(a.name),CInt(t)))))
          part1 = UImply(locs
                        ,UDGuard(Pred("<=",List(Var(since(a.name,b.name)),Val(1)))))
          part2 = UImply(UAnd(locs,UDGuard(Pred("==",List(Var(since(a.name,b.name)),Val(1)))))
                        ,UCGuard(GE(clock(a.name),CInt(t))))  
        } else throw new FormulaException("Action name not found: " + b)
        val part3 = UEventually(stf2UStF(a),stf2UStF(b))
        List(UAA(part1),UAA(part2),part3) //UEventually(stf2UStF(DoingAction(a.name)),stf2UStF(DoingAction(b.name))))
      case Eventually(Action(a), Before(f1,f2)) => List(UEventually(stf2UStF(Action(a)),stf2UStF(Before(f1,f2))))
      case Eventually(Action(a), Until(f1,Action(b))) => List(UEventually(stf2UStF(Action(a)),UAnd(UNot(mkFirstOf(Action(b))),stf2UStF(f1))))
      case Eventually(f1, f2) if f1.hasUntil || f2.hasUntil => throw new FormulaException("Until clauses inside eventually clause can have the form a --> f until b")
      case Eventually(f1, f2) if f1.hasBefore || f2.hasBefore => throw new FormulaException("Before clauses inside eventually clause can have the form a --> c before b")
      case Eventually(f1, f2) => List(UEventually(stf2UStF(f1),stf2UStF(f2)))
//      case Until(f1,f2) => UAA(UImply(UNot(mkFirstOf(f2)),stf2UStF(f1)))
    }

    def mkFirstOf(f:StFormula):UppaalStFormula = f match {
      case Deadlock       => UDeadlock
      case Nothing        => UDGuard(act2port.map(a => Pred("==",List(Var(done(a._1)),Val(0)))).foldRight[Guard](Ltrue)(_&&_))
      case TFTrue         => UTrue
      case Action(a)      => UDGuard(Pred("==",List(Var(done(a)),Val(1))))
      case DGuard(g)      => UDGuard(g) //todo: see what this should actually be when adding data guards to uppaal
      case CGuard(c)      =>
        val ccons = collectCCons(c)
        val constraint:Guard = ccons.map(cc=>Pred("==",List(Var(cc2Name(cc)),Val(1)))).foldRight[Guard](Ltrue)(_&&_)
        UDGuard(constraint)
      case Not(f1)        => UNot(mkFirstOf(f1))
      case And(f1,f2)     => UAnd(mkFirstOf(f1),mkFirstOf(f2))
      case Or(f1,f2)      => UOr(mkFirstOf(f1),mkFirstOf(f2))
      case Imply(f1,f2)   => UImply(mkFirstOf(f1),mkFirstOf(f2))
//      case DoingAction(_) | DoneAction(_) => throw new FormulaException("Cannot make a firstTime clause of a Doing/Done an action clause.")
//      case Until(f1,f2)   => throw new FormulaException("Cannot make a firstTime clause of a Until clause.")
//      case Before(f1,f2)  => throw new FormulaException("Cannot make a firstTime clause of a Before clause.")
      case _ => throw new FormulaException(s"Cannot make a firstTime clause for ${Show(f)}")
    }

    def stf2UStF(st: StFormula): UppaalStFormula = st match {
      case Deadlock => UDeadlock
      case Nothing        => UDGuard(act2port.map(a => Pred("==",List(Var(port(a._1)),Val(0)))).foldRight[Guard](Ltrue)(_&&_))
      case TFTrue => UTrue
      case Action(a) =>
        if (act2port.isDefinedAt(a))
          UAnd(stf2UStF(DoingAction(a)),UCGuard(ET(clock(a),CInt(0))))
        else throw new FormulaException("Unknown port name: " + a)
      case DoingAction(a) => UDGuard(Pred("==", List(Var(port(a)), Val(1))))
      case DoneAction(a) => UDGuard(Pred("==", List(Var(done(a)), Val(1))))
      case DGuard(g) => UDGuard(g)
      case CGuard(g) => UCGuard(expandCCons(g,act2port))
      //case Can(f1) => UOr(Location("Hub.L"+portToString(initState)),stf2UStF(f1)) //mkCan(f1)
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
      case Refires(a,mode,t) if mode == RBeforeOrAt || mode == RBefore =>
        //UImply(mkFirstOf(a),mode2UStF(a,mode,t))
        mode2UStF(a,mode,t)
      case Refires(a,mode,t) => // atLeast or moreThan
        val locsOfA: Set[UppaalStFormula] = act2locs.getOrElse(a.name, Set()).map(l => Location("Hub.L" + portToString(l)))
        var res:UppaalStFormula = UTrue
        if (locsOfA.nonEmpty) {
          res = locsOfA.foldRight[UppaalStFormula](UNot(UTrue))(_ || _)
          res = UImply(mkFirstOf(a),UImply(res,mode2UStF(a,mode,t)))
        } else throw new FormulaException("Action name not found: " + a)
        res
    }

    def mode2UStF(a: Action, mode: RefireMode, t: Int):UppaalStFormula = mode match {
      case RAfterOrAt  => UCGuard(GE(clock(a.name),CInt(t)))
      case RBeforeOrAt   => UCGuard(LE(clock(a.name),CInt(t)))
      case RAfter => UCGuard(GT(clock(a.name),CInt(t)))
      case RBefore => UCGuard(LT(clock(a.name),CInt(t)))
    }

    if (act2port.map(_._1).toSet.intersect(formula.actions) == formula.actions )
      tf2UF(formula).map(Simplify(_))
    else throw new FormulaException("Unkown action names in formula "+ Show(formula))
  }


  /**
    * Expand clock constraints of the form action.t to the actual clock associanted to the action based on act2port
    * @param cc clock constraint
    * @param act2port a map from an action name to the corresponding int port number (used as clock name: "t"+portname
    * @return a suitable clock constraint for uppaal
    */
  def expandCCons(cc:CCons,act2port:Map[String,Int]):CCons = cc match {
    case CTrue => cc
    case LE(c,n) => LE(changeClock(c,act2port),expandCCExpr(n,act2port))
    case GE(c,n) => GE(changeClock(c,act2port),expandCCExpr(n,act2port))
    case ET(c,n) => ET(changeClock(c,act2port),expandCCExpr(n,act2port))
    case LT(c,n) => LT(changeClock(c,act2port),expandCCExpr(n,act2port))
    case GT(c,n) => GT(changeClock(c,act2port),expandCCExpr(n,act2port))
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
    else if (act2port.isDefinedAt(c.dropRight(2))) clock(c.dropRight(2)) //"t"+portToString(act2port(c.dropRight(2)))
    else throw new RuntimeException("Action name not found: "+c)
  }

  private def expandCCExpr(ce:ClockExpr,act2port:Map[String,Int]):ClockExpr = ce match {
    case Clock(c) => Clock(changeClock(c,act2port))
    case CPlus(c,i) => CPlus(changeClock(c,act2port),i)
    case CMinus(c,i) => CMinus(changeClock(c,act2port),i)
    case _ => ce
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
    val ints = vs.filter(v=>v.name.startsWith("since"))
    //if (vs.nonEmpty) "int[0,2] "+vs.map(Show(_)).mkString("",",",";") else ""
    if (vs.nonEmpty) "bool "+vs.map(Show(_)).mkString("",",",";\n") else ""
    if (ints.nonEmpty) "int[0,2] "+ints.map(Show(_)).mkString("",",",";\n") else ""
  }

  private def mkVariables(vars: Set[Var]):String = {
    // all booleans for know
    val bools = vars.filterNot(v=>v.name.startsWith("since"))
    val ints = vars.filter(v=>v.name.startsWith("since"))
    (if (bools.nonEmpty) "bool "+bools.map(Show(_)).mkString("",",",";\n") else "") +
      (if (ints.nonEmpty) "int[0,2] "+ints.map(Show(_)).mkString("",",",";\n") else "")
  }

  private def mkLocation(loc:Int,inv:CCons,committed:Boolean):String = {
    s"""<location id="id$loc" x="${loc*100}" y="0">
       |<name x="${loc*100-10}" y="-34">L${if (loc>=0) loc else "_"+(-1*loc)}</name>
       |${if (inv==CTrue) "" else mkInvariant(loc*100-10,inv)}
       |${if (committed) "<committed/>" else ""}
       |</location>""".stripMargin
  }

  private def mkInvariant(i:Int,cc:CCons): String =
    s"""<label kind="invariant" x="$i" y="11">${mkCC(cc)}</label>"""

  private def mkCC(cc:CCons): String = cc match {
    case CTrue => "true"
    case ET(c, n) => c+"=="+Show(n)
    case LT(c, n) => c+"&lt;"+Show(n)
    case GT(c, n) => c+"&gt;"+Show(n)
    case LE(c, n) => c+"&lt;="+Show(n)
    case GE(c, n) => c+"&gt;="+Show(n)
    case CAnd(cc1, cc2) => mkCC(cc1)+" and "+ mkCC(cc2)
  }

  private def mkTransition(e:UppaalEdge):String = {
    s"""<transition>
       |<source ref="id${e.from}"/>
       |<target ref="id${e.to}"/>
       |${if (e.ccons==CTrue /*&& e.guard==LTrue*/) "" else mkGuard(e.from,e.ccons,e.guard)}
       |${mkActLabel(e)}
       |${mkEdgeUpd(e)}
       |</transition>""".stripMargin
  }

  private def mkActLabel(e: UppaalEdge):String = {
    val syncChannel = e.ports.find(p=> p.endsWith("!")|| p.endsWith("?"))
    if (syncChannel.isDefined)
      s"""<label kind="synchronisation" x="${e.from * 100 + 15}" y="-34">${syncChannel.get}</label>"""
    else ""
//    else s"""<label kind="comments" x="${e.from * 100 + 15}" y="-34">${e.act}</label>"""
  }

  private def mkEdgeUpd(e:UppaalEdge):String = {
    val supd = Simplify(e.upd)
    if (e.creset.isEmpty) mkUpdates(e.from,mkUpd(supd))
    else if (supd == Noop) mkUpdates(e.from,e.creset.map(_+":=0").mkString(", "))
    else mkUpdates(e.from,e.creset.map(_+":=0").mkString(", ")+" , "+mkUpd(supd))
  }

  private def mkUpd(u:Update):String = Show(u)

  private def mkGuard(from:Int,cc:CCons,g:Guard): String =
    s"""<label kind="guard" x="${from*100+25}" y="-34">${mkCCG(cc,g)}</label>"""

  private def mkCCG(cc:CCons,g:Guard):String = //if (cc==CTrue) "" else mkCC(cc)
    if      (cc==CTrue) mkUFormulaGuard(g) //Show.showUppaalGuard(g)
    else if (g==Ltrue) mkCC(cc)
    else mkCC(cc)+" &amp;&amp; "+ mkUFormulaGuard(g) //Show.showUppaalGuard(g)

  private def mkUpdates(from:Int,upds:String): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${upds}</label>"""

}
