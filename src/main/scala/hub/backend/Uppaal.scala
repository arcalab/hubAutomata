package hub.backend

import hub._
import hub.analyse._
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
                  committed:Set[Int],act2locs:Map[Int,Set[Int]]) {}

case class UppaalEdge(from:Int,to:Int,ports:Set[Int],ccons:ClockCons,creset:Set[String],guard:Guard,upd:Update) {}

object Uppaal {

  def emptyAutomata = Uppaal(Set(),0,Set(),Set(),Map(),Set(),Map())

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
    s"""<?xml version="1.0" encoding="utf-8"?>
       |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
       |<nta>
       |<declaration>
       |// Place global declarations here.
       |// clocks:
       |${if (uppaal.clocks.nonEmpty) uppaal.clocks.mkString("clock ", ",", ";") else ""}
       |// variables:
       |${mkVariables(uppaal)}
       |// Channels (actions)
       |</declaration>
       |${mkTemplate(uppaal)}
       |<system>
       |// Place template instantiations here.
       |// todo when incorporating variables
       |// List one or more processes to be composed into a system.
       |system Hub;
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
    val ta = mkTimeAutomata(Simplify(hub))
    println("Updates:\n"+ta.edges.map(e => Show(e.upd)).mkString("\n"))
    apply(ta)
  }

  /**
    * Given a hub automata, creates an Uppaal timed automata with committed states in between each original transition
    * @param hub hub automata
    * @return uppaal timed automata
    */
  def mkTimeAutomata(hub:HubAutomata):Uppaal = {
    val hubedges = hub.getTransitions

    var newlocs = hub.sts
    var newedges = Set[UppaalEdge]()
    var committed = Set[Int]()
    var act2locs = Map[Int,Set[Int]]()
    var maxloc = hub.sts.max

    for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
      newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,Noop)//u)
      // experimenting with setting how many times a executed before another action executes
//      var executions:Update = (for( a<-acts;other<-hub.ports;if a!=other) yield {
//        Asg(Var("v"+portToString(a)+"_"+portToString(other)),Fun("+",List(Var("v"+portToString(a)+"_"+portToString(other)),Val(1)))) &
//        Asg(Var("v"+portToString(other)+"_"+portToString(a)),Val(0))
//      }).foldRight[Update](Noop)(_&_)
      newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,Noop)//executions)
      committed += (maxloc+1)
      act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
      newlocs += maxloc+1
      maxloc +=1
    }
    val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

    Uppaal(newlocs,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs)
  }

  /**
    * Given a formula and a hub automaton, create an uppaal model with the necessary structures to verify such formula
    * @param tf temporal formula
    * @param hub hub automaton from which to create the model
    * @return Uppaal model
    */
  def fromFormula(tf:TemporalFormula,hub:HubAutomata):Uppaal = tf match {
    case f@Eventually(Action(name),Before(f2,f3)) => fromEventuallyBefore(f,hub)
    case f@Eventually(f1,Before(f2,f3)) =>
      throw new RuntimeException("Only an action is supported on the left side of an eventually before clause")
    case formula => mkTimeAutomata(hub)
  }

  private def fromEventuallyBefore(tf:Eventually,hub:HubAutomata):Uppaal = tf match {
    case Eventually(Action(name),Before(f2,f3))=>
      val hubedges = hub.getTransitions

      var newlocs = hub.sts
      var newedges = Set[UppaalEdge]()
      var committed = Set[Int]()
      var act2locs = Map[Int,Set[Int]]()
      var maxloc = hub.sts.max

      for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
        newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,Noop)//u)
        var names:Set[String] = acts.map(hub.getPortName)
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
        newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,executions)
        committed += (maxloc+1)
        act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
        newlocs += maxloc+1
        maxloc +=1
      }
      val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

      Uppaal(newlocs,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs)
  }

//  /**
//    * Given a temporal formula with syntactic sugar, expand actions an clocks of action to locations based on a given mapping
//    * @param tf temporal formula with syntactic sugar
//    * @param act2locs a map from action names to locations where those actions execute
//    * @param act2port a map from an action name to the corresponding int port number
//    * @return a temporal logic suitable for uppaal
//    */
//  def expandTemporalFormula(tf:TemporalFormula,act2locs:Map[String,Set[Int]],act2port:Map[String,Int]):TemporalFormula = tf match {
//    case AA(f) => AA(expandStFormula(f,act2locs,act2port))
//    case AE(f) => AE(expandStFormula(f,act2locs,act2port))
//    case EA(f) => EA(expandStFormula(f,act2locs,act2port))
//    case EE(f) => EE(expandStFormula(f,act2locs,act2port))
//    case Eventually(f1,f2) => Eventually(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
//  }
//
//  /**
//    * Expand action names and clocks of actions to corresponding locations and clocks
//    * @param f state formula
//    * @param act2locs a map from action names to locations where those actions execute
//    * @param act2port a map from an action name to the corresponding int port number
//    * @return a state formula suitable for uppaal
//    */
//  def expandStFormula(f: StFormula,act2locs:Map[String,Set[Int]],act2port:Map[String,Int]):StFormula = f match {
//    case Deadlock => f
//    case Action(name) =>
//      val locs:Set[StFormula] = act2locs.getOrElse(name,Set()).map(l => Action("Hub.L"+portToString(l)))
//      if (locs.nonEmpty) locs.foldRight[StFormula](Not(TFTrue))(_||_) else throw new RuntimeException("Action name not found: "+name)
//    case DGuard(g) => f
//    case CGuard(cc) => CGuard(expandCCons(cc,act2port))
//    case Not(f1) => Not(expandStFormula(f1,act2locs,act2port))
//    case And(f1,f2) => And(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
//    case Or(f1,f2) => Or(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
//    case Imply(f1,f2) => Imply(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
//    case Before(Action(a1),Action(a2)) =>
//        var a1n = if (act2port.isDefinedAt(a1)) act2port(a1) else throw new RuntimeException("Unknown port name in: "+a1)
//        var a2n = if (act2port.isDefinedAt(a2)) act2port(a2) else throw new RuntimeException("Unknown port name in: "+a2)
//      And(DGuard(Pred("==",List(Var("v"+a1n+"_"+a2n),Val(1)))),expandStFormula(Action(a2),act2locs,act2port))
//    case Before(f1,f2) =>
//      throw new RuntimeException("Only single actions are supported on an eventually before clause")
////      Before(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
//  }


  /**
    * @param formula temporal formula with syntactic sugar
    * @param act2locs a map from action names to locations where those actions execute
    * @param act2port a map from an action name to the corresponding int port number
    * @return a temporal logic suitable for uppaal
    */
  def toUppaalFormula(formula:TemporalFormula, act2locs:Map[String,Set[Int]], act2port:Map[String,Int]):UppaalFormula = {
    def tf2UF(tf: TemporalFormula): UppaalFormula = tf match {
      case AA(sf) => UAA(stf2UStF(sf))
      case AE(sf) => UAE(stf2UStF(sf))
      case EA(sf) => UEA(stf2UStF(sf))
      case EE(sf) => UEE(stf2UStF(sf))
      case Eventually(Action(a), Before(f1,f2)) => UEventually(stf2UStF(Action(a)),stf2UStF(Before(f1,f2)))
      case Eventually(f1, Before(f2,f3)) => throw new RuntimeException("Only single actions are supported on an eventually before clause")
      case Eventually(f1, f2) => UEventually(stf2UStF(f1),stf2UStF(f2))
    }

    def stf2UStF(st: StFormula): UppaalStFormula = st match {
      case Deadlock => UDeadlock
      case TFTrue => UTrue
      case Action(a) =>
        val locs: Set[UppaalStFormula] = act2locs.getOrElse(a, Set()).map(l => Location("Hub.L" + portToString(l)))
        if (locs.nonEmpty) locs.foldRight[UppaalStFormula](UNot(UTrue))(_ || _) else throw new RuntimeException("Action name not found: " + a)
      case DGuard(g) => UDGuard(g)
      case CGuard(g) => UCGuard(expandCCons(g,act2port))
      case Not(f1) => UNot(stf2UStF(f1))
      case And(f1, f2) => UAnd(stf2UStF(f1),stf2UStF(f2))
      case Or(f1, f2) => UOr(stf2UStF(f1),stf2UStF(f2))
      case Imply(f1, f2) => UImply(stf2UStF(f1),stf2UStF(f2))
      case Before(Action(a1), Action(a2)) =>
        var a1n = if (act2port.isDefinedAt(a1)) act2port(a1) else throw new RuntimeException("Unknown port name in: " + a1)
        var a2n = if (act2port.isDefinedAt(a2)) act2port(a2) else throw new RuntimeException("Unknown port name in: " + a2)
        val res = stf2UStF(And(DGuard(Pred("==", List(Var("v" + a1n + "_" + a2n), Val(1)))), Action(a2)))
        res
      case Before(f1, f2) =>
        throw new RuntimeException("Only single actions are supported on an eventually before clause")
    }

    tf2UF(formula)
  }

//  def toVerifyta(f:UppaalFormula):String = f match {
//    case UAA(sf) => "A[] " + toVerifyta(sf)
//    case UAE(sf) => "A<> " + toVerifyta(sf)
//    case UEA(sf) => "E[] " + toVerifyta(sf)
//    case UEE(sf) => "E<> " + toVerifyta(sf)
//    case UEventually(f1,f2) => toVerifyta(f1) + " --> " + toVerifyta(f2)
//  }
//
//  def toVerifyta(sf:UppaalStFormula):String = sf match {
//    case UDeadlock => "deadlock"
//    case UTrue => "true"
//    case Location(l) => l
//    case UDGuard(g) => Show(g)
//    case UCGuard(g) => mkCC(g)
//    case UNot(f1) => "not("+ toVerifyta(f1) + ")"
//    case UAnd(f1,f2) => "(" + toVerifyta(f1) + " and " + toVerifyta(f2) + ")"
//    case UOr(f1,f2) => "(" + toVerifyta(f1) + " or " + toVerifyta(f2) + ")"
//    case UImply(f1,f2) => "(" + toVerifyta(f1) + " imply " + toVerifyta(f2) + ")"
//  }

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
    val name = "Hub"
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
    if      (cc==CTrue) Show(g)
    else if (g==Ltrue) mkCC(cc)
    else mkCC(cc)+" &amp;&amp; "+Show(g)

//  private def mkG(g:Guard):String = g match {
//    case Ltrue => "true"
//    case LNot(Ltrue) => "false"
//    case LOr(g1, g2) => mkG(g1) + "  |amp;|amp; " + mkG(g2)
//    case LAnd(g1, g2) => mkG(g1) + "  &amp;&amp; " + mkG(g2)
//    case LNot(g) => s"!amp;(${mkG(g)})"
//    case Pred(name, a1::a2::Nil) if isKnownBinOp(name) =>
//    Show(a1)+binOpToStr(name)+Show(a2)
//    case Pred(name,param) => s"$name(${param.map(Show(_)).mkString(",")})"
//  }
//
//  private def isKnownBinOp(op:String):Boolean = Set("<=","<","==",">",">=","+","-").contains(op)
//  private def binOpToStr(op:String):String = op match {
//    case "<=" => "&lt;="
//    case "<" => "&lt;"
//    case ">" => "&gt;"
//    case ">=" => "&gt;="
//    case _ => op
//  }

  private def mkUpdates(from:Int,upds:String): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${upds}</label>"""

}
