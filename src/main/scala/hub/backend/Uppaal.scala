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
      newedges += UppaalEdge(from,maxloc+1,acts,cc,cr,g,u)
      newedges += UppaalEdge(maxloc+1,to,Set(),CTrue,acts.map(a => s"t${if (a>=0) a.toString else "_"+Math.abs(a).toString}"),Ltrue,Noop)
      committed += (maxloc+1)
      act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
      newlocs += maxloc+1
      maxloc +=1
    }
    val actclocks = hub.ports.map(p => s"t${if (p>=0) p.toString else "_"+Math.abs(p).toString}")

    Uppaal(newlocs,hub.init,hub.clocks++actclocks,newedges,hub.inv,committed,act2locs)
  }

  /**
    * Given a temporal formula with syntactic sugar, expand actions an clocks of action to locations based on a given mapping
    * @param tf temporal formula with syntactic sugar
    * @param act2locs a map from action names to locations where those actions execute
    * @param act2port a map from an action name to the corresponding int port number
    * @return a temporal logic suitable for uppaal
    */
  def expandTemporalFormula(tf:TemporalFormula,act2locs:Map[String,Set[Int]],act2port:Map[String,Int]):TemporalFormula = tf match {
    case AA(f) => AA(expandStFormula(f,act2locs,act2port))
    case AE(f) => AE(expandStFormula(f,act2locs,act2port))
    case EA(f) => EA(expandStFormula(f,act2locs,act2port))
    case EE(f) => EE(expandStFormula(f,act2locs,act2port))
    case Eventually(f1,f2) => Eventually(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
  }

  /**
    * Expand action names and clocks of actions to corresponding locations and clocks
    * @param f state formula
    * @param act2locs a map from action names to locations where those actions execute
    * @param act2port a map from an action name to the corresponding int port number
    * @return a state formula suitable for uppaal
    */
  def expandStFormula(f: StFormula,act2locs:Map[String,Set[Int]],act2port:Map[String,Int]):StFormula = f match {
    case Deadlock => Deadlock
    case Action(name) =>
      val locs:Set[StFormula] = act2locs.getOrElse(name,Set()).map(l => Action("L"+portToString(l)))
      if (locs.nonEmpty) locs.foldRight[StFormula](Not(TFTrue))(_||_) else throw new RuntimeException("Action name not found: "+name)
    case d@DGuard(g) => d
    case CGuard(cc) => CGuard(expandCCons(cc,act2port))
    case Not(f1) => Not(expandStFormula(f,act2locs,act2port))
    case And(f1,f2) => And(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
    case Or(f1,f2) => Or(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
    case Imply(f1,f2) => Imply(expandStFormula(f1,act2locs,act2port),expandStFormula(f2,act2locs,act2port))
  }

  /**
    * Expand clock constraints of the form action.t to the actual clock associanted to the action based on act2port
    * @param cc clock constraint
    * @param act2port a map from an action name to the corresponding int port number (used as clock name: "t"+portname
    * @return a suitable clock constraint for uppaal
    */
  def expandCCons(cc:ClockCons,act2port:Map[String,Int]):ClockCons = cc match {
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
       |// Place local declarations here.
       |// clocks:
       |${if (uppaal.clocks.nonEmpty) uppaal.clocks.mkString("clock ", ",", ";") else ""}
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
       s"""|${if (e.creset.isEmpty) "" else mkReset(e.from,e.creset)}
       |</transition>""".stripMargin
  }

//  private def mkActLabel(e: UppaalEdge):String = {
//    if (e.ports.endsWith("!") || e.act.endsWith("?"))
//      s"""<label kind="synchronisation" x="${e.from * 100 + 15}" y="-34">${e.act}</label>"""
//    else s"""<label kind="comments" x="${e.from * 100 + 15}" y="-34">${e.act}</label>"""
//  }

  private def mkGuard(from:Int,cc:ClockCons,g:Guard): String =
    s"""<label kind="guard" x="${from*100+25}" y="-34">${mkCCG(cc,g)}</label>"""

  private def mkCCG(cc:ClockCons,g:Guard):String = if (cc==CTrue) "" else mkCC(cc)
//    if      (cc==CTrue) mkG(g)
//    else if (g==FTrue) mkCC(cc)
//    else mkCC(cc)+" &amp;&amp; "+mkG(g)

//  private def mkG(g:Guard):String = "" // for now ignore guards

  private def mkReset(from:Int,cs:Set[String]): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${cs.map(_+":=0").mkString(", ")}</label>"""
}
