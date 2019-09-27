package hub.backend

import hub._
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

case class UppaalEdge(from:Int,to:Int,ccons:ClockCons,creset:Set[String],guard:Guard,upd:Update) {}

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
    * Translates a hub automata into a Uppaal timed automata
    * @param hub hub automata
    * @return a string with the uppaal model (xml)
    */
  def apply(hub:HubAutomata): String = { ""
//    s"""<?xml version="1.0" encoding="utf-8"?>
//       |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
//       |<nta>
//       |<declaration>
//       |// Place global declarations here.
//       |// Channels (actions)
//       |${if (sacts.isEmpty) "" else sacts.mkString("chan ",",",";")}
//       |</declaration>
//       |${auts.zipWithIndex.map(x=>mkTemplate(x._1,x._2)).mkString("\n")}
//       |<system>
//       |// Place template instantiations here.
//       |// todo when incorporating variables
//       |// List one or more processes to be composed into a system.
//       |system ${if (auts.isEmpty) ""  else usednames.map(n => n._1).mkString("",",",";")}
//       |</system>
//       |<queries>
//       |</queries>
//       |</nta>""".stripMargin
  }

  /**
    * Given a hub automata, creates an Uppaal timed automata with committed states in between each original transition
    * @param hub hub automata
    * @return uppaal timed automata
    */
  private def mkTimeAutomata(hub:HubAutomata):Uppaal = {
    val hubedges = hub.getTransitions

    var newlocs = hub.sts
    var newedges = Set[UppaalEdge]()
    var committed = Set[Int]()
    var act2locs = Map[Int,Set[Int]]()
    var maxloc = hub.sts.max

    for ((from,to,acts,cc,cr,g,u)  <-hubedges) {
      newedges += UppaalEdge(from,maxloc+1,cc,cr,g,u)
      newedges += UppaalEdge(maxloc+1,to,CTrue,Set(),Ltrue,Noop)
      committed += (maxloc+1)
      act2locs = (act2locs.toSeq ++ acts.map(a => a -> Set(maxloc+1)).toMap.toSeq).groupBy(_._1).mapValues(_.map(_._2).toSet.flatten)
      maxloc +=1
    }

    Uppaal(newlocs,hub.init,hub.clocks,newedges,hub.inv,committed,act2locs)
  }

  private def mkLocation(loc:Int,committed:Boolean,inv:ClockCons):String = {
    s"""<location id="id$loc" x="${loc*100}" y="0">
       |<name x="${loc*100-10}" y="-34">"L"$loc</name>
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
    case CAnd(cc1, cc2) => mkCC(cc1)+" &amp;&amp; "+mkCC(cc2)
  }
}
