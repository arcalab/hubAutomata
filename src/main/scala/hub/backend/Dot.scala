package hub.backend

import hub.{Asg, HubAutomata, Update}
import hub.analyse.DependencyGraph

/**
  * Created by guille on 08/01/2019
  */


object Dot {

  def apply(dg:DependencyGraph):String = {
    s"""digraph G {
       |rankdir=LR;
       |node [margin=0.1 width=1 height=0.2,style="rounded",shape="rectangle"]
       |edge [arrowsize=0.7]
       |
      |${mkDgNodes(dg.nodes)}
       |
      |${mkDgLinks(dg.links)}
       |
      |}
    """.stripMargin
  }


  private def mkDgNodes(nodes:Set[Asg]):String = {
    nodes.map(n => s"""{node [label="${n.toString}"] ${n.hashCode()}}""").mkString("\n")
  }

  private def mkDgLinks(links: Set[(Asg,Asg)]):String = {
    links.map(l => s"""${l._1.hashCode()} -> ${l._2.hashCode()}""").mkString("\n")
  }

  def apply(hub:HubAutomata):String = {
    s"""digraph G {
       |rankdir=LR;
       |node [margin=0.1 width=1 height=0.2,shape="circle"]
       |edge [arrowsize=0.7]
       |
       |${mkHubLinks(hub.getTrans())}
       |
       |}
    """.stripMargin
  }

  private def mkHubLinks(transitions: Set[(Int, Any, String, Int)]):String = {
    transitions.map(mkHubLink).mkString("\n")
  }

  private def mkHubLink(trans:(Int,Any,String,Int)):String = trans match {
    case (from,lbl,_,to) => s"""$from -> $to [label=$lbl]"""
  }

}
