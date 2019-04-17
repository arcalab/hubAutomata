package hub.analyse

import hub._ //{Asg, Noop, Update}

/**
  * Created by guille on 07/01/2019
  */

case class DependencyGraph(nodes:Set[Asg],links:Set[(Asg,Asg)]) {

  private lazy val children:Map[Asg,Set[Asg]] = links.groupBy(_._1).map(n => n._1 -> n._2.map(_._2))

  private lazy val parents:Map[Asg,Set[Asg]] = links.groupBy(_._2).map(n => n._1 -> n._2.map(_._1))

  /**
    * Makes a new dependency graph by joining nodes and links
    * @param dg
    * @return
    */
  def ++(dg:DependencyGraph):DependencyGraph = {
    DependencyGraph(this.nodes++dg.nodes,this.links++dg.links)
  }

  /**
    * Makes a new dependency graph by adding a new link
    * if the nodes don't exist, it creates them
    * @param source
    * @param target
    * @return
    */
  def addLink(source:Asg,target:Asg):DependencyGraph = {
    DependencyGraph(this.nodes++Set(source,target),this.links+((source,target)))
  }

  /**
    * Makes a new dependency graph by adding a set of links
    * if the nodes don't exist, it creates them
    * @param links
    * @return
    */
  def addLinks(links:Set[(Asg,Asg)]):DependencyGraph = {
    var res = this
    for (l <- links ) res = res addLink(l._1,l._2)
    res
  }

  /**
    * Return all nodes that don't have incoming links
    * @return
    */
  def getInits:Set[Asg] = nodes.filter(parents.getOrElse(_,Set()).isEmpty)

  /**
    * get all nodes that don't have outgoing links
    * @return
    */
  def getLeaves:Set[Asg] = nodes.filter(children.getOrElse(_,Set()).isEmpty)

  /**
    * Kahn's algorithm to find a topological order in a directed graph, if any
    * @return topological order, if it exists
    */
  def getTopologicalOrder:Option[List[Asg]] = {
    var nc = children
    var np = parents
    var res:List[Asg] = List()
    var inits: Set[Asg] = getInits

    while (inits.nonEmpty) {
      var n = inits.head
      inits -= n
      res = res ++ List(n)
      var childrenOfN = nc.getOrElse(n,Set())
      nc += n -> Set()
      np = np.map(node => node._1 -> (node._2 - n))
      inits = inits ++ childrenOfN.filter(m => np.getOrElse(m,Set()).isEmpty)
    }

    if (!nc.exists(m => m._2.nonEmpty) && !np.exists(m => m._2.nonEmpty))
      Some(res)
    else
      None
  }

}

object DependencyGraph {

  def emptyDG = DependencyGraph(Set(),Set())

  /**
    * Creates a dependency graph based on an update
    * Divides the update in parallel sequences
    * Creates a dependency graph for each parallel sequence
    * based on the structure and then adds dependency links
    * between graphs of different sequences
    * @param u
    * @return
    */
  def apply(u: Update):DependencyGraph = {
    var dgs:Set[DependencyGraph] = u.sequences.map(u => mkDepStruct(u))
    var links: Set[(Asg,Asg)] = Set()
    var toVisit = dgs
    for (dg <- dgs) {
      toVisit  = toVisit - dg //dgs - dg
      links ++= mkDepCons(dg, toVisit)
    }
    DependencyGraph(dgs.flatMap(_.nodes),dgs.flatMap(_.links) ++ links)
  }

  /**
    * Given an update it returns a dependency graph based on the structure
    * of the update
    * @param u
    * @return
    */
  private def mkDepStruct(u:Update):DependencyGraph = u match {
    case a:Asg => DependencyGraph(Set(a), Set())
    case Noop => DependencyGraph(Set(), Set())
//    case Group(u1) => apply(u)
    case Par(u1, u2) => apply(u1) ++ apply(u2)
    case Seq(u1, u2) => {
      var dg1 = apply(u1)
      var dg2 = apply(u2)
      var nlinks = for (l <- dg1.getLeaves; i <- dg2.getInits) yield (l, i)
      (dg1 ++ dg2).addLinks(nlinks)
    }
  }

  /**
    * Makes dependency constraints that should be added to the dependency graph
    * from an indenpendent graph dg to other independent graphs
    * @param dg
    * @param others
    * @return
    */
  private def mkDepCons(dg:DependencyGraph,others:Set[DependencyGraph]):Set[(Asg,Asg)] = {
    var otherNodes = others.flatMap(_.nodes)
    dg.nodes.flatMap(n => mkLinks(n, otherNodes))
  }

  /**
    * Makes dependency links between a node and a set of nodes
    * based on dependencies between variables
    * @param n
    * @param other
    * @return
    */
  private def mkLinks(n:Asg, other:Set[Asg]): Set[(Asg,Asg)] = {
    var res: Set[(Asg,Asg)] = Set()
    for (o <-other) {
      if (n.dependsOn(o)) res = res + ((o,n))
      if (o.dependsOn(n)) res = res + ((n,o))
    }
    res
  }
}
