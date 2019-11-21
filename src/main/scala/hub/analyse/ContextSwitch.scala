package hub.analyse

import hub.{CCons, Guard, HubAutomata, Update}
import preo.backend.Network

/**
  * Created by guillecledou on 04/03/2019
  */


object ContextSwitch {

  def apply(hub:HubAutomata, pattern:List[String]):(Boolean, List[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim]))],Int)={//CS ={

    var fromNameToInt:Map[String,Int] = (for (p <- hub.getInputs++hub.getOutputs) yield (hub.getPortName(p),p)).toMap
    var patternInt = pattern.map(a => fromNameToInt(a))

    // Check if all actions in pattern ar part of the interface otherwise error
    if ((hub.getInputs ++ hub.getOutputs).intersect(patternInt.toSet) != patternInt.toSet)
      throw new RuntimeException("Only interface actions can be referred")

    // create a map to easily access outgoin transition from each state
    var mapAut:Map[Int,Set[(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim])]] = Map()
    for (t <- hub.trans) mapAut += (t._1 -> (mapAut.getOrElse(t._1,Set()) ++ Set(t._2)))

    findATrace(hub.init,mapAut,patternInt,Set())//hub.init))
  }

  def findATrace(from:Int
                 ,map:Map[Int,Set[(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim])]]
                 ,pattern:List[Int],visited:Set[Int]):(Boolean, List[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim]))],Int) = pattern match {//List[(Int,Any,String,Int)],Int) = pattern match {

    case Nil        => (true,List(),0)
    case head::rest =>

      var toVisit:Set[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim]))] =
        map(from).filterNot(t => visited.contains((t._2,t._3,t._4,t._5,t._6,t._7).hashCode())).map(e => (from,e))
      var minCs = Int.MaxValue
      var bestT:List[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim]))] = List()
      var solution:Boolean = false

      for (t <- toVisit) {
        // res = (found, foundTrace, foundCost)
        var res:(Boolean, List[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Network.Prim]))],Int) = (false,List(),0)
            t match {
          case (f,(to,fire,g,cc,cr,u,es)) if fire.contains(head) =>
              res = findATrace(to,map,rest,Set()) //Set(to)) //visited+to)
          case (f,(to,fire,g,cc,cr,u,es)) =>
              res = findATrace(to,map,head::rest,visited+((fire,g,cc,cr,u,es).hashCode()))
        }
        solution = solution || res._1
        if (res._1) {
          var cs = (t._2._2.size * 2) + res._3
          if (cs < minCs) {
            minCs = cs
            bestT = t :: res._2
          }
        }
      }

      (solution, bestT, minCs)
  }

}

