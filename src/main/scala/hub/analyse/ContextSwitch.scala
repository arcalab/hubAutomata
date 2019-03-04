package hub.analyse

import hub.{Guard, HubAutomata, Update}
import preo.backend.ReoGraph

/**
  * Created by guillecledou on 04/03/2019
  */


object ContextSwitch {

//  type CS = List[(Int, String, Int)] // (from, by, to)

  def apply(hub:HubAutomata, pattern:List[String]):(Boolean, List[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])],Int)={//CS ={

    var fromNameToInt:Map[String,Int] = (for (p <- hub.getInputs++hub.getOutputs) yield (hub.getPortName(p),p)).toMap
//    println(s"ports: ${(hub.getInputs ++ hub.getOutputs)}")
//    println(s"pattern actions: ${pattern.map(a => s"$a -> ${fromNameToInt.getOrElse(a,-1)}")}")
    var patternInt = pattern.map(a => fromNameToInt(a))
    println(s"intersection: ${(hub.getInputs ++ hub.getOutputs).intersect(patternInt.toSet)}")
    // Check if all actions in pattern ar part of the interface otherwise error
    if ((hub.getInputs ++ hub.getOutputs).intersect(patternInt.toSet) != patternInt.toSet)
      throw new RuntimeException("Only interface actions can be referred")
    // create a map to easily access outgoin transition from each state
    var mapAut:Map[Int,Set[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])]] = Map()
    for (t <- hub.trans) mapAut += (t._1 -> (mapAut.getOrElse(t._1,Set()) ++ Set(t._2)))
    findATrace(hub.init,mapAut,patternInt,Set())//hub.init))
  }

  def findATrace(from:Int
                 ,map:Map[Int,Set[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])]]
                 ,pattern:List[Int],visited:Set[Int]):(Boolean, List[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])],Int) = pattern match {//List[(Int,Any,String,Int)],Int) = pattern match {
    case Nil        => (true,List(),0)
    case head::rest => {
      var toVisit = map(from).filterNot(t => visited.contains((t._2,t._3,t._4,t._5).hashCode()))
      var minCs = Int.MaxValue
      var bestT:List[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])] = List()
      var solution:Boolean = false
      for (t <- toVisit) {
        // res = (found, foundTrace, foundCost)
        var res:(Boolean, List[(Int,Set[Int],Guard,Update,Set[ReoGraph.Edge])],Int) = (false,List(),0)
            t match {
          case (to,fire,g,u,es) if fire.contains(head) =>
              res = findATrace(to,map,rest,Set()) //Set(to)) //visited+to)
          case (to,fire,g,u,es) =>
              res = findATrace(to,map,head::rest,visited+((t._2,t._3,t._4,t._5).hashCode()))
        }
        solution = solution || res._1
        if (res._1) {
          var cs = (t._2.size * 2) + res._3
          if (cs < minCs) {
            minCs = cs
            bestT = t :: res._2
          }
        }
      }
      (solution, bestT, minCs)
    }
  }

}

