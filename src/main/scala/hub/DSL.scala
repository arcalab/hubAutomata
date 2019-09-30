package hub

import scala.language.implicitConversions
import hub.analyse.DependencyGraph
import hub.backend.{Dot, Uppaal}
import hub.common.ParseException
import hub.lang.Parser

/**
  * Created by guille on 18/12/2018.
  */

object DSL {

  class Assignment(n:String) {
    def :=(e:Expr):Update = Asg(Var(n),e)
  }

  implicit def toVarOrCons(s:String):Expr = if (s.matches("[A-Z]*")) Cons(s) else Var(s)
  implicit def toVal(d:Int):Expr = Val(d)
  implicit def toAssignment(s:String):Assignment = new Assignment(s)

  def toDot(dg:DependencyGraph) = Dot(dg)

  def toUppaal(hub:HubAutomata):String  = Uppaal(hub)

  def getDG(u:Update) = DependencyGraph(u)

  def parsePattern(pattern:String):List[String] = Parser.parsePattern(pattern) match {
    case Parser.Success(f,_) => f
    case f:Parser.NoSuccess =>
      throw new ParseException(f.toString)
  }
}
