package hub

import scala.language.implicitConversions
import hub.analyse.{DependencyGraph, TemporalFormula}
import hub.backend.{Dot, Simplify, Uppaal}
import hub.common.ParseException
import hub.lang.{Parser, TemporalFormulaParser}
import preo.ast.{IVal, Port, Prim}

/**
  * Created by guille on 18/12/2018.
  */

object DSL {

  /* Known names for virtuoso hubs */
  val hubs:Set[String] = Set("semaphore","resource", "port","dataEvent","event","fifo","blackboard","eventFull","dataEventFull","fifoFull","blackboardFull","await","timeout")

  /* Known names for primitive connectors */
  val primitiveConnectors:Set[String] = Set("node","dupl","dupls","xor","xors","mrg","drain","timer","nbtimer","writer","reader","nbreader","putNB","getNB")


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

  def parseFormula(formula:String):Either[String,List[TemporalFormula]] = TemporalFormulaParser.parse(formula) match {
    case TemporalFormulaParser.Success(f,_) => Right(f)
    case e:TemporalFormulaParser.NoSuccess  => Left(e.toString)
  }

}
