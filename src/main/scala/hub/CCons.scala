package hub

/**
  * Created by guillecledou on 2019-11-20
  */


trait CCons {
  def clocks: List[String] = this match {
    case CTrue => List()
    case ET(c, expr) => List(c) ++ expr.clocks
    case LT(c, expr) => List(c) ++ expr.clocks
    case GT(c, expr) => List(c) ++ expr.clocks
    case LE(c, expr) => List(c) ++ expr.clocks
    case GE(c, expr) => List(c) ++ expr.clocks
    case CAnd(cc1, cc2) => cc1.clocks ++ cc2.clocks
  }

  def &(other:CCons) = CAnd(this,other)
}

case object CTrue              extends CCons
case class ET(c:String,cexp:ClockExpr)  extends CCons
case class LT(c:String,cexp:ClockExpr)  extends CCons
case class GT(c:String,cexp:ClockExpr)  extends CCons
case class LE(c:String,cexp:ClockExpr)  extends CCons
case class GE(c:String,cexp:ClockExpr)  extends CCons
case class CAnd(cc1:CCons,cc2:CCons) extends CCons


trait ClockExpr {
  def clocks:List[String] = this match {
    case Clock(c) => List(c)
    case CPlus(c,i) => List(c)
    case CMinus(c,i) => List(c)
    case _ => List()
  }
}

case class CInt(i:Int) extends ClockExpr
case class Clock(c:String) extends ClockExpr
case class CPlus(c:String,i:Int) extends ClockExpr
case class CMinus(c:String,i:Int) extends ClockExpr

