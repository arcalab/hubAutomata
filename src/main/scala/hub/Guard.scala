package hub

/**
  * Created by guillecledou on 28/01/2019
  */


sealed trait Guard {

  def vars: Set[Var] = this match {
    case Ltrue => Set()
    case Pred(_,param) => param.flatMap(_.vars).toSet
    case LNot(g) => g.vars
    case LOr(g1, g2) => g1.vars ++ g2.vars
    case LAnd(g1, g2) => g1.vars ++ g2.vars
  }

  def ||(other:Guard): Guard = LOr(this,other)
  def &&(other:Guard): Guard = LAnd(this,other)
}


case object Ltrue                             extends Guard
case class Pred(name:String, param:List[Expr]) extends Guard
case class LOr(g1:Guard,g2:Guard)             extends Guard
case class LAnd(g1:Guard,g2:Guard)            extends Guard
case class LNot(g:Guard)                      extends Guard
