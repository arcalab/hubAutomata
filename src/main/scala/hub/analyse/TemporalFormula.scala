package hub.analyse

import hub.Guard
import ifta.ClockCons

/**
  * Created by guillecledou on 2019-10-05
  */


sealed trait TemporalFormula {

  def hasUntil:Boolean = this match {
    case Until(_,_) => true
    case _          => false
  }

  def hasBefore:Boolean = this match {
    case AA(f) => f.hasBefore
    case AE(f) => f.hasBefore
    case EA(f) => f.hasBefore
    case EE(f) => f.hasBefore
    case Eventually(f1,f2) => f1.hasBefore || f2.hasBefore
    case Until(f1,f2) => f1.hasBefore || f2.hasBefore
  }
}

case class AA(f:StFormula) extends TemporalFormula
case class AE(f:StFormula) extends TemporalFormula
case class EA(f:StFormula) extends TemporalFormula
case class EE(f:StFormula) extends TemporalFormula
case class Eventually(f1:StFormula,f2:StFormula) extends TemporalFormula
//case class EventuallyBefore(f1: StFormula,f2: StFormula,f3:StFormula) extends TemporalFormula
case class Until(f1:StFormula,f2:StFormula) extends TemporalFormula


sealed trait StFormula {

  def ||(other:StFormula) = Or(this,other)
  def &&(other:StFormula) = And(this,other)

  def hasBefore:Boolean = this match {
    case Before(f1,f2)  => true
    case And(f1,f2)     => f1.hasBefore || f2.hasBefore
    case Or(f1,f2)      => f1.hasBefore || f2.hasBefore
    case Imply(f1,f2)   => f1.hasBefore || f2.hasBefore
    case Not(f1)        => f1.hasBefore
    case _              => false
  }
}

case object Deadlock                        extends StFormula
case object TFTrue                          extends StFormula
case class DGuard(g:Guard)                  extends StFormula
case class CGuard(c:ClockCons)              extends StFormula
case class Action(a:String)                 extends StFormula
case class Can(f:StFormula)                 extends StFormula
case class Not(f:StFormula)                 extends StFormula
case class And(f1:StFormula, f2:StFormula)  extends StFormula
case class Or(f1:StFormula, f2:StFormula)   extends StFormula
case class Imply(f1:StFormula,f2:StFormula) extends StFormula

case class Before(f1:StFormula,f2:StFormula)extends StFormula