package hub.analyse

import hub.Guard
import ifta.ClockCons

/**
  * Created by guillecledou on 2019-10-05
  */


sealed trait TemporalFormula

case class AA(f:StFormula) extends TemporalFormula
case class AE(f:StFormula) extends TemporalFormula
case class EA(f:StFormula) extends TemporalFormula
case class EE(f:StFormula) extends TemporalFormula
case class Eventually(f1:StFormula,f2:StFormula) extends TemporalFormula

sealed trait StFormula {
  def ||(other:StFormula) = Or(this,other)
  def &&(other:StFormula) = And(this,other)
}

case object Deadlock                        extends StFormula
case object TFTrue                          extends StFormula
case class DGuard(g:Guard)                  extends StFormula
case class CGuard(c:ClockCons)              extends StFormula
case class Action(a:String)                 extends StFormula
case class Not(f:StFormula)                 extends StFormula
case class And(f1:StFormula, f2:StFormula)  extends StFormula
case class Or(f1:StFormula, f2:StFormula)   extends StFormula
case class Imply(f1:StFormula,f2:StFormula) extends StFormula