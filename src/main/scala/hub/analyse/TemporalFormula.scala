package hub.analyse

import hub.{CCons, Guard}


/**
  * Created by guillecledou on 2019-10-05
  */


sealed trait TemporalFormula {

//  def hasUntil:Boolean = this match {
//    case Until(_,_) => true
//    case _          => false
//  }

  def isComplex:Boolean = this.hasEvery || this.hasUntil || this.hasBefore || this.hasWaits

  def hasWaits:Boolean = this match {
    case AA(f) => f.hasWaits
    case AE(f) => f.hasWaits
    case EA(f) => f.hasWaits
    case EE(f) => f.hasWaits
    case Eventually(f1,f2) => f1.hasWaits || f2.hasWaits
    case _ => false
  }

  def hasEvery:Boolean = this match {
    case EveryAfter(_,_,_) | Every(_,_) => true
    case _ => false
  }

  def hasUntil:Boolean = this match {
    case AA(f) => f.hasUntil
    case AE(f) => f.hasUntil
    case EA(f) => f.hasUntil
    case EE(f) => f.hasUntil
    case Eventually(f1,f2) => f1.hasUntil || f2.hasUntil
    case _ => false
//    case Until(f1,f2) => f1.hasBefore || f2.hasBefore
  }

  def hasBefore:Boolean = this match {
    case AA(f) => f.hasBefore
    case AE(f) => f.hasBefore
    case EA(f) => f.hasBefore
    case EE(f) => f.hasBefore
    case Eventually(f1,f2) => f1.hasBefore || f2.hasBefore
    case _ => false
//    case Until(f1,f2) => f1.hasBefore || f2.hasBefore
  }

  def hasDone:Boolean = this match {
    case AA(f) => f.hasDone
    case AE(f) => f.hasDone
    case EA(f) => f.hasDone
    case EE(f) => f.hasDone
    case Eventually(f1,f2) => f1.hasDone || f2.hasDone
    case _ => false
    //    case Until(f1,f2) => f1.hasBefore || f2.hasBefore
  }

  def actions:Set[String] = this match {
    case AA(f) => f.actions
    case AE(f) => f.actions
    case EA(f) => f.actions
    case EE(f) => f.actions
    case Eventually(f1,f2) => f1.actions ++ f2.actions
    case Every(a,b) => Set(a.name,b.name)
    case EveryAfter(a,b,t) => Set(a.name,b.name)
  }

  def waitModes:Set[WaitMode] = this match {
    case AA(f) => f.waitModes
    case AE(f) => f.waitModes
    case EA(f) => f.waitModes
    case EE(f) => f.waitModes
    case Eventually(f1,f2) => f1.waitModes ++ f2.waitModes
    case _ => Set()
  }
}

case class AA(f:StFormula) extends TemporalFormula
case class AE(f:StFormula) extends TemporalFormula
case class EA(f:StFormula) extends TemporalFormula
case class EE(f:StFormula) extends TemporalFormula
case class Eventually(f1:StFormula,f2:StFormula) extends TemporalFormula
//case class EventuallyBefore(f1: StFormula,f2: StFormula,f3:StFormula) extends TemporalFormula
//case class Until(f1:StFormula,f2:StFormula) extends TemporalFormula
case class Every(a:Action,b:Action) extends TemporalFormula
case class EveryAfter(a:Action,b:Action,t:Int) extends TemporalFormula


sealed trait StFormula {

  def ||(other:StFormula) = Or(this,other)
  def &&(other:StFormula) = And(this,other)

  def hasUntil:Boolean = this match {
    case Until(f1,f2)   => true
    case Before(f1,f2)  => f1.hasUntil || f2.hasUntil
    case And(f1,f2)     => f1.hasUntil || f2.hasUntil
    case Or(f1,f2)      => f1.hasUntil || f2.hasUntil
    case Imply(f1,f2)   => f1.hasUntil || f2.hasUntil
    case Not(f1)        => f1.hasUntil
    case _              => false
  }

  def hasBefore:Boolean = this match {
    case Before(f1,f2)  => true
    case And(f1,f2)     => f1.hasBefore || f2.hasBefore
    case Or(f1,f2)      => f1.hasBefore || f2.hasBefore
    case Imply(f1,f2)   => f1.hasBefore || f2.hasBefore
    case Not(f1)        => f1.hasBefore
    case _              => false
  }

  def hasWaits:Boolean = this match {
    case Waits(a,m,t) => true
    case And(f1,f2)     => f1.hasWaits || f2.hasWaits
    case Or(f1,f2)      => f1.hasWaits || f2.hasWaits
    case Imply(f1,f2)   => f1.hasWaits || f2.hasWaits
    case Not(f1)        => f1.hasWaits
    case _              => false
  }

  def hasDone:Boolean = this match {
    case DoneAction(a)  => true
    case Before(f1,f2)  => f1.hasDone || f2.hasDone
    case And(f1,f2)     => f1.hasDone || f2.hasDone
    case Or(f1,f2)      => f1.hasDone || f2.hasDone
    case Imply(f1,f2)   => f1.hasDone || f2.hasDone
    case Not(f1)        => f1.hasDone
    case _              => false
  }

  def actions:Set[String] = this match {
    case Action(name) => Set(name)
    case DoingAction(a) => Set(a)
    case DoneAction(a) => Set(a)
//    case Can(f) => f.actions
    case Not(f) => f.actions
    case And(f1,f2) => f1.actions++f2.actions
    case Or(f1,f2) => f1.actions++f2.actions
    case Imply(f1,f2) => f1.actions++f2.actions
    case Waits(a,m,t) => Set(a.name)
    case Until(f1,f2) => f1.actions++f2.actions
    case Before(f1,f2) => f1.actions++f2.actions
    case CGuard(cc) => cc.clocks.filter(_.endsWith(".t")).map(_.dropRight(2)).toSet
    case _ => Set()

  }

  def waitModes:Set[WaitMode] = this match {
    case Waits(a,m,t) => Set(m)
    case And(f1,f2)     => f1.waitModes ++ f2.waitModes
    case Or(f1,f2)      => f1.waitModes ++ f2.waitModes
    case Imply(f1,f2)   => f1.waitModes ++ f2.waitModes
    case Not(f1)        => f1.waitModes
    case _              => Set()
  }
}

case object Deadlock                        extends StFormula
case object Nothing                         extends StFormula
case object TFTrue                          extends StFormula
case class DGuard(g:Guard)                  extends StFormula
case class CGuard(c:CCons)                  extends StFormula
case class Action(name: String)             extends StFormula
case class DoneAction(name: String)         extends StFormula
case class DoingAction(a:String)            extends StFormula
case class Not(f:StFormula)                 extends StFormula
case class And(f1:StFormula, f2:StFormula)  extends StFormula
case class Or(f1:StFormula, f2:StFormula)   extends StFormula
case class Imply(f1:StFormula,f2:StFormula) extends StFormula

case class Waits(a:Action,mod:WaitMode,t:Int) extends StFormula

case class Until(f1:StFormula,f2:StFormula) extends StFormula
case class Before(f1:StFormula,f2:StFormula)extends StFormula


sealed trait WaitMode

case object AtLeast     extends WaitMode
case object AtMost      extends WaitMode
case object MoreThan extends WaitMode
case object LessThan extends WaitMode