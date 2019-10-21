package hub.analyse

import hub.Guard
import ifta.ClockCons

/**
  * Created by guillecledou on 2019-10-21
  */


sealed trait UppaalFormula

case class UAA(f:UppaalStFormula) extends UppaalFormula
case class UAE(f:UppaalStFormula) extends UppaalFormula
case class UEA(f:UppaalStFormula) extends UppaalFormula
case class UEE(f:UppaalStFormula) extends UppaalFormula
case class UEventually(f1:UppaalStFormula,f2:UppaalStFormula) extends UppaalFormula

sealed trait UppaalStFormula {
  def ||(other:UppaalStFormula) = UOr(this,other)
  def &&(other:UppaalStFormula) = UAnd(this,other)
}

case object UDeadlock                                    extends UppaalStFormula
case object UTrue                                        extends UppaalStFormula
case class UDGuard(g:Guard)                              extends UppaalStFormula
case class UCGuard(c:ClockCons)                          extends UppaalStFormula
case class Location(a:String)                            extends UppaalStFormula
case class UNot(f:UppaalStFormula)                       extends UppaalStFormula
case class UAnd(f1:UppaalStFormula, f2:UppaalStFormula)  extends UppaalStFormula
case class UOr(f1:UppaalStFormula, f2:UppaalStFormula)   extends UppaalStFormula
case class UImply(f1:UppaalStFormula,f2:UppaalStFormula) extends UppaalStFormula
