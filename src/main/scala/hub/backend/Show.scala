package hub.backend

import hub._
import hub.analyse._

/**
  * Created by guille on 18/12/2018.
  */

object Show {

  def apply(u:Update):String = u match {
    case Asg(x,e) => x.name + " := " + Show(e)//x.name + " ← " + Show(e)
    case Par(u1,u2) => Show(u1) + " | " + Show(u2)
    case Seq(u1,u2) =>  Show(u1) + " , " + Show(u2)//Show(u1) + " ; " + Show(u2)
//    case Group(u) => "(" + Show(u) + ")"
    case Noop => "" //"noop"
  }

  def apply(e:Expr):String = e match {
    case Var(n,v) => n
    case Val(d) => d.toString
    case Cons(n,v) => n
    case Fun(name,a1::a2::Nil) if name.matches("[^a-zA-Z0-9]") =>
      apply(a1)+s" $name "+apply(a2) // parenthesis?
    case Fun(name,args) => s"$name(${args.map(apply).mkString(",")})"
  }

  def apply(g:Guard):String = g match {
    case Ltrue => "⊤"
    case LNot(Ltrue) => "⊥"
    case LOr(g1, g2) => apply(g1) + " | " + apply(g2)
    case LAnd(g1, g2) => apply(g1) + " & " + apply(g2)
    case LNot(g) => s"¬(${apply(g)})"
    case Pred(name, a1::a2::Nil) if name.matches("[^a-zA-Z0-9]") =>
      apply(a1)+name+apply(a2)
    case Pred(name,param) => s"$name(${param.map(apply).mkString(",")})"
  }

  def apply(f:TemporalFormula): String = f match {
    case AA(sf) => "A[] " + apply(sf)
    case AE(sf) => "A<> " + apply(sf)
    case EA(sf) => "E[] " + apply(sf)
    case EE(sf) => "E<> " + apply(sf)
    case Eventually(f1,f2) => apply(f1) + " --> " + apply(f2)
//    case Until(f1,f2) => apply(f1) + " until " + apply(f2)
    case Every(a,b) => "every "+ a.a + " --> " + b.a
    case EveryAfter(a,b,t) => "every "+ a.a + " --> " + b.a + " after " + t

  }

  def apply(f: StFormula):String = f match {
    case Deadlock => "deadlock"
    case Nothing=> "nothing"
    case TFTrue => "true"
    case Action(a) => "@"+a
    case DoingAction(a) => "doing " + a
    case DGuard(g) => apply(g)
    case CGuard(g) => ifta.backend.Show(g)
    case Can(f1) => "can ("+apply(f1)+")"
    case Not(f1) => "not ("+ apply(f1) + ")"
    case And(f1,f2) => "(" + apply(f1) + " and " + apply(f2) + ")"
    case Or(f1,f2) => "(" + apply(f1) + " or " + apply(f2) + ")"
    case Imply(f1,f2) => "(" + apply(f1) + " imply " + apply(f2) + ")"
    case Before(f1,f2) => apply(f1) + " before " + apply(f2)
    case Until(f1,f2) => apply(f1) + " until " + apply(f2)
  }

  def apply(f:UppaalFormula): String = f match {
    case UAA(sf) => "A[] " + apply(sf)
    case UAE(sf) => "A<> " + apply(sf)
    case UEA(sf) => "E[] " + apply(sf)
    case UEE(sf) => "E<> " + apply(sf)
    case UEventually(f1,f2) => apply(f1) + " --> " + apply(f2)
  }

  def apply(f: UppaalStFormula):String = Simplify(f) match {
    case UDeadlock => "deadlock"
    case UTrue => "true"
    case Location(l) => l
    case UDGuard(g) => showUppaalGuard(g)
    case UCGuard(g) => ifta.backend.Show(g)
    case UNot(f1) => "not("+ apply(f1) + ")"
    case UAnd(f1,f2) => parShow(f1) + " and " + parShow(f2)
    case UOr(f1,f2) =>  parShow(f1) + " or " + parShow(f2)
    case UImply(f1,f2) => parShow(f1) + " imply " + parShow(f2)
  }
  private def parShow(f: UppaalStFormula): String = f match {
    case UDeadlock | UTrue | UNot(_) => apply(f)
    case _ => "("+apply(f)+")"
  }


  def showUppaalGuard(g:Guard):String = Simplify(g) match {
    case Ltrue => "true"
    case LNot(Ltrue) => "false"
    case LOr(g1, g2) => showUppaalGuard(g1) + " || " + showUppaalGuard(g2)
    case LAnd(g1, g2) => showUppaalGuard(g1) + "  && " + showUppaalGuard(g2)
    case LNot(g) => s"!(${showUppaalGuard(g)})"
    case Pred(name, a1::a2::Nil) if  Set("<=","<","==",">",">=","+","-").contains(name) =>
      apply(a1)+name+apply(a2)
    case Pred(name,param) => s"$name(${param.map(apply(_)).mkString(",")})"
  }

}