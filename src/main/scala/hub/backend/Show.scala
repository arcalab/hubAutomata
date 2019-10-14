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
  }

  def apply(f: StFormula):String = f match {
    case Deadlock => "deadlock"
    case TFTrue => "true"
    case Action(a) => a
    case DGuard(g) => apply(g)
    case CGuard(g) => ifta.backend.Show(g)
    case Not(f1) => "not("+ apply(f1) + ")"
    case And(f1,f2) => "(" + apply(f1) + " and " + apply(f2) + ")"
    case Or(f1,f2) => "(" + apply(f1) + " or " + apply(f2) + ")"
    case Imply(f1,f2) => "(" + apply(f1) + " imply " + apply(f2) + ")"
    case Before(f1,f2) => ""
  }
}