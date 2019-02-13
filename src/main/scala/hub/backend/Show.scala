package hub.backend

import hub._

/**
  * Created by guille on 18/12/2018.
  */

object Show {

  def apply(u:Update):String = u match {
    case Asg(x,e) => x.name + " ← " + Show(e)
    case Par(u1,u2) => Show(u1) + " | " + Show(u2)
    case Seq(u1,u2) =>  Show(u1) + " ; " + Show(u2)
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
    case LOr(g1, g2) => apply(g1) + " || " + apply(g2)
    case LAnd(g1, g2) => apply(g1) + " && " + apply(g2)
    case LNot(g) => s"¬(${apply(g)})"
    case Pred(name, a1::a2::Nil) if name.matches("[^a-zA-Z0-9]") =>
      apply(a1)+name+apply(a2)
    case Pred(name,param) => s"$name(${param.map(apply).mkString(",")})"
  }

}