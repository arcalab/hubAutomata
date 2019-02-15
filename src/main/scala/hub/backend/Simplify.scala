package hub.backend

import hub._

/**
  * Created by guille on 18/12/2018.
  */

object Simplify {

  def apply(u:Update):Update = u match {
    case Asg(_,_) => u
    case Noop => u
    case Par(u1,u2) => (Simplify(u1),Simplify(u2)) match {
      case (Noop, Noop) => Noop
      case (Noop, u3) => u3
      case (u3, Noop) => u3
      case (u3, u4) => Par(u3, u4)
    }
    case Seq(u1,u2) => (Simplify(u1),Simplify(u2)) match {
      case (Noop, Noop) => Noop
      case (Noop, u3) => u3
      case (u3, Noop) => u3
      case (u3, u4) => Seq(u3, u4)
    }
//    case Group(u1) => Simplify(u1) match {
//      case Noop() => Noop()
//      case Seq(u2,u3) => Seq(u2,u3)
//      case Asg(x,e) => Asg(x,e)
//      case Group(u3) => u3
//      case u3 => Group(u3)
//    }
  }


  def apply(g:Guard):Guard = g match {
    case Ltrue => Ltrue
    case LOr(g1, g2) => (apply(g1),apply(g2)) match {
      case (Ltrue,g) => g
      case (g,Ltrue) => g
      case (LNot(Ltrue), g) => g
      case (g, LNot(Ltrue)) => g
      case (LNot(g3), g4) => if (g3 == g4) Ltrue else LOr(LNot(g3), g4)
      case (e3, LNot(e4)) => if (e3 == e4) Ltrue else LOr(e3, LNot(e4))
      case (e3, e4) => if (e3 == e4) e3 else LOr(e3, e4)
    }
    case LAnd(g1, g2) => (apply(g1), apply(g2)) match {
      case (Ltrue, g) => g
      case (g, Ltrue) => g
      case (LNot(Ltrue), _) => LNot(Ltrue)
      case (_, LNot(Ltrue)) => LNot(Ltrue)
      case (LNot(g3), g4) => if (g3 == g4) LNot(Ltrue) else LAnd(LNot(g3), g4)
      case (g3, LNot(e4)) => if (g3 == e4) LNot(Ltrue) else LAnd(g3, LNot(e4))
      case (g3, g4) => if (g3 == g4) g3 else LAnd(g3, g4)
    }
    case LNot(g1) => apply(g1) match {
      case LNot(g2) => g2
      case g2 => LNot(g2)
    }
    case Pred(_,_) => g
  }
}