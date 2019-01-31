package hub.backend

import hub.{Asg, Noop, Par, Seq, Update}

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
}