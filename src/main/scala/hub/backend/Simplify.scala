package hub.backend

import hub._
import hub.analyse._

/**
  * Created by guille on 18/12/2018.
  */

object Simplify {

  def apply(u: Update): Update = u match {
    case Asg(_, _) => u
    case Noop => u
    case Par(u1, u2) => (Simplify(u1), Simplify(u2)) match {
      case (Noop, Noop) => Noop
      case (Noop, u3) => u3
      case (u3, Noop) => u3
      case (u3, u4) => Par(u3, u4)
    }
    case Seq(u1, u2) => (Simplify(u1), Simplify(u2)) match {
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

  def apply(hub: HubAutomata): HubAutomata = hub match {
    case HubAutomata(ports, sts, init, trans, clocks, inv, initVal) =>
      val nInv = inv.map(i => i._1 -> ifta.analyse.Simplify(i._2))
      val nTrans = for ((from, (to, fire, g, cc, cr, upd, es)) <- trans) yield
        (from, (to, fire, Simplify(g), ifta.analyse.Simplify(cc), cr, Simplify(upd), es))
      HubAutomata(ports, sts, init, nTrans, clocks, nInv, initVal)
  }


  def apply(g: Guard): Guard = g match {
    case Ltrue => Ltrue
    case LOr(g1, g2) => (apply(g1), apply(g2)) match {
      case (Ltrue, g) => g
      case (g, Ltrue) => g
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
    case Pred(_, _) => g
  }

  def apply(f:TemporalFormula):TemporalFormula = f match {
    case AA(sf) => AA(apply(sf))
    case AE(sf) => AE(apply(sf))
    case EA(sf) => EA(apply(sf))
    case EE(sf) => EE(apply(sf))
    case Eventually(f1,f2) => Eventually(apply(f1),apply(f2))
  }

  def apply(sf: StFormula):StFormula = sf match {
    case Deadlock => Deadlock
    case a@Action(n) => a
    case TFTrue => TFTrue
    case DGuard(g)=> DGuard(apply(g))
    case CGuard(c)=> CGuard(ifta.analyse.Simplify(c))
    case Not(Not(f)) => apply(f)
    case Not(f) => Not(apply(f))
    case And(f1,f2) => (apply(f1),apply(f2)) match {
      case (TFTrue, f) => f
      case (f, TFTrue) => f
      case (Not(TFTrue), _) => Not(TFTrue)
      case (_, Not(TFTrue)) => Not(TFTrue)
      case (Not(f3), f4) => if (f3 == f4) Not(TFTrue) else And(Not(f3), f4)
      case (f3, Not(f4)) => if (f3 == f4) Not(TFTrue) else And(f3, Not(f4))
      case (f3, f4) => if (f3 == f4) f3 else And(f3, f4)
    }
    case Or(f1,f2) => (apply(f1),apply(f2)) match {
      case (TFTrue,f) => TFTrue
      case (f,TFTrue) => TFTrue
      case (Not(TFTrue), f) => f
      case (f,Not(TFTrue)) => f
      case (Not(f3),f4) => if (f3==f4) TFTrue else Or(Not(f3),f4)
      case (f3,Not(f4)) => if (f3==f4) TFTrue else Or(f3,Not(f4))
      case (f3,f4) => if (f3==f4) f3 else Or(f3,f4)
    }
    case Imply(f1,f2) => (apply(f1),apply(f2)) match {
      case (Not(TFTrue), _) => TFTrue
      case (_, TFTrue) => TFTrue
      case (f3, f4) => Imply(f3, f4)
    }
    case Before(f1,f2) => (apply(f1),apply(f2)) match {
      case (f3, f4) => Before(f3, f4)
    }
  }

}