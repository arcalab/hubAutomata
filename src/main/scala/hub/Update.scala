package hub

import backend.{Show, Simplify}
import hub.analyse.DependencyGraph

/**
  * Created by guille on 18/12/2018.
  */

sealed trait Update {

  /**
    * Returns all parallel sequences of this update
    * @return
    */
  def sequences:Set[Update] = this match {
    case Par(u1,u2) => u1.sequences ++ u2.sequences
    case Noop => Set()
    case u => Set(u)
  }

  def prod:Set[Var] = this match {
    case Asg(x,e) => Set(x)
    case Par(u1,u2) => u1.prod ++ u2.prod
    case Seq(u1,u2) => u1.prod ++ u2.prod
//    case Group(u) => u.prod
    case Noop => Set()
  }

  def dep:Set[Var] = this match {
    case Asg(x,e) => e.vars
    case Par(u1,u2) => u1.dep ++ u2.dep
    case Seq(u1,u2) => u1.dep ++ u2.dep
//    case Group(u) => u.dep
    case Noop => Set()
  }

  def vars:Set[Var] = prod ++ dep

  def instance:Update = {
    DependencyGraph((this)).getTopologicalOrder match  {
      case None => throw new RuntimeException("Impossible to serialize")
      case Some(sol) => Simplify(sol.foldRight[Update](Noop)(Seq(_,_)))
    }
  }

  /* Join by sequence or parallel */

  def &(other:Update):Update = Seq(this,other)
  def |(other:Update):Update = Par(this,other)

  /* Some auxiliary functions to detect conflicts */

  def dependsOn(other:Update):Boolean =
    this.dep.intersect(other.prod).nonEmpty

  def independent(other:Update): Boolean =
    this.conflictVars(other).isEmpty

  def <(other:Update):Boolean = {
    this.prod.intersect(other.dep).nonEmpty &&
    this.dep.intersect(other.prod).isEmpty
  }

  def conflictVars(other:Update):Set[Var] = {
    this.prod.intersect(other.dep) ++
      this.dep.intersect(other.prod)
  }

  def size: Int = this match {
    case Asg(x, e) => 1
    case Par(u1, u2) => u1.size + u2.size
    case Seq(u1, u2) => u1.size + u2.size
    case Noop => 0
  }

  /* to string */

  override def toString = Show(this)
}


case class Asg(x:Var, e:Expr)       extends Update
case class Par(u1:Update,u2:Update) extends Update
case class Seq(u1:Update,u2:Update) extends Update
//case class Group(u:Update) extends Update
case object Noop                  extends Update  // no op

sealed trait Expr {

  def vars:Set[Var] = this match {
    case Var(x,v) => Set(Var(x,v))
    case Val(_) => Set()
    case Cons(_,_) => Set()
    case Fun(_,args) => args.toSet.flatMap((_:Expr).vars)
  }

  override def toString = Show(this)
}

case class Var(name:String,value:Int = 0)   extends Expr
case class Val(d:Int)                       extends Expr
case class Cons(name:String,value:Int = 0)  extends Expr
case class Fun(name:String,args:List[Expr]) extends Expr