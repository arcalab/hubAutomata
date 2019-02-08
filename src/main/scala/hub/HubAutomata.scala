package hub

import hub.HubAutomata.Trans
import hub.DSL._
import hub.backend.{Show, Simplify}
import preo.ast.CPrim
import preo.backend.ReoGraph.Edge
import preo.backend.{Automata, AutomataBuilder, PortAutomata}


//case class Edge(prim: String, ins:List[Int], outs:List[Int], parents:List[String])
//case class ReoGraph(edges:List[Edge], ins:List[Int], outs:List[Int]) {
//  def ++(other:ReoGraph) = ReoGraph(edges++other.edges,ins++other.ins,outs++other.outs)
//}


/**
  * Representation of an automata, aimed at being generated from a [[preo.backend.ReoGraph]].
  * @param ports Represent the possible labels (actions)
  * @param init Initial state
  * @param trans Transitions - Relation between input and output states, with associated
  *              sets of actions and of edges (as in [[Edge]]).
  */
case class HubAutomata(ports:Set[Int],init:Int,trans:Trans) extends Automata {

  /** Collects all states, seen as integers */
  def getStates: Set[Int] = (for((x,(y,_,_,_,_)) <- trans) yield Set(x,y)).flatten + init
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  def getInit: Int = init

  /** Returns the transitions to be displayed */
  def getTrans: Set[(Int,Any,String,Int)] = // from, label, id, to
    for ((from, (to, fire, g, upd, es)) <- trans)
      yield (
        from
        , s"${Show(Simplify(g))}~"+es.map(getName(_,fire))
        .filterNot(s => s=="sync" || s=="sync↓" || s=="sync↑" || s=="sync↕")
        .foldRight[Set[String]](Set())(cleanDir)
        .mkString(".")+s"~${Show(Simplify(upd))}"
        , (fire,es).hashCode().toString //todo fix needs more than acts (guards and updates)
        , to)

  /* Return the set of input ports */
  def getInputs: Set[Int] = (for((_,(_,_,_,_,edges)) <- trans) yield edges.flatMap(_.ins)).flatten intersect ports

  /* Return the set of output ports */
  def getOutputs: Set[Int] = (for((_,(_,_,_,_,edges)) <- trans) yield edges.flatMap(_.outs)).flatten intersect ports

  /* Return the set of internal variable names */
  def getInternalVarsNames: Set[String] =
    (for((_,(_,_,g,u,_)) <- trans) yield g.vars ++ u.vars).flatten.map(_.name) --
      (getInputs ++ getOutputs).map(_.toString)

  /* Return the set of all variables */
  def getVars:Set[Var] = (for((_,(_,_,g,u,_)) <- trans) yield g.vars ++ u.vars).flatten

  def getInternalVars:Set[Var] = {
    var globalVars: Set[String] = (this.getOutputs ++ this.getInputs).map(_.toString)
    (for ((_, (_, _, g, u, _)) <- trans) yield g.vars ++ u.vars).flatten.filterNot(v => globalVars.contains(v.name))
  }

  // number of states, a list of (varTypeName, sizeOfvarTypeName in number of bytes)
  //  Memory = (Int,List[(String,Int)])
  //todo: proper memory class perhaps when vars are of different types.
  def memory:(Int,List[(String,Int)]) = {
    val stSize = getStates.size
    val varSize = getInternalVars.toList.map(v => (v.value.getClass.toString, 32))//(32 bit integers) for now all variables are of type int
    (stSize,varSize)
  }

  private def getName2(edge: Edge,fire:Set[Int]):String =
    s"${edge.prim}-${edge.parents.mkString("/")}-${fire.mkString(":")}"

  private def getName(edge: Edge,fire:Set[Int]):String =
    if (fire.intersect((edge.ins ++ edge.outs).toSet).nonEmpty) {
      (edge.parents match {
        case Nil     => edge.prim.name
        case ""::_   => edge.prim.name
        case head::_ => head
      }) + getDir(edge,fire)
    } else ""  //+
  //  s"[${edge.ins.toSet.intersect(fire).mkString("|")}->${edge.outs.toSet.intersect(fire).mkString("|")}]"
  //  fire.mkString("|")
  private def getDir(edge: Edge,fire:Set[Int]): String = {
      val src = (edge.ins.toSet intersect fire).nonEmpty
      val snk = (edge.outs.toSet intersect fire).nonEmpty
      (src, snk) match {
        case (true, false) => "↓"
        case (false, true) => "↑"
        case (true,true) => "↕"
        case _ => ""
      }
    }

//  private def primName(prim: CPrim): String = (prim.name,prim.extra) match {
//    case ("writer",Some(s:String)) => s"wr($s)"
//    case ("reader",Some(s:String)) => s"rd($s)"
//    case (n,Some(s:String)) => s"$n($s)"
//    case (n,_) => n
//  }
  private def cleanDir(s:String,rest:Set[String]): Set[String] =
  if (s.nonEmpty) {
    (s.init, s.last) match {
      case (name, '↓') if rest.contains(name + '↑') || rest.contains(name + '↕') =>
        rest - (name + '↓') - (name + '↑') + (name + '↕')
      case (name, '↑') if rest.contains(name + '↓') || rest.contains(name + '↕') =>
        rest - (name + '↓') - (name + '↑') + (name + '↕')
      case _ => rest + s
    }
  } else rest

  private def printPrim(edge: Edge):String = {
    s"""${edge.prim}-${edge.ins.mkString(".")}-${edge.outs.mkString(".")}"""
  }

  /**
    * Remove unreachable states (traverse from initial state)
    * @return automata without unreachable states
    */
  private def cleanup: HubAutomata = {
    var missing = Set(init)
    var done = Set[Int]()
    var ntrans: Trans = Set()
    while (missing.nonEmpty) {
      val next = missing.head
      missing = missing.tail
      done += next
      for (t@(from, (to,_,_,_, _)) <- trans if from == next) {
        ntrans += t
        if (!(done contains to)) missing += to
      }
    }
    HubAutomata(ports, init, ntrans)
  }

  // TODO: maybe move these methods to Simplify?
  /**
    * Simplify transitions by cleaning updates (removing unused and intermediate variables)
    * @return automata without intermediate nor unused variables
    */
  def simplify: HubAutomata = {
    // all variables produced in transitions of the hub
    val allProd: Set[Var] = this.trans.flatMap(t => t._2._4.prod)
    // all variables dependencies in transitions of the hub
    var allDep: Set[Var] = this.trans.flatMap(t => t._2._4.dep ++ t._2._3.vars)

    var internalVars:Set[Var] = this.getInternalVars
    var unusedVars:Set[Var] = internalVars -- allProd.intersect(allDep)

    // variables that in some transition appear only on the right-hand side (act as dependencies)
    var areDependencies:Set[Var] = Set()
    // variables that always act as intermediate
    var intermediateVars:Set[Var] = Set()

    // get all intermediate variables
    for (t@(from, (to, p, g, u, es)) <- trans) {
      var intermediateOfU =  getIntermediateUpd(u,Set())
      intermediateVars ++=  intermediateOfU -- areDependencies
      areDependencies ++= u.dep -- intermediateOfU
    }

    var ntrans: Trans = Set()
    for (t@(from,(to,p,g,u,es)) <- trans) {
      // remove unused variables of u
      var cleanUpd = if (u.vars.intersect(unusedVars).isEmpty) u else rmUnusedUpd(u,unusedVars)
      // remove intermediate variables of u
      cleanUpd = if (cleanUpd.vars.intersect(intermediateVars).isEmpty) cleanUpd else rmIntermediateUpd(cleanUpd,intermediateVars)
      ntrans += ((from,(to,p,g,Simplify(cleanUpd),es)))
    }
    HubAutomata(ports,init,ntrans)
  }

  // Todo: refactor
  private def rmIntermediateUpd(u: Update, intermediate: Set[Var]): Update = {
    var remap:Map[Var,Expr] = Map()

    def rmInterUpd(u:Update,intermediate:Set[Var]):Update = u match {
      case a@Asg(x,e) => {
        var asg:Update = a
        var expr = e
        if (intermediate.intersect(e.vars).nonEmpty) {
          expr = rmInterExpr(e,intermediate)
        }
        if (intermediate.contains(x)) {
          remap += x -> rmInterExpr(expr,intermediate);
          asg = Noop
        }
        if (asg == Noop) Noop else Asg(x,expr)
      }
      case Noop => Noop
      case Seq(u1,u2) => Seq(rmInterUpd(u1,intermediate),rmInterUpd(u2,intermediate))
      case Par(_,_) => throw new RuntimeException("Removal of intermediate variables must be performed on serialized updates")
    }

    def rmInterExpr(e:Expr, intermediate:Set[Var]):Expr = e  match {
      case Val(d) => Val(d)
      case v@Var(x,va) => {
        if (intermediate.contains(v))
          if (remap.contains(v)) remap(v)
          else throw new RuntimeException(s"Trying to remove intermediate variable ${v} not previously defined") //should never hapend
        else v
      }
      case Fun(n,args) => Fun(n,args.map(e => rmInterExpr(e,intermediate)))
    }

    rmInterUpd(u,intermediate)
  }

  // Todo: refactor
  private def getIntermediateUpd(u:Update, prodBefore:Set[Var]):Set[Var] = u match {
    case Asg(x,e) => e.vars.intersect(prodBefore)
    case Noop => Set()
    case Seq(a@Asg(x,e),u1) =>  getIntermediateUpd(a,prodBefore) ++ getIntermediateUpd(u1, prodBefore + x)
    case Seq(u1,u2) =>
      getIntermediateUpd(u1,prodBefore) ++ getIntermediateUpd(u2, prodBefore ++ u1.prod)
    case Par(u1, u2) =>
      throw new RuntimeException("Removal of intermediate variables must be performed on serialized updates")
  }

  /**
    * Removes assignments to variables that are not used later
    * @param u update to look for assignments
    * @param unused unused variables
    * @return an update without unused assignments
    */
  private def rmUnusedUpd(u:Update,unused:Set[Var]):Update = u match {
      case Noop => Noop
      case Asg(x,e) => if (unused.contains(x)) Noop else Asg(x,e)
      case Par(u1,u2) => Par(rmUnusedUpd(u1,unused),rmUnusedUpd(u2,unused))
      case Seq(u1,u2) => Seq(rmUnusedUpd(u1,unused),rmUnusedUpd(u2,unused))
  }

  def serialize: HubAutomata = {
    var ntrans:Trans = Set()
    for ((from,(to,p,g,u,es)) <- trans)
      ntrans += ((from,(to,p,g,u.instance,es)))

    HubAutomata(ports,init,ntrans)
  }

  /** List transitions in a pretty-print. */
  //TODO: show updates and guards as well
  def show: String =
    s"$init:\n"+trans.map(x=>s" - ${x._1}->${x._2._1} "+
      s"${x._2._2.toList.sorted.mkString("[",",","]")} "+
      s"${x._2._5.toList.map(_.prim.name).sorted.mkString("(",",",")")}").mkString("\n")

  def smallShow: String = {
    trans.flatMap(_._2._5).toList.map(_.prim.name).sorted.mkString("Aut(",",",")")
  }

}

object HubAutomata {

  //TODO: add guard to Trans
  // from -> (target, ports, guards, update, originalEdge)
  type Trans = Set[(Int,(Int,Set[Int],Guard, Update,Set[Edge]))]


//  def mkVar(start:String, ports:List[Int]):String = ports.mkString(start,"_","")

  implicit object HubAutomataBuilder extends AutomataBuilder[HubAutomata] {

    //TODO: use a parser to write updates and guards in a simpler way
    /** Given an edge between two nodes (ports), builds a primitive automata for the connector in its edge.
      * Only recognises primitive connectors.
      *
      * @param e    edge with primitive and ports
      * @param seed current counter used to generate state names
      * @return new HubAutomata and updated counter for state names
      */
    def buildAutomata(e: Edge, seed: Int): (HubAutomata, Int) = e match {
      case Edge(CPrim("sync",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Edge(CPrim("id",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Edge(CPrim("port",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Edge(CPrim("event",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1, Set(seed - 1 -> (seed, Set(a), Ltrue, Noop, Set(e)), seed -> (seed - 1, Set(b), Ltrue, Noop, Set(e)))), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Edge(CPrim("dataEvent",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1
          , Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Edge(CPrim("fifo",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      //    case Edge(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
      //      (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Edge(CPrim("drain",_,_,_), List(a, b), List(), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, Noop, Set(e)))), seed + 1)
      case Edge(CPrim("merger",_,_,_), List(a, b), List(c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, c), Ltrue, c.toString := a.toString, Set(e)),
            seed -> (seed, Set(b, c), Ltrue, c.toString := b.toString, Set(e))))
          , seed + 1)
      case Edge(CPrim("dupl",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, b, c), Ltrue, (b.toString := a.toString) & (c.toString := a.toString), Set(e))))
          , seed + 1)
      case Edge(CPrim("xor",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, b), Ltrue, (b.toString := a.toString) , Set(e))
            ,   seed -> (seed, Set(a,c), Ltrue, (c.toString := a.toString), Set(e))))
          , seed + 1)
      case Edge(CPrim("node",_,_,extra), List(a), List(b, c), _) if extra contains("dupl") =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, b, c), Ltrue, (b.toString := a.toString) & (c.toString := a.toString), Set(e))))
          , seed + 1)
      // if we use onetooneSimple we need to add support for nodes
      case Edge(CPrim("node",_,_,extra), ins, outs, _) if extra contains("dupl") =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, seed
          , for (xi <- i) yield
             seed -> (seed, i++o, Ltrue, (for (xo <- o) yield xo.toString := xi.toString).fold[Update](Noop)(_ & _) , Set(e)))
          , seed + 1)
      case Edge(CPrim("node",_,_,extra), ins, outs, _)  =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, seed
          , for (xi <- i; xo <- o) yield
            seed -> (seed, i++o, Ltrue,  xo.toString := xi.toString , Set(e)))
          , seed + 1)
      case Edge(CPrim("resource",_,_,_), List(a,b), List(), _) =>
        (HubAutomata(Set(a,b), seed - 1
          , Set(seed - 1  -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e))
          ,     seed -> (seed -1, Set(b), Pred("=", List("bf", b.toString)),Noop, Set(e))))
          , seed +2)
      case Edge(CPrim("blackboard",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b), seed -1
          , Set(seed -1 -> (seed, Set(a), Ltrue, ("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), ("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))))
          , seed + 2)
      case Edge(CPrim("semaphore",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b)
          , seed
          , Set(seed -> (seed, Set(a), Pred("<", List("c", "MAXINT")), "c" := Fun("+", List("c", Val(1))), Set(e)),
            seed -> (seed, Set(b), Pred(">", List("c", Val(1))), "c" := Fun("-", List("c", Val(1))), Set(e))))
          , seed + 1)
      //    case Edge(CPrim("writer", _, _, _), List(), List(a),_) =>
      //      (HubAutomata(Set(a), seed, Set(seed -> (seed, Set(a), Set(e)))), seed + 1)
      //    case Edge(CPrim("reader", _, _, _), List(a), List(),_) =>
      //      (HubAutomata(Set(a), seed, Set(seed -> (seed, Set(a), Set(e)))), seed + 1)
      //    case Edge(CPrim("noSnk", _, _, _), List(), List(a),_) =>
      //      (HubAutomata(Set(a), seed, Set()), seed + 1)
      //    case Edge(CPrim("noSrc", _, _, _), List(a), List(),_) =>
      //      (HubAutomata(Set(a), seed, Set()), seed + 1)

        /////// FULL //////
      case Edge(CPrim("eventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Ltrue, Noop, Set(e)), seed -> (seed - 1, Set(b), Ltrue, Noop, Set(e)))), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Edge(CPrim("dataEventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed
          , Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Edge(CPrim("fifoFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      //    case Edge(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
      //      (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Edge(CPrim("blackboardFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b), seed
          , Set(seed -1 -> (seed, Set(a), Ltrue, ("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), ("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))))
          , seed + 2)


      // unknown name with type 1->1 -- behave as identity
      case Edge(name, List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)


      case Edge(p, _, _, _) =>
        throw new RuntimeException(s"Unknown hub automata for primitive $p")

    }

    def emptyAutomata = HubAutomata(Set(), 0, Set())

    /**
      * Automata composition - combining every possible transition,
      * and including transitions that can occur in parallel.
      *
      * @param a1   automata to be composed
      * @param a2   automata to be composed
      * @param hide whether to hide internal actions while composing
      * @return composed automata
      */
    def join(a1: HubAutomata, a2: HubAutomata): HubAutomata = join(a1, a2, true, 20000)

    def join(a1: HubAutomata, a2: HubAutomata, hide: Boolean, timeout: Int): HubAutomata = {
      //     println(s"combining ${this.show}\nwith ${other.show}")
      var seed = 0
      var steps = timeout
      val shared = a1.ports.intersect(a2.ports)
      var restrans = Set[(Int, (Int, Set[Int], Guard, Update, Set[Edge]))]()
      var newStates = Map[(Int, Int), Int]()

      def mkState(i1: Int, i2: Int) = if (newStates.contains((i1, i2)))
        newStates((i1, i2))
      else {
        seed += 1
        newStates += (i1, i2) -> seed
        seed
      }

      def tick(): Unit = {
        steps -= 1
        if (steps == 0) throw new
            RuntimeException(s"Timeout when composing automata:\n - ${a1.smallShow}\n - ${a2.smallShow}")
      }

      def ok(toFire: Set[Int]): Boolean = {
        tick()
        toFire.intersect(shared).isEmpty
      }

      def ok2(toFire1: Set[Int], toFire2: Set[Int]): Boolean = {
        tick()
        toFire1.intersect(a2.ports) == toFire2.intersect(a1.ports)
      }

      // rename internal vars if necessary
      // external variables not necesary because they have the same name (corresponding to the rename after syncronization)
      var sharedVars = a1.getInternalVarsNames.intersect(a2.getInternalVarsNames)
      var newVars = Map[(String,Int), String]()
      var varSeed = 0


      def mkVar(v: String,aut:Int): String = {
        if (sharedVars contains v)
          if (newVars.contains((v,aut)))
            newVars((v,aut))
          else {
            varSeed += 1
            newVars += ((v,aut) -> s"$v$varSeed")
            v + s"$varSeed"
          }
        else v
      }

      def mkGuard(g: Guard,aut:Int): Guard =
        if (g.vars.map(_.name).intersect(sharedVars).isEmpty) g
        else remapGuard(g,aut)

      def shareVars(v: Set[Var]): Boolean = v.map(_.name).intersect(sharedVars).nonEmpty

      def remapGuard(g: Guard,aut:Int): Guard = g match {
        case Ltrue => Ltrue
        case LNot(g1) => LNot(remapGuard(g,aut))
        case Pred(n, param) => Pred(n, remapParam(param,aut))
        case LOr(g1, g2) => LOr(remapGuard(g1,aut), remapGuard(g2,aut))
        case LAnd(g1, g2) => LAnd(remapGuard(g1,aut), remapGuard(g2,aut))
      }

      def remapUpd(u: Update,aut:Int): Update = u match {
        case Noop => Noop
        case Asg(x, e) => Asg(Var(mkVar(x.name,aut)), remapExpr(e,aut))
        case Par(u1, u2) => Par(remapUpd(u1,aut), remapUpd(u2,aut))
        case Seq(u1, u2) => Seq(remapUpd(u1,aut), remapUpd(u2,aut))
      }

      def remapParam(param: List[Expr],aut:Int): List[Expr] = {
        param.map(remapExpr(_,aut))
      }

      def remapExpr(e: Expr,aut:Int): Expr = e match {
        case Var(n, v) => Var(mkVar(n,aut), v)
        case n@Val(v) => n
        case Fun(n, args) => Fun(n, remapParam(args,aut))
      }

      // hide internal actions if desired
      def mkPorts(fire: Set[Int]): Set[Int] = if (hide) fire -- shared else fire

      // just 1
      for ((from1, (to1, fire1, g1, u1, es1)) <- a1.trans; p2 <- a2.getStates)
        if (ok(fire1))
          restrans += mkState(from1, p2) -> (mkState(to1, p2), fire1, remapGuard(g1,1), remapUpd(u1,1), es1)
      // just 2
      for ((from2, (to2, fire2, g2, u2, es2)) <- a2.trans; p1 <- a1.getStates)
        if (ok(fire2))
          restrans += mkState(p1, from2) -> (mkState(p1, to2), fire2, remapGuard(g2,2), remapUpd(u2,2), es2)
      // communication
      for ((from1, (to1, fire1, g1, u1, es1)) <- a1.trans; (from2, (to2, fire2, g2, u2, es2)) <- a2.trans) {
        if (ok2(fire1, fire2))
          restrans += mkState(from1, from2) ->
            (mkState(to1, to2), mkPorts(fire1 ++ fire2), remapGuard(g1,1) && remapGuard(g2,2), remapUpd(u1,1) | remapUpd(u2,2), es1 ++ es2)
      }
      // println(s"ports: $newStates")
      val res1 = HubAutomata(mkPorts(a1.ports ++ a2.ports), mkState(a1.init, a2.init), restrans)
      //    println(s"got ${a.show}")
      val res2 = res1.cleanup
      //    println(s"cleaned ${a2.show}")
      //      println(s"${res2.smallShow} -> ${timeout-steps}\n===${res2.show}")
      res2
    }
  }

}