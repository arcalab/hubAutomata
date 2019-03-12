package hub

import hub.HubAutomata.Trans
import hub.DSL._
import hub.backend.{Show, Simplify}
import preo.ast.CPrim
import preo.backend.Network.Prim
import preo.backend.{Automata, AutomataBuilder, PortAutomata}

import scala.collection.JavaConverters._
import scala.collection.mutable


//case class Edge(prim: String, ins:List[Int], outs:List[Int], parents:List[String])
//case class ReoGraph(edges:List[Edge], ins:List[Int], outs:List[Int]) {
//  def ++(other:ReoGraph) = ReoGraph(edges++other.edges,ins++other.ins,outs++other.outs)
//}


/**
  * Representation of an automata, aimed at being generated from a [[preo.backend.Network]].
 *
  * @param ports Represent the possible labels (actions)
  * @param init  Initial state
  * @param trans Transitions - Relation between input and output states, with associated
  *              sets of actions and of edges (as in [[Prim]]).
  */
case class HubAutomata(ports:Set[Int],init:Int,trans:Trans) extends Automata {

  private var inSeed = 0
  private var outSeed = 0

//  private var portName:Map[Int,String] = Map()
  private lazy val portName:Map[Int,String] =
    (getInputs++getOutputs).map(p => p -> mkPortName(p)).toMap


  /** Collects all states, seen as integers */
  def getStates: Set[Int] = (for((x,(y,_,_,_,_)) <- trans) yield Set(x,y)).flatten + init
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  def getInit: Int = init


  /** Returns the transitions to be displayed */
//  override def getTrans(fullName:Boolean = false): Set[(Int, Any,String,Int)] = {
//    for ((from, (to, fire, g, upd, es)) <- trans) {
//      println(s"Transition with ports: ${fire}")
//      println("reo edges associated: ")
//      es.map(e => println(s" $e \n  - primname: ${e.prim.name}  ins: ${e.ins}; outs: ${e.outs}"))
//    }
//    for ((from, (to, fire, g, upd, es)) <- trans)
//      yield (
//        from
//        , s"${Show(Simplify(if(fullName) mkPortInGuard(g) else g))}~"
//          + s"${if (fullName)
//              cleanFullNameDir(es.flatMap(getFullName(_, fire))).mkString(".")
//            else
//              es.map(getName(_, fire)).foldRight[Set[String]](Set())(cleanDir).mkString(".")}"
//          + s"~${Show(Simplify(if (fullName) mkPortInUpd(upd) else upd))}"
//        , (g, fire, upd,es).hashCode().toString
//        , to)
//  }

  /** Returns the transitions to be displayed */
  // New name for transition actions, and associated variables
  // idea:
  // - if action belongs to a predefine hub (e.g., node, port, sem, etc.) -> then use custom name inX or outX
  // - if action belong to a user name (e.g. put1, get, etc.) - then use the name of the connector to which the action belongs
  override def getTrans(fullName:Boolean = false): Set[(Int,Any,String,Int)] = {
    for ((from, (to, fire, g, upd, es)) <- trans)
      yield (
        from
        , s"${Show(Simplify(if(fullName) renamePortsInGuard(g) else g))}~"
        + {if (fullName)
//        cleanFullNameDir(es.flatMap(getFullName(_, fire))).mkString(".")
            fire.map(p => getPortName(p)+portDir(p)).mkString(".")
          else
            es.map(getName(_, fire)).foldRight[Set[String]](Set())(cleanDir).mkString(".")
          }
        + s"~${Show(Simplify(if (fullName) renamePortsInUpd(upd) else upd))}"
        , (g, fire, upd,es).hashCode().toString
        , to)
  }

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
    val globalVars: Set[String] = (this.getOutputs ++ this.getInputs).map(_.toString)
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


  /**
    * Returns the name of an interface port
    * @param p
    * @return
    */
   def getPortName(p:Int):String = {
//    portName.getOrElse(p,mkPortName(p))
     if ((getInputs++getOutputs).contains(p))
        portName.getOrElse(p,p.toString)
     else
       throw new RuntimeException(s"Unknown port, ${p}, for this automaton")
  }

  /**
    * Returns the name of an interface port
    * if the port is associated to an edge that is named by the user (e.g., put1), it uses such a name
    * otherwise it uses a name a generic name inX for inputs and outX for outputs,
    * where X is an index identifying different ins and outs
    * @param p
    * @return
    */
  private def mkPortName(p:Int):String = {
    var name = ""
    var t = trans.find(t => t._2._2.contains(p)) //get a transition with p
    if (t.nonEmpty) {
      // get the reo edge to which p belongs (there is only one because it is an in or out)
      var e = t.get._2._5.find(e => (e.outs ++ e.ins).contains(p))

      if (HubAutomata.PRIMITIVE.contains(e.get.prim.name)) // if it is primitive, return general name inX or outX
        name = getPortIndexedName(p)
      else
        name = e.get.prim.name // if it is user define name, use it

//      portName += (p -> name)
    }
    name
  }

  /**
    * Creates a unique name for an interface port
    * inX or outX dependin if p is an input or an output,
    * X is a unique seed to identify different ins and outs
    * @param p
    * @return
    */
  private def getPortIndexedName(p:Int):String = {
    if (getInputs.contains(p)){
      inSeed+=1
      s"in${if (getInputs.size >1) inSeed else "" }"
    } else
    if (getOutputs.contains(p)) {
      outSeed+=1
      s"out${if (getOutputs.size >1) outSeed else "" }"
    } else ""
  }

  /**
    * Returns the direction of an interface port,
    * Since it is an interface port, it is either an input or an output
    * @param p
    * @return
    */
  private def portDir(p:Int):String =
    if (getInputs.contains(p))
      "↓"
    else if (getOutputs.contains(p))
      "↑"
    else ""

//  private def mkPortName(p:Int, edges:Set[Edge]):String = {
//    val e = edges.find(e => (e.ins++e.outs).contains(p))
//    var name = ""
//    if (VirtuosoParser.PRIMITIVE.contains(e.get.prim.name))
//      name = getPortIndexedName(p)
//    else
//      name = e.get.prim.name
//    portName += (p -> name)
//    name
//  }

//  private def getFullName(edge:Edge,fire:Set[Int]): Set[String] = {
//    fire.map(p =>
//      if (edge.ins.contains(p) || edge.outs.contains(p))
//        getPortIndexedName(p) + getFullNameDir(edge,p) else "")
//  }
//
//  private def getFullNameDir(edge:Edge, p:Int):String = {
//    if (edge.ins.contains(p))
//      "↓"
//    else if (edge.outs.contains(p))
//      "↑"
//    else ""
//  }
//
//  private def cleanFullNameDir(fire:Set[String]):Set[String] ={
//    var groupByName:Map[String,Set[String]] = fire.groupBy(s => if (s.nonEmpty) s.init else s )
//
//    def clean(p:String, occurences:Set[String]):String =
//      if (occurences.size >1) p+"↕" else occurences.head
//
//    groupByName.map(p => clean(p._1,p._2)).toSet
//  }

//  private def getUpdName(u:Update):Update = u match {
//    case Asg(x, e) =>
//  }

  /**
    * Rename variables that belong to ports in expresions
    * @param e
    * @return
    */
  private def renamePortsInExp(e:Expr):Expr = e match {
    case Var(n,v) if (getInputs++getOutputs).map(_.toString()).contains(n) => Var(getPortName(n.toInt),v)//Var(getPortIndexedName(n.toInt),v)
    case Fun(n,args) => Fun(n,args.map(p=> renamePortsInExp(p)))
    case e1 => e1
  }

  /**
    * Rename variables that belong to ports in updates
    * @param u
    * @return
    */
  private def renamePortsInUpd(u:Update):Update = u match {
    case Asg(x, e) =>
      Asg(if ((getInputs++getOutputs).map(_.toString()).contains(x.name)) Var(getPortName(x.name.toInt),x.value) //Var(getPortIndexedName(x.name.toInt),x.value)
          else x
        , renamePortsInExp(e))
    case Seq(u1, u2) => Seq(renamePortsInUpd(u1),renamePortsInUpd(u2))
    case Par(u1, u2) => Par(renamePortsInUpd(u1),renamePortsInUpd(u2))
    case u1 => u1
  }

  /**
    * Rename variables that belong to ports in guards
    * @param g
    * @return
    */
  private def renamePortsInGuard(g:Guard):Guard = g match {
    case Pred(n, p) => Pred(n, p.map(renamePortsInExp(_)))
    case LAnd(g1, g2) => LAnd(renamePortsInGuard(g1),renamePortsInGuard(g2))
    case LOr(g1, g2) => LOr(renamePortsInGuard(g1),renamePortsInGuard(g2))
    case LNot(g1) => LNot(renamePortsInGuard(g1))
    case Ltrue => Ltrue
  }

  private def getName2(edge: Prim, fire:Set[Int]):String =
    s"${edge.prim}-${edge.parents.mkString("/")}-${fire.mkString(":")}"

  private def getName(edge: Prim, fire:Set[Int]):String =
    if (fire.intersect((edge.ins ++ edge.outs).toSet).nonEmpty) {
      (edge.parents match {
        case Nil     => edge.prim.name
        case ""::_   => edge.prim.name
        case head::_ => head
      }) + getDir(edge,fire)
    } else ""  //+
  //  s"[${edge.ins.toSet.intersect(fire).mkString("|")}->${edge.outs.toSet.intersect(fire).mkString("|")}]"
  //  fire.mkString("|")
  private def getDir(edge: Prim, fire:Set[Int]): String = {
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

  private def printPrim(edge: Prim):String = {
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
    println(this)
    // all variables produced in transitions of the hub
    val allProd: Set[Var] = this.trans.flatMap(t => t._2._4.prod)
    // all variables dependencies in transitions of the hub
    var allDep: Set[Var] = this.trans.flatMap(t => t._2._4.dep ++ t._2._3.vars)
    // variable dependencies in transitions only based on assignments and exclude uppercase vars (assume they are constants)
    var allDepAsg: Set[Var] = this.trans.flatMap(t => t._2._4.dep)//.filterNot(v => v.name.matches("[A-Z]*"))

    var internalVars:Set[Var] = this.getInternalVars
    // assigned variables that are never used
    var unusedVars:Set[Var] = internalVars -- allProd.intersect(allDep)
    // variables that are used as dependencies but never assigned
    var nonAsgVars:Set[Var] = (allDepAsg -- allProd).intersect(internalVars)

    // variables that in some transition appear only on the right-hand side (act as dependencies)
    var areDependencies:Set[Var] = Set()
    // variables that always act as intermediate
    var intermediateVars:Set[Var] = Set()

    //first remove assignements that depend only on nonAsgVars
    var simplifiedTrans:Trans = this.trans
    while (nonAsgVars.nonEmpty) {
      println(s"nonAsigned vars: ${nonAsgVars}")
      simplifiedTrans = rmDepNotAsg(nonAsgVars,simplifiedTrans)
      var nProd = simplifiedTrans.flatMap(t => t._2._4.prod)
      var nDep = simplifiedTrans.flatMap(t => t._2._4.dep) //.filterNot(v => v.name.matches("[A-Z]*")) //++ t._2._3.vars)
      nonAsgVars = (nDep -- nProd).intersect(internalVars)
    }

    // get all intermediate variables
    for (t@(from, (to, p, g, u, es)) <- simplifiedTrans) {
      var intermediateOfU =  getIntermediateUpd(u,Set())
      intermediateVars ++=  intermediateOfU -- areDependencies
      areDependencies ++= u.dep -- intermediateOfU
    }

    var ntrans: Trans = Set()
    for (t@(from,(to,p,g,u,es)) <- simplifiedTrans) {
      // remove unused variables of u
      var cleanUpd = if (u.vars.intersect(unusedVars).isEmpty) u else rmUnusedUpd(u,unusedVars)
      // remove intermediate variables of u
      cleanUpd = if (cleanUpd.vars.intersect(intermediateVars).isEmpty) cleanUpd else rmIntermediateUpd(cleanUpd,intermediateVars)
      ntrans += ((from,(to,p,g,Simplify(cleanUpd),es)))
    }
    HubAutomata(ports,init,ntrans)
  }


  /**
    * Generate transition without assignments that depend only on non-assigned internal vars
    * @param nonAsg
    * @return
    */
  private def rmDepNotAsg(nonAsg:Set[Var],t:Trans):Trans  = {
    var ntrans:Trans = Set()
    for (t@(from,(to,p,g,u,es)) <- t) {
      ntrans += ((from,(to,p,g,rmNotAsgUpd(u,nonAsg),es)))
    }
    ntrans
  }

  /**
    * Remove all asignments that depend only on non-assigned internal variables
    * @param u
    * @param nonAsg
    * @return
    */
  private def rmNotAsgUpd(u:Update,nonAsg:Set[Var]):Update = u match {
    case a@Asg(x,e) => if (e.vars.intersect(nonAsg) == e.vars) Noop else a
    case Noop => Noop
    case Par(u1,u2) => Par(rmNotAsgUpd(u1,nonAsg),rmNotAsgUpd(u2,nonAsg))
    case Seq(u1,u2) => Seq(rmNotAsgUpd(u1,nonAsg),rmNotAsgUpd(u2,nonAsg))
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
      case c@Cons(n,v) => c
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

  def wherePortsAre: Map[(Int,Prim),(Guard,Boolean,Int)] = {
    // SHOULD map each port/edge to: (source state to: condition to fire + canBeSingle)
    // in the end flatten conditions (with AND) and canBeSingle (with AND) and number of ports of source
    val res = mutable.Map[(Int,Prim),Map[Int,(Guard,Boolean)]]()
    for ((from,(to,ps,g,u,es)) <- trans) {
      val canBeSingle = ps.size == 1 // only 1 ports
      //println(s"--- can be single? [${ps.mkString(",")}] - $canBeSingle")
      for (e <- es) { // for all edges in some transitions
        val ports = ps intersect (e.ins.toSet ++ e.outs.toSet) // look at transition ports that belong to the edge
        for (p <- ports) {
          val key = (p, e)
          res += key -> (res.get(key) match {
            case None => Map(from -> (g, canBeSingle))
            case Some(srcToSt) => srcToSt.get(from) match {
              case None => srcToSt + (from -> (g, canBeSingle))
              case Some((g2, canBeSingle2)) => srcToSt + (from -> (g && g2, canBeSingle || canBeSingle2)) //g||g2
            }
          })
        }
      }
    }
    res
      .mapValues(src => (src,src.size))
      .mapValues(ss => (ss._1.foldLeft[(Guard,Boolean)]((Ltrue,true))((prev,next) => (prev._1 && next._2._1 , prev._2 && next._2._2)),ss._2))
      .mapValues(x => (Simplify(x._1._1),x._1._2,x._2))
      .toMap
  }

  /** List transitions in a pretty-print. */
  //TODO: show updates and guards as well
  def show: String =
    s"$init - ${ports.mkString(",")}:\n"+trans.map(x=>s" - ${x._1}->${x._2._1} "+
      s"${x._2._2.toList.sorted.mkString("[",",","]")} "+
      s"${x._2._5.toList.map(x=>x.prim.name + (x.ins++x.outs).mkString("{",",","}")).sorted.mkString("(",",",")")}").mkString("\n")

  def smallShow: String = {
    //trans.flatMap(_._2._5).toList.map(_.toString).sorted.mkString("Aut(",",",")")
    trans.flatMap(_._2._5).toList.map(p => s"${p.prim.name}${if(p.prim.extra.nonEmpty) "/"+p.prim.extra.mkString("/") else ""}" ).sorted.mkString("Aut(",",",")")
  }

}

object HubAutomata {

  val PRIMITIVE = Set(
    "semaphore"
    , "resource"
    , "port"
    , "dataEvent"
    , "event"
    , "fifo"
    , "blackboard"
    , "node"
    , "dupl"
    , "dupls"
    , "xor"
    , "xors"
    , "mrg"
    , "drain"
  )

  //TODO: add guard to Trans
  // from -> (target, ports, guards, update, originalEdge)
  type Trans = Set[(Int,(Int,Set[Int],Guard, Update,Set[Prim]))]


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
    def buildAutomata(e: Prim, seed: Int): (HubAutomata, Int) = e match {
      case Prim(CPrim("sync",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Prim(CPrim("id",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Prim(CPrim("port",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)
      case Prim(CPrim("event",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1, Set(seed - 1 -> (seed, Set(a), Ltrue, Noop, Set(e)), seed -> (seed - 1, Set(b), Ltrue, Noop, Set(e)))), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Prim(CPrim("dataEvent",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1
          , Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Prim(CPrim("fifo",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed - 1,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      //    case Edge(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
      //      (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Prim(CPrim("drain",_,_,_), List(a, b), List(), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, Noop, Set(e)))), seed + 1)
      case Prim(CPrim("merger",_,_,_), List(a, b), List(c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, c), Ltrue, c.toString := a.toString, Set(e)),
            seed -> (seed, Set(b, c), Ltrue, c.toString := b.toString, Set(e))))
          , seed + 1)
      case Prim(CPrim("dupl",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, b, c), Ltrue, (b.toString := a.toString) & (c.toString := a.toString), Set(e))))
          , seed + 1)
      case Prim(CPrim("xor",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c), seed
          , Set(seed -> (seed, Set(a, b), Ltrue, (b.toString := a.toString) , Set(e))
            ,   seed -> (seed, Set(a,c), Ltrue, (c.toString := a.toString), Set(e))))
          , seed + 1)
//      case Edge(CPrim("node",_,_,extra), List(a), List(b, c), _) if extra contains("dupl") =>
//        (HubAutomata(Set(a, b, c), seed
//          , Set(seed -> (seed, Set(a, b, c), Ltrue, (b.toString := a.toString) & (c.toString := a.toString), Set(e))))
//          , seed + 1)
      // if we use onetooneSimple we need to add support for nodes
      case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra contains("dupl") =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, seed
          , for (xi <- i) yield
            seed -> (seed, o+xi, Ltrue, (for (xo <- o) yield xo.toString := xi.toString).fold[Update](Noop)(_ & _) , Set(e)))
          , seed + 1)
      case Prim(CPrim("node",_,_,extra), ins, outs, _)  =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, seed
          , for (xi <- i; xo <- o) yield
            seed -> (seed, Set(xi,xo), Ltrue,  xo.toString := xi.toString , Set(e)))
          , seed + 1)
      case Prim(CPrim("resource",_,_,_), List(a,b), List(), _) =>
        (HubAutomata(Set(a,b), seed - 1
          , Set(seed - 1  -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e))
          ,     seed -> (seed -1, Set(b), Pred("=", List("bf", b.toString)),Noop, Set(e))))
          , seed +2)
      case Prim(CPrim("blackboard",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b), seed -1
          , Set(seed -1 -> (seed, Set(a), Ltrue, ("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), ("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))))
          , seed + 2)
      case Prim(CPrim("semaphore",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b)
          , seed
          , Set(seed -> (seed, Set(a), Pred("<", List("c", "MAXINT")), "c" := Fun("+", List("c", Val(1))), Set(e)),
            seed -> (seed, Set(b), Pred(">", List("c", Val(1))), "c" := Fun("-", List("c", Val(1))), Set(e))))
          , seed + 1)
          case Prim(CPrim("writer", _, _, _), List(), List(a),_) =>
            (HubAutomata(Set(a), seed, Set(seed -> (seed, Set(a),Ltrue,Noop, Set(e)))), seed + 1)
          case Prim(CPrim("reader", _, _, _), List(a), List(),_) =>
            (HubAutomata(Set(a), seed, Set(seed -> (seed, Set(a), Ltrue,Noop, Set(e)))), seed + 1)
          case Prim(CPrim("noSnk", _, _, _), List(), List(a),_) =>
            (HubAutomata(Set(a), seed, Set()), seed + 1)
          case Prim(CPrim("noSrc", _, _, _), List(a), List(),_) =>
            (HubAutomata(Set(a), seed, Set()), seed + 1)

        /////// FULL //////
      case Prim(CPrim("eventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Ltrue, Noop, Set(e)), seed -> (seed - 1, Set(b), Ltrue, Noop, Set(e)))), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Prim(CPrim("dataEventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed
          , Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Prim(CPrim("fifoFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, b.toString := "bf", Set(e))))
          , seed + 2)
      //    case Edge(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
      //      (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Prim(CPrim("blackboardFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b), seed
          , Set(seed -1 -> (seed, Set(a), Ltrue, ("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), ("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), Noop, Set(e))))
          , seed + 2)

      // unknown name with type 1->1 -- behave as identity
      case Prim(name, List(a), List(b), _) =>
        (HubAutomata(Set(a, b), seed, Set(seed -> (seed, Set(a, b), Ltrue, b.toString := a.toString, Set(e)))), seed + 1)


      case Prim(p, _, _, _) =>
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
      //println(s"combining ${a1.smallShow}\nwith ${a2.smallShow}")
      //println(s"combining ${a1.show}\nwith ${a2.show}")

      var seed = 0
      var steps = timeout
      val shared = a1.ports.intersect(a2.ports)
      var restrans = Set[(Int, (Int, Set[Int], Guard, Update, Set[Prim]))]()
      var newStates = Map[(Int, Int), Int]()

      // println(s"combining ${a1.smallShow}\nwith ${a2.smallShow}\nover ${shared}")

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
        case c@Cons(n,v) => c
        case n@Val(v) => n
        case Fun(n, args) => Fun(n, remapParam(args,aut))
      }

      // hide internal actions if desired
      def mkPorts(fire: Set[Int]): Set[Int] = if (hide) fire -- shared else fire
      val newPorts = mkPorts(a1.ports ++ a2.ports)

      // hide internal edges if desired
      def mkEdges(edges: Set[Prim]): Set[Prim] =
        if (hide) edges.filter(e => e.ins.exists(newPorts) || e.outs.exists(newPorts))
        else edges

      // just 1
      for ((from1, (to1, fire1, g1, u1, es1)) <- a1.trans; p2 <- a2.getStates)
        if (ok(fire1))
          restrans += mkState(from1, p2) -> (mkState(to1, p2), fire1, mkGuard(g1,1), remapUpd(u1,1), es1)
      // just 2
      for ((from2, (to2, fire2, g2, u2, es2)) <- a2.trans; p1 <- a1.getStates)
        if (ok(fire2))
          restrans += mkState(p1, from2) -> (mkState(p1, to2), fire2, mkGuard(g2,2), remapUpd(u2,2), es2)
      // communication
      for ((from1, (to1, fire1, g1, u1, es1)) <- a1.trans; (from2, (to2, fire2, g2, u2, es2)) <- a2.trans) {
        if (ok2(fire1, fire2))
          //println(s"!! merging ${fire1.mkString("/")} and ${fire2.mkString("/")} -> ${mkPorts(fire1++fire2)}")
          //println(s"!! merging ${es1.mkString("/")} and ${es2.mkString("/")} -> ${mkEdges(es1++es2)}")
          restrans += mkState(from1, from2) ->
            (mkState(to1, to2), mkPorts(fire1 ++ fire2)
              , remapGuard(g1, 1) && remapGuard(g2, 2)
              , remapUpd(u1, 1) | remapUpd(u2, 2)
              , mkEdges(es1 ++ es2))
            (mkState(to1, to2), mkPorts(fire1 ++ fire2), mkGuard(g1,1) && mkGuard(g2,2), remapUpd(u1,1) | remapUpd(u2,2), es1 ++ es2)

      }
      //            (mkState(to1, to2), mkPorts(fire1 ++ fire2), remapGuard(g1,1) && remapGuard(g2,2), remapUpd(u1,1) | remapUpd(u2,2), es1 ++ es2)
      // println(s"ports: $newStates")
      val res1 = HubAutomata(newPorts, mkState(a1.init, a2.init), restrans)
      //    println(s"got ${a.show}")
      val res2 = res1.cleanup
//      println(res2.show)
      res2
    }
  }

}