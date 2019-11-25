package hub

import hub.HubAutomata.{Trans, Valuation}
import hub.DSL._
import hub.backend.{Show, Simplify}
import hub.common.GenerationException
import preo.ast.CPrim
import preo.backend.Network.{Prim}
import preo.backend.{Automata, AutomataBuilder}

import scala.collection.mutable


/**
  * Representation of an automata, aimed at being generated from a [[preo.backend.Network]].
 *
  * @param ports Represent the possible labels (actions)
  * @param init  Initial state
  * @param trans Transitions - Relation between input and output states, with associated
  *              sets of actions and of edges (as in [[Prim]]).
  */
case class HubAutomata(ports:Set[Int],sts:Set[Int],init:Int,trans:Trans,
                       clocks:Set[String],inv:Map[Int,CCons],initVal:Valuation,
                       taskPort:(Set[Int],Set[Int])=(Set(),Set())) extends Automata {

  private var inSeed = 0
  private var outSeed = 0

//  private var portName:Map[Int,String] = Map()
  private lazy val portName:Map[Int,String] =
    (getInputs++getOutputs).map(p => p -> mkPortName(p)).toMap


  /** Collects all states, seen as integers */
  def getStates: Set[Int] = (for((x,(y,_,_,_,_,_,_)) <- trans) yield Set(x,y)).flatten + init
  // states: ints, transitions: maps from states to (new state,ports fired, primitives involved)

  /** Returns the initial state */
  def getInit: Int = init

  /**
    * Returns the transitions to be displayed
    * New name for transition actions, and associated variables
    * - if action belongs to a predefine hub (e.g., node, port, sem, etc.)
    *   -> then use custom name inX or outX
    * - if action belong to a user name (e.g. put1, get, etc.)
    *   -> then use the name of the connector to which the action belongs
    *
    * @param fullName
    * @return
    */
  override def getTrans(fullName:Boolean = false): Set[(Int,Any,String,Int)] = {
    for ((from, (to, fire, g,cc, cr, upd, es)) <- trans)
      yield (
        from
        , s"${Show(Simplify(if(fullName) renamePortsInGuard(g) else g))}~"
        + (Simplify(cc) match {
            case CTrue => ""
            case const => Show(const)}) + "~"
        + {if (fullName)
            fire.map(p => getPortName(p)+portDir(p)).mkString(".")
          else
            es.map(getName(_, fire)).foldRight[Set[String]](Set())(cleanDir).mkString(".")
          }
        + s"~${Show(Simplify(if (fullName) renamePortsInUpd(upd) else upd))}~"
        + s"${cr.map(c => s"$c := 0").mkString(",")}~" +
          "§" + fire.mkString("§")
        , (g, cc,fire,upd,cr,es).hashCode().toString
        , to)
  }

  /**
    * Returns the set of transitions
    * @return
    */
  def getTransitions: Set[(Int,Int,Set[Int],CCons,Set[String],Guard,Update)] =
    for ((from, (to, fire, g,cc, cr, upd, es)) <- trans) yield
      (from,to,fire,Simplify(cc),cr,g,upd)


  /* Return the set of input ports */
  def getInputs: Set[Int] = (for((_,(_,_,_,_,_,_,edges)) <- trans) yield edges.flatMap(_.ins)).flatten intersect ports

  /* Return the set of output ports */
  def getOutputs: Set[Int] = (for((_,(_,_,_,_,_,_,edges)) <- trans) yield edges.flatMap(_.outs)).flatten intersect ports


  /* Return the set of internal variable names */
  def getInternalVarsNames: Set[String] =
    (for((_,(_,_,g,_,_,u,_)) <- trans) yield g.vars ++ u.vars).flatten.map(_.name) --
      (getInputs ++ getOutputs).map(_.toString)

  /* Return the set of all variables */
  def getVars:Set[Var] = (for((_,(_,_,g,_,_,u,_)) <- trans) yield g.vars ++ u.vars).flatten

  def getInternalVars:Set[Var] = {
    val globalVars: Set[String] = (this.getOutputs ++ this.getInputs).map(_.toString)
    (for ((_, (_, _, g,_ , _,u, _)) <- trans) yield g.vars ++ u.vars).flatten.filterNot(v => globalVars.contains(v.name))
  }

  // number of states, a list of (varTypeName, sizeOfvarTypeName in number of bytes), number of real value clocks
  //  Memory = (Int,List[(String,Int)],Int)
  //todo: proper memory class perhaps when vars are of different types.
  def memory:(Int,List[(String,Int)],Int) = {
    val stSize = getStates.size
    val varSize = getInternalVars.toList.map(v => (v.value.getClass.toString, 32))//(32 bit integers) for now all variables are of type int
    (stSize,varSize,this.clocks.size)
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
    *
    * @param p
    * @return
    */
  private def mkPortName(p:Int):String = {
    var name = ""
    var t = trans.find(t => t._2._2.contains(p)) //get a transition with p
    if (t.nonEmpty) {
      // NOT true: get the reo edge to which p belongs (there is only one because it is an in or out)
      // find all reo edges to which p belongs,
      // if it is an interface then there is only one, but if it is a port from a task it can appear in two prims
      var e = t.get._2._7.filter(e => (e.outs ++ e.ins).contains(p))
      var extraInfo:Set[String] = e.flatMap(p=> p.prim.extra.filter(e => e.isInstanceOf[String])).map(e => e.asInstanceOf[String])
      var portName:String = extraInfo.find(e => e.endsWith("!") || e.endsWith("?")) match {
        case Some(s) => s
        case _ => ""
      }
      if (portName!="")
        name = portName.split(" ").tail.head.dropRight(1)
      else {
        //if (HubAutomata.PRIMITIVE.contains(e.get.prim.name)) // if it is primitive, return general name inX or outX
        if ((DSL.primitiveConnectors ++ DSL.hubs).contains(e.head.prim.name))
          name = getPortIndexedName(p)
        else
          name = e.head.prim.name // if it is user define name, use it
      }
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
    if (taskPort._1.contains(p)) {
      inSeed+=1
      s"in${if (getInputs.size >1) inSeed else "" }"
    } else
    if (taskPort._2.contains(p)) {
      outSeed+=1
      s"out${if (getOutputs.size >1) outSeed else "" }"
    } else
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
    if (taskPort._1.contains(p))
      "↓"
    else if (taskPort._2.contains(p))
      "↑"
    else if (getInputs.contains(p))
      "↓"
    else if (getOutputs.contains(p))
      "↑"
    else ""

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
    var nsts:Set[Int] = Set(init)
    while (missing.nonEmpty) {
      val next = missing.head
      missing = missing.tail
      done += next
      for (t@(from, (to,_,_,_,_,_, _)) <- trans if from == next) {
        ntrans += t
        nsts += to
        if (!(done contains to)) missing += to
      }
    }
    var nInv = inv.filter(i => nsts.contains(i._1))
    HubAutomata(ports, nsts, init, ntrans, clocks,nInv, initVal,taskPort)
  }

  // TODO: maybe move these methods to Simplify?
  /**
    * Simplify transitions by cleaning updates (removing unused and intermediate variables)
    * @return automata without intermediate nor unused variables
    */
  def simplify: HubAutomata = {

    // all variables produced in transitions of the hub
    val allProd: Set[Var] = this.trans.flatMap(t => t._2._6.prod)
    // all variables dependencies in transitions of the hub
    var allDep: Set[Var] = this.trans.flatMap(t => t._2._6.dep ++ t._2._3.vars)
    // variable dependencies in transitions only based on assignments and exclude uppercase vars (assume they are constants)
    var allDepAsg: Set[Var] = this.trans.flatMap(t => t._2._6.dep)//.filterNot(v => v.name.matches("[A-Z]*"))

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
//      println(s"nonAsigned vars: ${nonAsgVars}")
      simplifiedTrans = rmDepNotAsg(nonAsgVars,simplifiedTrans)
      var nProd = simplifiedTrans.flatMap(t => t._2._6.prod)
      var nDep = simplifiedTrans.flatMap(t => t._2._6.dep) //.filterNot(v => v.name.matches("[A-Z]*")) //++ t._2._3.vars)
      nonAsgVars = (nDep -- nProd).intersect(internalVars)
    }

    // get all intermediate variables
    for (t@(from, (to, p, g,cc,cr, u, es)) <- simplifiedTrans) {
      var intermediateOfU =  getIntermediateUpd(u,Set())
      intermediateVars ++=  intermediateOfU -- areDependencies
      areDependencies ++= u.dep -- intermediateOfU
    }

//    println("Intermediate variables: \n"+ intermediateVars.mkString(","))
    var ntrans: Trans = Set()
    for (t@(from,(to,p,g,cc,cr,u,es)) <- simplifiedTrans) {
      // remove unused variables of u
      var cleanUpd = if (u.vars.intersect(unusedVars).isEmpty) u else rmUnusedUpd(u,unusedVars)
      // remove intermediate variables of u
//      println("General Intermediates vs here: \n")
//      println(intermediateVars )
      var intermediateOfT = getIntermediateUpd(u,Set())
//      println(intermediateVars)
//      println(intermediateOfT)
//      println("About to clean update: \n" + cleanUpd)
//      println("Intersects with intermediate? : \n" + cleanUpd.vars.intersect(intermediateVars))
      //cleanUpd = if (cleanUpd.vars.intersect(intermediateVars).isEmpty) cleanUpd else rmIntermediateUpd(cleanUpd,intermediateVars)
      cleanUpd = if (cleanUpd.vars.intersect(intermediateVars.intersect(intermediateOfT)).isEmpty) cleanUpd else rmIntermediateUpd(cleanUpd,intermediateVars.intersect(intermediateOfT))
//      println("New Update: \n"+cleanUpd)
      ntrans += ((from,(to,p,Simplify(g),Simplify(cc),cr,Simplify(cleanUpd),es)))
    }

    val nInitVal = initVal -- unusedVars
    val ninv = inv.map(i=> i._1->Simplify(i._2))

    HubAutomata(ports,sts,init,ntrans,clocks,ninv, nInitVal,taskPort)
  }


  /**
    * Generate transition without assignments that depend only on non-assigned internal vars
    * @param nonAsg
    * @return
    */
  private def rmDepNotAsg(nonAsg:Set[Var],t:Trans):Trans  = {
    var ntrans:Trans = Set()
    for (t@(from,(to,p,g,cc,cr,u,es)) <- t) {
      ntrans += ((from,(to,p,g,cc,cr,rmNotAsgUpd(u,nonAsg),es)))
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
      case Asg(x,e) => if (unused.contains(x) && !(x.name.startsWith("_"))) Noop else Asg(x,e)
      case Par(u1,u2) => Par(rmUnusedUpd(u1,unused),rmUnusedUpd(u2,unused))
      case Seq(u1,u2) => Seq(rmUnusedUpd(u1,unused),rmUnusedUpd(u2,unused))
  }

  def serialize: HubAutomata = {
    var ntrans:Trans = Set()
    for ((from,(to,p,g,cc,cr,u,es)) <- trans)
      ntrans += ((from,(to,p,g,cc,cr,u.instance,es)))

    HubAutomata(ports,sts,init,ntrans,clocks,inv,initVal,taskPort)
  }

  // todo: check if it needs update after incorporating clock constraints
  def wherePortsAre: Map[(Int,Prim),(Guard,Boolean,Int)] = {
    // SHOULD map each port/edge to: (source state to: condition to fire + canBeSingle)
    // in the end flatten conditions (with AND) and canBeSingle (with AND) and number of ports of source
    val res = mutable.Map[(Int,Prim),Map[Int,(Guard,Boolean)]]()
    for ((from,(to,ps,g,cc,cr,u,es)) <- trans) {
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
    s"$init - ${ports.mkString(",")}:\n"+
      s"InTask:${taskPort._1.map(p=>getPortName(p)).mkString(",")}\n"+
      s"OutTask:${taskPort._2.map(p=>getPortName(p)).mkString(",")}\n"+trans.map(x=>s" - ${x._1}->${x._2._1} "+
      s"${x._2._2.toList.sorted.mkString("[",",","]")} "+
      s"${x._2._7.toList.map(x=>x.prim.name + (x.ins++x.outs).mkString("{",",","}")).sorted.mkString("(",",",")")}").mkString("\n")

  def smallShow: String = {
    //trans.flatMap(_._2._5).toList.map(_.toString).sorted.mkString("Aut(",",",")")
    trans.flatMap(_._2._7).toList.map(p => s"${p.prim.name}${if(p.prim.extra.nonEmpty) "/"+p.prim.extra.mkString("/") else ""}" ).sorted.mkString("Aut(",",",")")
  }

}

object HubAutomata {

//  val PRIMITIVE = Set("semaphore","resource", "port","dataEvent","event","fifo","blackboard","node","dupl","dupls"
//    ,"xor","xors","mrg","drain","timer","nbtimer","writer","reader","nbreader")


  // from -> (target, ports, guards, clock constraints, clock resets, update, originalEdge)
  type Trans = Set[(Int,(Int,Set[Int],Guard,CCons,Set[String],Update,Set[Prim]))]

  type Valuation = Map[Var,Expr]

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
      // if prim has ports with same name (selfloop) then return an emtpy automaton
      case Prim(CPrim(_, _, _, _), ins, outs,_) if (ins++outs).groupBy(p=>p).exists(g=>g._2.size>1) => (emptyAutomata,seed)
      case Prim(CPrim("sync",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed), seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue, Set(), b.toString := a.toString, Set(e))),Set(),Map(),Map()), seed + 1)
      case Prim(CPrim("id",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed), seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue,Set(), b.toString := a.toString, Set(e))),Set(),Map(),Map()), seed + 1)
      case Prim(CPrim("port",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed), seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue,Set(), b.toString := a.toString, Set(e))),Set(),Map(),Map()), seed + 1)
      case Prim(CPrim("lossy",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), Set(seed), seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue,Set(), b.toString := a.toString, Set(e))
                                         , seed -> (seed, Set(a) , Ltrue, CTrue,Set(), Noop, Set(e))),Set(),Map(),Map()), seed + 1)
      case Prim(CPrim("event",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed,seed-1), seed - 1, Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue,Set(), Noop, Set(e)),
                                              seed -> (seed - 1, Set(b), Ltrue, CTrue, Set(),Noop, Set(e))),Set(),Map(),Map()), seed + 2)
      case Prim(CPrim("pevent",_,_,extra), List(a), List(b), _) =>
        val extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        val period:Option[Int] = extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) => Some(s.drop(7).toInt)
          case _ => None}
        val inv:Map[Int,CCons] = if (period.isDefined) Map((seed)->LE("p",CInt(period.get))) else  Map()
        val clocks:Set[String] = if (period.isDefined) Set("p") else Set()
        (HubAutomata(Set(a, b),Set(seed,seed-1), seed - 1, Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue,Set(), Noop, Set(e)),
          seed -> (seed - 1, Set(b), Ltrue, CTrue, Set(),Noop, Set(e))),clocks,inv,Map()), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Prim(CPrim("dataEvent",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), Set(seed,seed-1),seed - 1
          , Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue,Set(), "bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, CTrue, Set(),"bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, CTrue,Set(), b.toString := "bf", Set(e)))
          , Set(),Map()
          , Map(Var("bf")->Val(0)))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Prim(CPrim("fifo",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), Set(seed,seed-1),seed - 1,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set(),"bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, CTrue,Set(), b.toString := "bf", Set(e)))
          , Set(),Map(),Map(Var("bf")->Val(0)))
          , seed + 2)
      //    case Edge(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
      //      (HubAutomata(Set(a, b), seed, Set(seed - 1 -> (seed, Set(a), Set(e)), seed -> (seed - 1, Set(b), Set(e)))), seed + 2)
      case Prim(CPrim("drain",_,_,_), List(a, b), List(), _) =>
        (HubAutomata(Set(a, b),Set(seed), seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue,Set(), Noop, Set(e))),Set(),Map(),Map()), seed + 1)
      case Prim(CPrim("merger",_,_,_), List(a, b), List(c), _) =>
        (HubAutomata(Set(a, b, c), Set(seed),seed
          , Set(seed -> (seed, Set(a, c), Ltrue, CTrue,Set(), c.toString := a.toString, Set(e)),
            seed -> (seed, Set(b, c), Ltrue, CTrue, Set(),c.toString := b.toString, Set(e)))
          , Set(),Map(),Map())
          , seed + 1)
      case Prim(CPrim("dupl",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c),Set(seed), seed
          , Set(seed -> (seed, Set(a, b, c), Ltrue, CTrue, Set(),(b.toString := a.toString) & (c.toString := a.toString), Set(e)))
          , Set(),Map(),Map())
          , seed + 1)
      case Prim(CPrim("xor",_,_,_), List(a), List(b, c), _) =>
        (HubAutomata(Set(a, b, c),Set(seed), seed
          , Set(seed -> (seed, Set(a, b), Ltrue, CTrue, Set(),(b.toString := a.toString) , Set(e))
            ,   seed -> (seed, Set(a,c), Ltrue, CTrue,Set(), (c.toString := a.toString), Set(e)))
          , Set(),Map(),Map())
          , seed + 1)
//      case Edge(CPrim("node",_,_,extra), List(a), List(b, c), _) if extra contains("dupl") =>
//        (HubAutomata(Set(a, b, c), seed
//          , Set(seed -> (seed, Set(a, b, c), Ltrue, (b.toString := a.toString) & (c.toString := a.toString), Set(e))))
//          , seed + 1)
      // if we use onetooneSimple we need to add support for nodes
      case Prim(CPrim("node",_,_,extra), _, _, _) if extra.intersect(Set("vdupl","vmrg","vxor")).nonEmpty =>
        throw new GenerationException(s"Connector with variable parts ${extra.mkString(",")}, to be interpreted only as an IFTA instance")
      case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra contains("dupl") =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, Set(seed),seed
          , for (xi <- i) yield
            seed -> (seed, o+xi, Ltrue, CTrue, Set[String](), (for (xo <- o) yield xo.toString := xi.toString).fold[Update](Noop)(_ & _) , Set(e))
          , Set(),Map(),Map())
          , seed + 1)
      case Prim(CPrim("node",_,_,extra), ins, outs, _) =>
        val i = ins.toSet
        val o = outs.toSet
        (HubAutomata(i ++ o, Set(seed),seed
          , for (xi <- i; xo <- o) yield
            seed -> (seed, Set(xi,xo), Ltrue, CTrue, Set[String](), xo.toString := xi.toString , Set(e))
          , Set(),Map(),Map())
          , seed + 1)
      case Prim(CPrim("resource",_,_,_), List(a,b), List(), _) =>
        (HubAutomata(Set(a,b), Set(seed,seed-1),seed - 1
          , Set(seed - 1  -> (seed, Set(a), Ltrue, CTrue, Set(), "bf" := a.toString, Set(e))
          ,     seed -> (seed -1, Set(b), Pred("=", List("bf", b.toString)), CTrue,Set(),Noop, Set(e)))
          , Set(),Map(),Map(Var("bf")->Val(0)))
          , seed +2)
      case Prim(CPrim("blackboard",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b),Set(seed,seed-1), seed -1
          , Set(seed -1 -> (seed, Set(a), Ltrue, CTrue, Set(),("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), CTrue, Set(),("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, CTrue, Set(),b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), CTrue,Set(), Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), CTrue,Set(), Noop, Set(e)))
          , Set(),Map(),Map(Var("bf")->Val(0),Var("u")->Val(0)))
          , seed + 2)
      case Prim(CPrim("semaphore",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b)
          , Set(seed),seed
          , Set(seed -> (seed, Set(a), Pred("<", List("c", "MAXINT")), CTrue,Set(), "c" := Fun("+", List("c", Val(1))), Set(e)),
                seed -> (seed, Set(b), Pred(">", List("c", Val(1))), CTrue, Set(),"c" := Fun("-", List("c", Val(1))), Set(e)))
          , Set(),Map(),Map(Var("c")->Val(0)))
          , seed + 1)
          case Prim(CPrim("writer", _, _, extra), List(), List(a),_) =>
            val extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
            val expr:Expr = extraInfo.find(e => e.startsWith("writes:")) match {
              case Some(s) => Val(s.drop(7).toInt)
              case None => Cons("*")
            }
            extraInfo.find(e => e.startsWith("period:")) match {
              case Some(p) =>
                val period:Int = p.drop(7).toInt
                (HubAutomata(Set(a),
                  Set(seed,seed-1), seed-1,
                  Set(seed-1 -> (seed, Set(a),Ltrue, CTrue,Set(),a.toString := expr, Set(e))
                    , seed -> (seed-1, Set(),Ltrue, ET("p",CInt(period)),Set("p"),Noop, Set(e)))
                  , Set("p"),Map((seed)->LE("p",CInt(period)),(seed-1)->LE("p",CInt(period))),Map(),
                  if(extra.contains("T")) (Set(),Set(a)) else (Set(),Set())), seed + 2)
              case None =>
                (HubAutomata(Set(a),
                  Set(seed), seed, Set(seed -> (seed, Set(a),Ltrue, CTrue,Set(),a.toString := expr, Set(e))),
                  Set(),Map(),Map(),if(extra.contains("T")) (Set(),Set(a)) else (Set(),Set())), seed + 1)
            }
          case Prim(CPrim("reader", _, _, extra), List(a), List(),_) =>
            val extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
            extraInfo.find(e => e.startsWith("period:")) match {
              case Some(p) =>
                val period:Int = p.drop(7).toInt
                (HubAutomata(Set(a),
                  Set(seed,seed-1),seed-1,
                  Set(seed-1 -> (seed, Set(a), Ltrue, CTrue,Set(),"_bf":= a.toString, Set(e))
                  , seed -> (seed-1, Set(), Ltrue, ET("p",CInt(period)),Set("p"),Noop, Set(e))),
                  Set("p"),Map((seed)->LE("p",CInt(period)),(seed-1)->LE("p",CInt(period)))
                  ,Map(),if(extra.contains("T")) (Set(a),Set()) else (Set(),Set())), seed + 2)
              case _ => None
                (HubAutomata(Set(a),
                  Set(seed),seed,
                  Set(seed -> (seed, Set(a), Ltrue, CTrue,Set(),"_bf":= a.toString, Set(e))),
                  Set(),Map(),Map(),if(extra.contains("T")) (Set(a),Set()) else (Set(),Set())), seed + 1)
            }
          case Prim(CPrim("nbreader", _, _, extra), List(a), List(),_) =>
            var extraInfo:List[String] = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
            var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
              case Some(s) => s.drop(3).toInt
              case _ => 0}
            val period:Option[Int] = extraInfo.find(e => e.startsWith("period:")) match {
              case Some(s) => Some(s.drop(7).toInt)
              case _ => None}
            val inv:Map[Int,CCons] = if (period.isDefined) Map(seed-> LE("cl",CInt(to)),(seed-1)->LE("p",CInt(period.get))) else  Map(seed->LE("cl",CInt(to)))
            val clocks = if (period.isDefined) Set("cl","p") else Set("cl")
            (HubAutomata(Set(a), Set(seed,seed-1),seed - 1,
              Set(seed - 1 -> (seed, Set(), Ltrue, if(period.isDefined) ET("p",CInt(period.get)) else CTrue, clocks, Noop, Set(e)),
                seed -> (seed - 1, Set(a), Ltrue, LE("cl",CInt(to)),Set(), "_bf":= a.toString, Set(e)),
                seed -> (seed - 1, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
              , clocks,inv,Map(),if(extra.contains("T")) (Set(a),Set()) else (Set(),Set()))
              , seed + 2)
          case Prim(CPrim("nbtimer", _, _, extra), List(a), List(b),_) =>
            var extraInfo:List[String] = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
            var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
              case Some(s) => s.drop(3).toInt
              case _ => 0}
            val period:Option[Int] =  extraInfo.find(e => e.startsWith("period:")) match {
              case Some(s) => Some(s.drop(7).toInt)
              case _ => None}
            val inv = if (period.isDefined) Map(seed->LE("cl",CInt(to)),(seed-1)->LE("p",CInt(period.get)))else  Map(seed->LE("cl",CInt(to)))
            val clocks = if (period.isDefined) Set("cl","p") else Set("cl")
            (HubAutomata(Set(a, b), Set(seed,seed-1),seed - 1,
              Set(seed - 1 -> (seed, Set(a), Ltrue, if(period.isDefined) ET("p",CInt(period.get)) else CTrue, clocks, "bf":= a.toString, Set(e)),
                seed -> (seed - 1, Set(b), Ltrue, LE("cl",CInt(to)),Set(), b.toString := "bf", Set(e)),
                seed -> (seed - 1, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
              , clocks,inv,Map(),(Set(),Set(b)))
              , seed + 2)
          case Prim(CPrim("await", _, _, extra), List(a,b), List(),_) =>
            var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
            var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
              case Some(s) => s.drop(3).toInt
              case _ => 0}
            //            var portName:String = extraInfo.find(e => e.endsWith("!|?")) match {
            //              case Some(s) =>
            //              case _ => a.toString()
            //            }
            (HubAutomata(Set(a,b), Set(seed,seed-1),seed - 1,
              Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set("cl"), Noop, Set(e)),
                seed -> (seed - 1, Set(b), Ltrue, LE("cl",CInt(to)),Set(), "bf":= b.toString, Set(e)),
                seed -> (seed - 1, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
              , Set("cl"),Map(seed->LE("cl",CInt(to))),Map())
              , seed + 2)
          case Prim(CPrim("noSnk", _, _, _), List(), List(a),_) =>
            (HubAutomata(Set(a), Set(seed),seed, Set(),Set(),Map(),Map()), seed + 1)
          case Prim(CPrim("noSrc", _, _, _), List(a), List(),_) =>
            (HubAutomata(Set(a), Set(seed),seed, Set(),Set(),Map(),Map()), seed + 1)

        /////// FULL //////
      case Prim(CPrim("eventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed,seed-1), seed, Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue,Set(), Noop, Set(e))
                                    , seed -> (seed - 1, Set(b), Ltrue, CTrue, Set(),Noop, Set(e))), Set(),Map(),Map()), seed + 2)
      //For now it doesn't have input Clear. //TODO: add Clear input if desirable
      case Prim(CPrim("dataEventFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b),Set(seed,seed-1), seed
          , Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set(),"bf" := a.toString, Set(e)),
            seed -> (seed, Set(a), Ltrue, CTrue,Set(), "bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, CTrue,Set(), b.toString := "bf", Set(e))),Set(),Map(),Map(Var("bf")->Val(0)))
          , seed + 2)
      // for now asumues fifo1 TODO: add support for receiving Size of fifo
      case Prim(CPrim("fifofull",x,y,z), a, b, c) =>
        buildAutomata(Prim(CPrim("fifoFull",x,y,z), a, b, c),seed)
      case Prim(CPrim("fifoFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b), Set(seed,seed-1),seed,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set(),"bf" := a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, CTrue,Set(), b.toString := "bf", Set(e)))
          , Set(),Map(),Map(Var("bf")->Val(0)))
          , seed + 2)
      case Prim(CPrim("blackboardFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a,b), Set(seed,seed-1), seed
          , Set(seed -1 -> (seed, Set(a), Ltrue, CTrue, Set(),("bf" := a.toString) & ("u" := Fun("+",List("u",Val(1)))) , Set(e))
            , seed -> (seed, Set(a), Pred("!=", List(a.toString, "CLR")), CTrue, Set(),("bf" := a.toString) & ("u" := Fun("mod",List(Fun("+",List("u",Val(1))),"MAXINT"))), Set(e))
            , seed -> (seed, Set(b), Ltrue, CTrue,Set(), b.toString := Fun("join",List("bf", "u")),Set(e))
            , seed -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), CTrue, Set(),Noop, Set(e))
            , seed-1 -> (seed -1, Set(a), Pred("=", List(a.toString, "CLR")), CTrue,Set(), Noop, Set(e)))
          , Set(),Map(),Map(Var("bf")->Val(0),Var("u")->Val(0)))
          , seed + 2)
      case Prim(CPrim("semaphoreFull",_,_,_), List(a), List(b), _) =>
        (HubAutomata(Set(a, b)
          , Set(seed),seed
          , Set(seed -> (seed, Set(a), Pred("<", List("c", "MAXINT")), CTrue,Set(), "c" := Fun("+", List("c", Val(1))), Set(e)),
            seed -> (seed, Set(b), Pred(">", List("c", Val(1))), CTrue, Set(),"c" := Fun("-", List("c", Val(1))), Set(e)))
          , Set(),Map(),Map(Var("c")->Val(1)))
          , seed + 1)

      case Prim(CPrim("timer", _, _, extra), List(a), List(b),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        (HubAutomata(Set(a, b), Set(seed,seed-1),seed - 1,
          //          Set(("bfP" := a.toString) & ("c" := Fun("+",List(Var("c"),Val(1)))) & ("p" := Fun("mod",List(Fun("+",List(Var("p"),Val(1))),Var("N"))))), Set(e)),
          Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set("cl"), "bf":= a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, ET("cl",CInt(to)),Set(), b.toString := "bf", Set(e)))
          , Set("cl"),Map(seed->LE("cl",CInt(to))),Map())
          , seed + 2)

      case Prim(CPrim("psync", _, _, extra), List(a), List(b),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var period:Int =  extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) => s.drop(7).toInt
          case _ => 0}
        (HubAutomata(Set(a, b), Set(seed),seed,
          Set(seed -> (seed, Set(a,b), Ltrue, ET("p",CInt(period)), Set("p"), b.toString := a.toString, Set(e)))
          , Set("p"),Map((seed)->LE("p",CInt(period))),Map())
          , seed + 1)
      case Prim(CPrim("timeout", _, _, extra), List(a), List(b),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        (HubAutomata(Set(a, b), Set(seed,seed-1),seed - 1,
          Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set("cl"), "bf":= a.toString, Set(e)),
            seed -> (seed - 1, Set(b), Ltrue, LE("cl",CInt(to)),Set(), b.toString := "bf", Set(e)),
            seed -> (seed - 1, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
          , Set("cl"),Map(seed->LE("cl",CInt(to))),Map())
          , seed + 2)
      case Prim(CPrim("putNB", _, _, extra), List(a), List(err,ok),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        (HubAutomata(Set(a,err,ok), Set(seed,seed-1),seed - 1,
          Set(seed - 1 -> (seed, Set(a), Ltrue, CTrue, Set("cl"), "bf":= a.toString, Set(e)),
            seed -> (seed - 1, Set(ok), Ltrue, LE("cl",CInt(to)),Set(), ok.toString := "bf", Set(e)),
            seed -> (seed - 1, Set(err), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
          , Set("cl"),Map(seed->LE("cl",CInt(to))),Map(),(Set(),Set(ok)))
          , seed + 2)
      case Prim(CPrim("getNB", _, _, extra), List(in,go), List(c),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        //            var portName:String = extraInfo.find(e => e.endsWith("!|?")) match {
        //              case Some(s) =>
        //              case _ => a.toString()
        //            }
        (HubAutomata(Set(in,go,c), Set(seed,seed-1),seed - 1,
          Set(seed - 1 -> (seed, Set(go), Ltrue, CTrue, Set("cl"), Noop, Set(e)),
            seed -> (seed - 1, Set(in,c), Ltrue, LE("cl",CInt(to)),Set(), "bf":= in.toString, Set(e)),
            seed -> (seed - 1, Set(c), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
          , Set("cl"),Map(seed->LE("cl",CInt(to))),Map(),(Set(in),Set()))
          , seed + 2)
      case Prim(CPrim("nbget", _, _, extra), List(in,go), List(next),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        val period:Option[Int] = extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) => Some(s.drop(7).toInt)
          case _ => None
        }
        val clocks:Set[String] = if (period.isDefined) Set("cl","p") else Set("cl")
        val inv:Map[Int,CCons] = if(period.isDefined) Map(seed->CAnd(LE("cl",CInt(to)),LE("p",CInt(period.get)))) else  Map(seed->LE("cl",CInt(to)))
        (HubAutomata(Set(in,go,next), Set(seed,seed-1),seed - 1,
          Set(seed - 1 -> (seed, Set(go), Ltrue, CTrue, Set("cl"), Noop, Set(e)),
            seed -> (seed - 1, Set(in,next), Ltrue,CTrue,Set(), "_bf":= in.toString, Set(e)),
            seed -> (seed - 1, Set(next), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
          , clocks,inv,Map(),(Set(in),Set()))
          , seed + 2)
      case Prim(CPrim("nbget", _, _, extra), List(in), List(),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) =>
            val period:Int = s.drop(7).toInt
            (HubAutomata(Set(in), Set(seed,seed-1),seed - 1,
              Set(seed - 1 -> (seed, Set(in), Ltrue, CTrue, Set(), "_bf":= in.toString, Set(e)),
                seed-1 -> (seed, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)),
                seed -> (seed - 1, Set(), Ltrue, ET("p",CInt(period)),Set("p","cl"), Noop, Set(e)))
              , Set("cl"),Map((seed-1)->CAnd(LE("cl",CInt(to)),LE("p",CInt(period))), (seed)-> (LE("p",CInt(period))))
                ,Map(),(Set(in),Set()))
              , seed + 2)
          case None =>
            (HubAutomata(Set(in), Set(seed),seed,
              Set(seed -> (seed, Set(in), Ltrue, CTrue, Set("cl"), "_bf":= in.toString, Set(e)),
                seed -> (seed , Set(), Ltrue, ET("cl",CInt(to)),Set("cl"), Noop, Set(e)))
              , Set("cl"),Map(seed->LE("cl",CInt(to))),Map(),(Set(in),Set()))
              , seed + 1)
        }
      case Prim(CPrim("nbput", _, _, extra), List(go), List(err,out),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        val expr:Expr = extraInfo.find(e => e.startsWith("writes:")) match {
          case Some(s) => Val(s.drop(7).toInt)
          case None => Cons("*")
        }
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        val period:Option[Int] = extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) => Some(s.drop(7).toInt)
          case _ => None
        }
        val clocks:Set[String] = if (period.isDefined) Set("cl","p") else Set("cl")
        val inv:Map[Int,CCons] = if(period.isDefined) Map(seed->CAnd(LE("cl",CInt(to)),LE("p",CInt(period.get)))) else  Map(seed->LE("cl",CInt(to)))
        (HubAutomata(Set(go,err,out), Set(seed,seed-1),seed - 1,
          Set(seed - 1 -> (seed, Set(go), Ltrue, CTrue, Set("cl"), Noop, Set(e)),
            seed -> (seed - 1, Set(out), Ltrue, CTrue,Set(), out.toString := expr, Set(e)),
            seed -> (seed - 1, Set(err), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)))
          , clocks,inv,Map(),(Set(),Set(out)))
          , seed + 2)
      case Prim(CPrim("nbput", _, _, extra), List(), List(out),_) =>
        var extraInfo = extra.toList.filter(e => e.isInstanceOf[String]).map(e => e.asInstanceOf[String])
        val expr:Expr = extraInfo.find(e => e.startsWith("writes:")) match {
          case Some(s) => Val(s.drop(7).toInt)
          case None => Cons("*")
        }
        var to:Int =  extraInfo.find(e => e.startsWith("to:")) match {
          case Some(s) => s.drop(3).toInt
          case _ => 0}
        extraInfo.find(e => e.startsWith("period:")) match {
          case Some(s) =>
            val period:Int = s.drop(7).toInt
            (HubAutomata(Set(out), Set(seed,seed-1),seed - 1,
              Set(seed - 1 -> (seed, Set(out), Ltrue, CTrue, Set(), out.toString:= expr, Set(e)),
                seed-1 -> (seed, Set(), Ltrue, ET("cl",CInt(to)),Set(), Noop, Set(e)),
                seed -> (seed - 1, Set(), Ltrue, ET("p",CInt(period)),Set("p","cl"), Noop, Set(e)))
              , Set("cl"),Map((seed-1)->CAnd(LE("cl",CInt(to)),LE("p",CInt(period))), (seed)-> (LE("p",CInt(period))))
              ,Map(),(Set(out),Set()))
              , seed + 2)
          case None =>
            (HubAutomata(Set(out), Set(seed),seed,
              Set(seed -> (seed, Set(out), Ltrue, CTrue, Set("cl"), out.toString := expr, Set(e)),
                seed -> (seed , Set(), Ltrue, ET("cl",CInt(to)),Set("cl"), Noop, Set(e)))
              , Set("cl"),Map(seed->LE("cl",CInt(to))),Map(),(Set(),Set(out)))
              , seed + 1)
        }
      // unknown name with type 1->1 -- behave as identity
      case Prim(name, List(a), List(b), _) =>
        (HubAutomata(Set(a, b), Set(seed),seed, Set(seed -> (seed, Set(a, b), Ltrue, CTrue,Set(), b.toString := a.toString, Set(e))),Set(),Map(), Map()), seed + 1)


      case Prim(p, _, _, _) =>
        throw new GenerationException(s"Primitive connector ${p.name} not modelled as Hub Automata.")

    }

    def emptyAutomata = HubAutomata(Set(),Set(0), 0, Set(), Set(),Map(),Map())

    /**
      * Automata composition - combining every possible transition,
      * and including transitions that can occur in parallel.
      *
      * @param a1   automata to be composed
      * @param a2   automata to be composed
      * @return composed automata
      */
    def join(a1: HubAutomata, a2: HubAutomata): HubAutomata = join(a1, a2, true, 40000)

    /**
      * Automata composition - combining every possible transition,
      * and including transitions that can occur in parallel.
      *
      * @param a1   automata to be composed
      * @param a2   automata to be composed
      * @param hide whether to hide internal actions while composing
      * @return composed automata
      */
    def join(a1: HubAutomata, a2: HubAutomata, hide: Boolean, timeout: Int): HubAutomata = {
      //println(s"combining ${a1.smallShow}\nwith ${a2.smallShow}")
      //println(s"combining ${a1.show}\nwith ${a2.show}")

      var seed = 0
      var steps = timeout
      val shared = a1.ports.intersect(a2.ports)
      //var restrans = Set[(Int, (Int, Set[Int], Guard, Update, Set[Prim]))]()
      var restrans:Trans = Set()
      var newStates = Map[(Int, Int), Int]()
      // if they share clocks, rename them to avoid conflicts
      var sharedClocks = (a1.clocks intersect a2.clocks) -- Set("p") // remove clock that has to be with periodicity (needed because of W with periodicity)
      var newClocks: Map[(String,Int),String] = Map()
      var clockSeed = 0
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

//      //find ports from tasks
//      //find all transitions that come from prims that came from tasks
//      val prims:Set[Prim] = (a1.trans++a2.trans).filter(t=> t._2._2.intersect(shared).nonEmpty)
//        .flatMap(t=>t._2._7).filter(p=>p.prim.extra.contains("T"))
//      val preserve:Set[Int] = prims.flatMap(p=>p.ins++p.outs)

      val preserve = a1.taskPort._1++a1.taskPort._2++a2.taskPort._1++a2.taskPort._2

      // hide internal actions if desired
      // keep ports that come from tasks
      def mkPorts(fire: Set[Int]): Set[Int] =
        if (hide) fire -- (shared--preserve) else fire

      val newPorts = mkPorts(a1.ports ++ a2.ports)


      // hide internal edges if desired
      def mkEdges(edges: Set[Prim]): Set[Prim] =
        if (hide) edges.filter(e => e.ins.exists(newPorts) || e.outs.exists(newPorts))
        else edges

      /** create new names for shared clock names */
      def getClockName(c:String,aut:Int):String = {
        if (sharedClocks contains c)
          if (newClocks.contains(c,aut))
            newClocks((c,aut))
          else {
            clockSeed += 1
            newClocks += ((c,aut) -> s"$c$clockSeed")
            c + s"$clockSeed"
          }
        else c
      }

      /** rename clocks in clock constraints */
      def renameClocks(cc:CCons,aut:Int):CCons = cc match {
        case CTrue => CTrue
        case CAnd(cc1,cc2) => CAnd(renameClocks(cc1,aut),renameClocks(cc2,aut))
        case ET(c,n) => ET(getClockName(c,aut),renameCExpr(n,aut))
        case GT(c,n) => GT(getClockName(c,aut),renameCExpr(n,aut))
        case GE(c,n) => GE(getClockName(c,aut),renameCExpr(n,aut))
        case LE(c,n) => LE(getClockName(c,aut),renameCExpr(n,aut))
        case LT(c,n) => LT(getClockName(c,aut),renameCExpr(n,aut))
      }

      def renameCExpr(ce:ClockExpr,aut:Int):ClockExpr = ce match {
        case Clock(c) => Clock(getClockName(c,aut))
        case CPlus(c,i) => CPlus(getClockName(c,aut),i)
        case CMinus(c,i) => CMinus(getClockName(c,aut),i)
        case _ => ce
      }


      // just 1
      for ((from1, (to1, fire1, g1, cc1,cr1, u1, es1)) <- a1.trans; p2 <- a2.getStates)
        if (ok(fire1))
          restrans += mkState(from1, p2) -> (mkState(to1, p2), fire1, mkGuard(g1,1), renameClocks(cc1,1), cr1.map(c=>getClockName(c,1)), remapUpd(u1,1), es1)
      // just 2
      for ((from2, (to2, fire2, g2, cc2, cr2,u2, es2)) <- a2.trans; p1 <- a1.getStates)
        if (ok(fire2))
          restrans += mkState(p1, from2) -> (mkState(p1, to2), fire2, mkGuard(g2,2), renameClocks(cc2,2),cr2.map(c=>getClockName(c,2)), remapUpd(u2,2), es2)
      // communication
      for ((from1, (to1, fire1, g1, cc1, cr1,u1, es1)) <- a1.trans; (from2, (to2, fire2, g2, cc2,cr2, u2, es2)) <- a2.trans) {
        if (ok2(fire1, fire2))
          //println(s"!! merging ${fire1.mkString("/")} and ${fire2.mkString("/")} -> ${mkPorts(fire1++fire2)}")
          //println(s"!! merging ${es1.mkString("/")} and ${es2.mkString("/")} -> ${mkEdges(es1++es2)}")
          restrans += mkState(from1, from2) ->
            (mkState(to1, to2), mkPorts(fire1 ++ fire2)
              , remapGuard(g1, 1) && remapGuard(g2, 2)
              , CAnd(renameClocks(cc1,1), renameClocks(cc2,2))
              , cr1.map(c=> getClockName(c,1))++cr2.map(c=> getClockName(c,2))
              , remapUpd(u1, 1) | remapUpd(u2, 2)
              , mkEdges(es1 ++ es2))
            (mkState(to1, to2), mkPorts(fire1 ++ fire2), mkGuard(g1,1) && mkGuard(g2,2), remapUpd(u1,1) | remapUpd(u2,2), es1 ++ es2)

      }
      // (mkState(to1, to2), mkPorts(fire1 ++ fire2), remapGuard(g1,1) && remapGuard(g2,2), remapUpd(u1,1) | remapUpd(u2,2), es1 ++ es2)
      // println(s"ports: $newStates")


      val resInvariant:Map[Int,CCons] = (for (l1<-a1.sts; l2<-a2.sts)
        yield mkState(l1, l2) -> CAnd(renameClocks(a1.inv.withDefaultValue(CTrue)(l1),1),
          renameClocks(a2.inv.withDefaultValue(CTrue)(l2),2))).toMap

//      println("Ports to preserve a1:"+(a1.taskPort._1++a1.taskPort._2).map(p=> p+"="+a1.getPortName(p)).mkString(","))
//      println("Ports to preserve a2:"+(a2.taskPort._1++a2.taskPort._2).map(p=> p+"="+a2.getPortName(p)).mkString(","))
      val res1 = HubAutomata(newPorts
        , for (l1<-a1.sts; l2<-a2.sts) yield mkState(l1, l2)
        , mkState(a1.init, a2.init)
        , restrans
        , a1.clocks.map(c=>getClockName(c,1))++a2.clocks.map(c=>getClockName(c,2))
        , resInvariant
        , a1.initVal ++ a2.initVal,
        (a1.taskPort._1++a2.taskPort._1,a1.taskPort._2++a2.taskPort._2))

      //    println(s"got ${a.show}")
      val res2 = res1.cleanup
//      println("return clocks: "+ res2.clocks)
      res2
    }
  }

}