package hub.backend

import hub.{HubAutomata, Utils}
import hub.analyse.{TemporalFormula, UppaalFormula}
import preo.backend.Automata

/**
  * Created by guillecledou on 2019-11-11
  */


object Verifyta {

  /**
    * Parsed the response from calling verifyta possible more than once
    * into a formula and the response for such formula
    * @param verifytas
    * @return
    */
  def parseMultipleVerifyta(verifytas:String):List[(String,String)] = {
    // seperate response into groups of calls to verifyta (separator '§')
    val groups:List[String] = verifytas
      .replaceAll("\\[2K","")
      .split("§").toList

    val res = groups.map(g=> g.split("~").toList).map(l=>(l.head,l.last))
    res
  }

  /**
    * Parse the response from a call to verifyta into either an error, or a list of
    * results
    * @param response
    * @return
    */
  def parseVerifytaResponse(response:String):Either[String,List[String]] = {
    if (response.startsWith("error:"))
      Left(response.toString().drop(6))
    else {
      val results = response.toString.drop(3).stripMargin
        .split("Verifying formula ([0-9]*) at \\/tmp\\/uppaal([0-9]*)_([0-9]*)\\.q:[0-9]*")
        .filterNot(_.matches("\\x1B")).toList
      Right(results)
    }
  }

  /**
    * Parses if a formula was satisfied
    * @param result
    * @return
    */
  def isSatisfiedVerifyta(result:String):Boolean = !result.split(" ").contains("NOT")

  /**
    * Parses if a composed formula was satisfied
    * @param results
    * @return
    */
  def isSatisfiedVerifyta(results:List[String]):Boolean =
      results.map(r => Verifyta.isSatisfiedVerifyta(r)).forall(f=>f)


  /**
    * Create a list of verifyta call to verify a list of temporal formulas
    * @param formulas
    * @param hub
    * @return
    */
  def createVerifytaCalls(formulas:List[TemporalFormula],hub:HubAutomata):Map[TemporalFormula,VerifytaCall] = {
    // get a map from port number to shown name (from hub)
    val interfaces:Map[Int,String] = (hub.getInputs ++ hub.getOutputs).map(p=>p->hub.getPortName(p)).toMap

//    // create an uppaal model for simple formulas
//    val ta = Set(Uppaal.mkTimeAutomata(hub))
//
//    // map each formula to a custom network of TA to verify such formula (simple formulas are maped to the same based TA
//    val formulas2nta:List[(TemporalFormula,Set[Uppaal])] =
//      formulas.map(f => if (f.isComplex) (f,Uppaal.fromFormula(f,hub)) else  (f,ta))
    // map each formula to its uppaal model
    val f2model = formulas.map(f=> f -> Uppaal.fromFormula(f,hub))

    // get init location from main uppaal model (hub)
    val f2modelWithInit = f2model.map(f => (f._1,f._2,f._2.find(t=> t.name=="Hub").get.init))

    // map each formula to its expanded formula for uppaal
    val f2uf:List[(TemporalFormula,List[UppaalFormula],Set[Uppaal])] =
      f2modelWithInit.map(f=>(f._1,Uppaal.toUppaalFormula(
        f._1,
        f._2.flatMap(ta=>ta.act2locs.map(a=> interfaces(a._1)->a._2)).toMap, interfaces.map(i => i._2->i._1),f._3),
        f._2))

//    // simplify formulas and convert them to a string suitable for uppaal
//    val formulasStr: List[(TemporalFormula,List[String],String)] =
//      formula2nta2uf.map(f => (f._1,f._2.map(uf => Show(Simplify(uf))),Uppaal(f._3)))

    // group formulas by model
//    val formulasByModel:List[(Set[Uppaal],List[(TemporalFormula,List[UppaalFormula],Set[Uppaal])])] = formula2nta2uf.groupBy(_._3).toList
    //    val orderedFormulasStr:List[(String,List[(TemporalFormula,String,String)])] = formulasStr.groupBy(_._3).toList


    (for ((tf,ufs,um) <- f2uf) yield {
      tf -> VerifytaCall(um,ufs)
    }).toMap
  }

  def matchResults(responses:String,calls:List[VerifytaCall]):Map[String,Either[String,List[String]]] = {
    // get groups of calls to verifyta (formulas, response)
    val f2res:List[(String,String)] = parseMultipleVerifyta(responses)
    // map tf to responses from its corresponding uppaal formulas (or errors)
    f2res.map(f=> f._1 -> parseVerifytaResponse(f._2)).toMap
  }

}


/**
  * Necesary information for a verifyta call
  * Each call corresponds to a model and a list of formulas for such model
  * The call has information of which temporal formula corresponds to which uppaal formulas
  *
  * @param um
  * @param uf
  * @param tf2uf
  */
case class VerifytaCall(um:Set[Uppaal], uf: List[UppaalFormula]) {}