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

    // create call and map it to the corresponding temporal formula
    (for ((tf,ufs,um) <- f2uf) yield {
      tf -> VerifytaCall(um,ufs)
    }).toMap
  }

  /**
    * Match the responses from multiple calls to verifyta
    *
    * @param responses appended responses from multiple call
    * @param calls call made for each call response
    * @return a map from a temporal formula to either:
    *         an error from verifyta, or
    *         the list of responses for each formula in the call
    */
  def matchResults(responses:String,calls:List[VerifytaCall]):Map[String,Either[String,List[String]]] = {
    // get groups of calls to verifyta (formulas, response)
    val f2res:List[(String,String)] = parseMultipleVerifyta(responses)
    // map tf to responses from its corresponding uppaal formulas (or errors)
    f2res.map(f=> f._1 -> parseVerifytaResponse(f._2)).toMap
  }

}


/**
  * Necesary information for a verifyta call
  * Each call corresponds to a model and a list of formulas to verify for that model
  *
  * @param um
  * @param uf
  * @param tf2uf
  */
case class VerifytaCall(  um:Set[Uppaal], uf: List[UppaalFormula]) {}