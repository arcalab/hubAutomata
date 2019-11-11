package hub

import hub.analyse.TemporalFormula

/**
  * Created by guillecledou on 2019-10-24
  */


object Utils {

  /**
    * Parsed the response from calling verifyta possible more than once
    * int a formula and the response for such formula
    * @param verifytas
    * @return
    */
  def parseMultipleVerifyta(verifytas:String):List[(String,String)] = {
    // seperate response into groups of calls to verifyta (separator '§')
    val groups:List[String] = verifytas
      .replaceAll("\\[2K","")
      .split("§").toList

    val res = groups.map(g=> g.split("~").toList).map(l=>(l.head,l.last))

//    // each group has the list of formulas used and the result from verifyta.
//    // separate groups into List[formulas,res]
//    val fs2res:List[List[String]] =
//    groups.map(g=> g.split("~").toList)
//
//    // match formula with response
//    val res:Map[String,String] =
//      fs2res.flatMap(g=>
//        g.head.split("\n").toList
//          .zip(
//            g.last.stripMargin
//              .split("Verifying formula ([0-9]*) at \\/tmp\\/uppaal([0-9]*)_([0-9]*)\\.q:[0-9]*")
//            .filterNot(_.matches("\\x1B"))))
//        .toMap

    res
  }

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

  def isSatisfiedVerifyta(result:String):Boolean = !result.split(" ").contains("NOT")

//  def toVerifyta(formulas:List[TemporalFormula]):



}
