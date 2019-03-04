package hub.lang

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 04/03/2019
  */


object Parser extends RegexParsers{

  def parsePattern(p:String):ParseResult[List[String]] = parseAll(pattern,p)


  val portName:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { _.toString }
  val num:Parser[Int] = """[0-9]*""".r ^^ {_.toInt}

  def pattern:Parser[List[String]] =
    portName~","~pattern ^^ {case p~_~list => p::list} |
    portName~"^"~num~","~pattern ^^ {case p~_~num~_~list => List.tabulate(num)(_=>p)++list} |
    portName~"^"~num ^^ {case p~_~num => List.tabulate(num)(_=>p)}|
    portName ^^ {List(_)}



}
