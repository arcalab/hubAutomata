package hub.lang

import hub._
import hub.analyse._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by guillecledou on 2019-10-05
  */


object TemporalFormulaParser extends RegexParsers {

  def parse(queries:String):ParseResult[List[TemporalFormula]] = parseAll(formulas,queries)

  override def skipWhitespace = true
  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val deadlock:Parser[StFormula] = """deadlock""".r ^^ {_ => Deadlock}
  val int:Parser[String] = """[0-9]+""".r

  def formulas:Parser[List[TemporalFormula]] =
    rep1(formula)

  def formula:Parser[TemporalFormula] =
    "A"~"[]"~>stFormula ^^ AA |
    "A"~"<>"~>stFormula ^^ AE |
    "E"~"[]"~>stFormula ^^ EA |
    "E"~"<>"~>stFormula ^^ EE |
    "every"~identifier~ "-->"~identifier~opt("after"~>int) ^^ {
      case _~a~_~b~Some(t) => EveryAfter(Action(a),Action(b),t.toInt)
      case _~a~_~b~None => Every(Action(a),Action(b))
    } |
    stFormula ~"-->"~ stFormula ^^ {case f1~_~f2 => Eventually(f1,f2)} //|
//    stFormula ~"until"~ stFormula ^^ {case f1~_~f2 => Until(f1,f2)}

  def stFormula:Parser[StFormula] =
    simpleStFormula~opt(boolCond) ^^ {
      case f~Some(cond) => cond(f)
      case f~None => f}

  def simpleStFormula:Parser[StFormula] =
    """nothing""".r ^^ { _ => Nothing} |
    "not"~>parFormula^^ Not |
    //"can"~>parFormula^^ Can |
    parFormula //|
//    singleStFormula

  def parFormula:Parser[StFormula] =
    "("~>stFormula<~")" |
    singleStFormula

  def singleStFormula:Parser[StFormula] =
    deadlock |
//    """nothing""".r ^^ { _ => Nothing} |
    identifier<~".doing"  ^^ DoingAction |
    identifier<~".done"   ^^ DoneAction |
    identifier~"refiresAfterOrAt" ~int  ^^ {case id~mode~t => Refires(Action(id),RAfterOrAt,t.toInt)} |
    identifier~"refiresBeforeOrAt"~int  ^^ {case id~mode~t => Refires(Action(id),RBeforeOrAt,t.toInt)} |
    identifier~"refiresAfter"     ~int  ^^ {case id~mode~t => Refires(Action(id),RAfter,t.toInt)} |
    identifier~"refiresBefore"    ~int  ^^ {case id~mode~t => Refires(Action(id),RBefore,t.toInt)} |
//    identifier~"waits"~waitMode~int ^^ {case id~_~mode~t => Waits(Action(id),mode,t.toInt)} |
    identifier~opt(".t")~intCond ^^ {case id1~t~cond => CGuard(cond(id1+t.getOrElse("")))} |
    identifier ^^ Action

//  def waitMode:Parser[WaitMode] =
//    "atLeast".r ^^ {_ => AtLeast}  |
//    "atMost".r  ^^ {_ => AtMost}  |
//    "moreThan".r ^^ {_ => MoreThan} |
//    "lessThan".r ^^ {_ => LessThan}

  def boolCond:Parser[StFormula => StFormula] =
    "or"~>stFormula ^^ (f => (f1: StFormula) => Or(f1, f)) |
    "and"~>stFormula ^^ {f => (f1:StFormula) => And(f1,f)} |
    "imply"~>stFormula ^^ {f => (f1:StFormula) => Imply(f1,f)} |
    "before"~>stFormula ^^ {f => (f1:StFormula) => Before(f1,f)} |
    "until"~>stFormula ^^ {f => (f1:StFormula) => Until(f1,f)}

  def intCond:Parser[String => CCons] =
    "<="~>clockExpr ^^ {cexpr => (c:String) => LE(c,cexpr)} |
    "<"~>clockExpr  ^^ {cexpr => (c:String) => LT(c,cexpr)} |
    ">="~>clockExpr ^^ {cexpr => (c:String) => GE(c,cexpr)} |
    ">"~>clockExpr  ^^ {cexpr => (c:String) => GT(c,cexpr)} |
    "=="~>clockExpr ^^ {cexpr => (c:String) => ET(c,cexpr)}

  def clockExpr:Parser[ClockExpr] =
    identifier~".t" ~ "\\+|\\-".r ~ int ^^
      {case c~_~op~i => if (op.matches("\\+")) CPlus(c+".t",i.toInt) else CMinus(c+".t",i.toInt)} |
    identifier<~".t" ^^ {c => Clock(c+".t")} |
    int ^^ {i => CInt(i.toInt)}


}
