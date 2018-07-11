package mrsc.pfp.sll

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{CharSequenceReader => Reader}
import mrsc.pfp._

sealed trait Expr {
  val size: Int
}

case class Var(name: String) extends Expr {
  val size = 1

  override def toString: String = name
}

case class Ctr(name: String, args: List[Expr]) extends Expr {
  lazy val size: Int = 1 + args.map(_.size).sum

  override def toString: Name = name + args.mkString("(", ", ", ")")
}

case class FCall(name: String, args: List[Expr]) extends Expr {
  lazy val size: Int = 1 + args.map(_.size).sum

  override def toString: Name = name + args.mkString("(", ", ", ")")

}

case class GCall(name: String, args: List[Expr]) extends Expr {
  lazy val size: Int = 1 + args.map(_.size).sum

  override def toString: Name = name + args.mkString("(", ", ", ")")
}

case class Let(term: Expr, bindings: List[(Name, Expr)]) extends Expr {
  lazy val size: Int = 1 + bindings.map(_._2.size).sum

  override def toString: Name =
    "let " + (bindings map { case (x, y) => x + "=" + y } mkString ", ") +
      " in " + term
}

case class Where(e: Expr, defs: List[Def]) extends Expr {
  lazy val size: Int = 1 + defs.map(_.rhs.size).sum

  override def toString: Name =
    " let " + defs.mkString("{", " ", "}") + " in " + e
}

case class Pat(name: String, args: List[Name]) {
  override def toString: Name =
    name + args.mkString("(", ", ", ")")
}

sealed abstract class Def {
  def name: String

  def lhs: Expr

  def rhs: Expr

  override def toString: Name = lhs + " = " + rhs + ";"
}

case class FFun(name: String, args: List[Name], term: Expr) extends Def {
  override val lhs = FCall(name, args map Var)
  override val rhs: Expr = term
}

case class GFun(name: String, p: Pat, args: List[Name], term: Expr) extends Def {
  override val lhs = GCall(name, Ctr(p.name, p.args map Var) :: (args map Var))
  override val rhs: Expr = term
}

case class Program(defs: List[Def]) {
  val f: Map[String, FFun] =
    (defs :\ Map[String, FFun]()) {
      case (x: FFun, m) => m + (x.name -> x)
      case (_, m) => m
    }
  val g: Map[(String, String), GFun] =
    (defs :\ Map[(String, String), GFun]()) {
      case (x: GFun, m) => m + ((x.name, x.p.name) -> x)
      case (_, m) => m
    }
  val gs: Map[String, List[GFun]] =
    (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) {
      case (x: GFun, m) => m + (x.name -> (x :: m(x.name)))
      case (_, m) => m
    }

  override def toString: String = defs.mkString("\n")
}

case class SLLTask(target: Expr, program: Program) {
  override def toString: Name = target + "\n" + program
}

object SLLTask {
  def apply(e: String, p: String): SLLTask =
    SLLTask(SLLParsers.parseExpr(e), SLLParsers.parseProg(p))
}

object SLLParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";")

  def prog: SLLParsers.Parser[List[Def]] = rep1(definition)

  def definition: Parser[Def] = gFun | fFun

  def term: Parser[Expr] = fcall | gcall | ctr | vrb

  def uid: SLLParsers.Parser[String] =
    ident ^? { case id if id.charAt(0).isUpper => id }

  def lid: SLLParsers.Parser[String] =
    ident ^? { case id if id.charAt(0).isLower => id }

  def fid: SLLParsers.Parser[String] =
    ident ^? { case id if id.charAt(0) == 'f' => id }

  def gid: SLLParsers.Parser[String] =
    ident ^? { case id if id.charAt(0) == 'g' => id }

  def vrb: SLLParsers.Parser[Var] =
    lid ^^ Var

  def pat: SLLParsers.Parser[Pat] =
    uid ~ ("(" ~> repsep(lid, ",") <~ ")") ^^ Pat

  def fFun: SLLParsers.Parser[FFun] =
    fid ~ ("(" ~> repsep(lid, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFun

  def gFun: SLLParsers.Parser[GFun] =
    gid ~ ("(" ~> pat) ~ (rep("," ~> lid) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFun

  def ctr: SLLParsers.Parser[Ctr] =
    uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr

  def fcall: SLLParsers.Parser[FCall] =
    fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall

  def gcall: SLLParsers.Parser[GCall] =
    gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall

  def parseProg(s: String) = Program(prog(new lexical.Scanner(new Reader(s))).get)

  def parseExpr(s: String): Expr =
    term(new lexical.Scanner(new Reader(s))).get
}
