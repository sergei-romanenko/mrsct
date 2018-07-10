package mrsc.pfp

import scala.language.implicitConversions

package object sll {
  implicit def text2Program(s: String): Program = SLLParsers parseProg s

  implicit def text2Expr(s: String): Expr = SLLParsers parseExpr s
}
