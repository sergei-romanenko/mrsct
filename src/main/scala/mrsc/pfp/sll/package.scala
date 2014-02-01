package mrsc.pfp

import scala.language.implicitConversions

package object sll {
  implicit def text2Program(s: String) = SLLParsers parseProg s
  implicit def text2Expr(s: String) = SLLParsers parseExpr s
}