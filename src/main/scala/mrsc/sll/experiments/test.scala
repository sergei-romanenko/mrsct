package mrsc.sll.experiments
import mrsc._
import mrsc.sll._

object test {

  def main(args: Array[String]): Unit = {
    val t3 = SLLTasks.namedTasks("gOr(gEven(x), gOdd(x))")
    val e = SLLParsers.parseExpr("gEven(S(n))")
    val spec = new Speculator(t3.program)
    val res = spec.speculate(e)
    println(res)
  }

}