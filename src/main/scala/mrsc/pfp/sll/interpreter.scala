package mrsc.pfp.sll

import scala.annotation.tailrec

import SLLSyntax._

object SLLInterpreter {
  def eval(task: SLLTask): Expr = {
    val int = new SLLInterpreter(task.program)
    int.eval(task.target)
  }
}

private class SLLInterpreter(program: Program)
  extends Reducer {

  type R = Expr

  private def eval(t: Expr): Expr = lazyEval(t) match {
    case Ctr(name, args) => Ctr(name, args.map(eval))
    case x => throw new Exception("Internal Error: lazy eval returns " + x)
  }

  @tailrec
  private def lazyEval(e: Expr): Expr = baseLazyEval(e) match {
    case e1 @ Ctr(_, _) => e1
    case e1 => lazyEval(e1)
  }

  private def baseLazyEval(t: Expr): Expr = decompose(t)

  def caseDecLet(let: Let): Expr =
    throw new Error("unexpected expression: " + let)

  def caseObservableCtr(ctr: Ctr) = ctr

  def caseObservableVar(v: Var) =
    throw new Error("unexpected expression: " + v)

  def caseFRedex(ctx: Ctx, fcall: FCall): Expr = {
    val reduced = subst(
      program.f(fcall.name).term,
      Map(program.f(fcall.name).args.zip(fcall.args): _*))
    ctx(reduced)
  }

  def caseGRedexCtr(ctx: Ctx, gcall: GCall, ctr: Ctr): Expr = {
    val g = program.g(gcall.name, ctr.name)
    val reduced = subst(
      g.term,
      Map((g.p.args ::: g.args) zip (ctr.args ::: gcall.args.tail): _*))
    ctx(reduced)
  }

  def caseGRedexVar(ctx: Ctx, g: GCall, v: Var): Expr =
    throw new Error("unexpected expression: " + v)

}