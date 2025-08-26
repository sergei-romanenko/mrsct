package mrsc.pfp.sll

trait Reducer {

  type R

  def caseDecLet(let: Let): R

  def caseObservableCtr(ctr: Ctr): R

  def caseObservableVar(v: Var): R

  def caseFRedex(ctx: Ctx, f: FCall): R

  def caseGRedexCtr(ctx: Ctx, g: GCall, c: Ctr): R

  def caseGRedexVar(ctx: Ctx, g: GCall, v: Var): R

  type Ctx = Expr => Expr

  def decompose(t: Expr): R = (t: @unchecked) match {
    case l: Let => caseDecLet(l)
    case c: Ctr => caseObservableCtr(c)
    case v: Var => caseObservableVar(v)
    case f: FCall => caseFRedex(t => t, f)
    case g: GCall => decomposeGCall(t => t, g)
  }

  private def decomposeGCall(ctx: Ctx, g: GCall): R =
    (g.args.head: @unchecked) match {
      case c0: Ctr =>
        caseGRedexCtr(ctx, g, c0)
      case v0: Var =>
        caseGRedexVar(ctx, g, v0)
      case f0: FCall =>
        caseFRedex(f1 => ctx(GCall(g.name, f1 :: g.args.tail)), f0)
      case g0: GCall =>
        decomposeGCall(g1 => ctx(GCall(g.name, g1 :: g.args.tail)), g0)
    }
}
