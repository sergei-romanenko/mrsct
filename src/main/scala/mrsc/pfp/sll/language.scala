package mrsc.pfp.sll

import mrsc.core._
import mrsc.pfp._

import SLLSyntax._

trait SLLSyntax extends PFPSyntax[Expr] {
  def equiv(c1: Expr, c2: Expr): Boolean = SLLSyntax.equiv(c1, c2)
  def instanceOf(c1: Expr, c2: Expr): Boolean = SLLSyntax.instanceOf(c1, c2)

  override def subst(c: Expr, sub: Subst[Expr]): Expr =
    SLLSyntax.subst(c, sub)

  override def rawRebuildings(e: Expr): List[RawRebuilding[Expr]] =
    SLLRebuilding.rebuildings(e)

  override def translate(rb: RawRebuilding[Expr]): Expr =
    Let(rb._1, rb._2.toList)

  override def findSubst(from: Expr, to: Expr) =
    SLLSyntax.findSubst(from, to)

  override def size(e: Expr) = e.size
  override val subclass = new SimplePartialOrdering[Expr] {
    override def lteq(c1: Expr, c2: Expr) = SLLSyntax.instanceOf(c1, c2)
  }

}

object SLLSyntax {
  def subst(term: Expr, m: Subst[Expr]): Expr = term match {
    case v @ Var(n) => m.getOrElse(n, v)
    case Ctr(name, args) => Ctr(name, args map { subst(_, m) })
    case FCall(name, args) => FCall(name, args map { subst(_, m) })
    case GCall(name, args) => GCall(name, args map { subst(_, m) })
    case Where(e, defs) => Where(subst(e, m), defs map { subst(_, m) })
    case Let(e, bs) => Let(subst(e, m), bs)
  }

  private def subst(deff: Def, m: Subst[Expr]): Def = deff match {
    case FFun(n, xs, e) => FFun(n, xs, subst(e, m -- xs))
    case GFun(n, Pat(pn, xs), ys, e) => GFun(n, Pat(pn, xs), ys, subst(e, m -- xs -- ys))
  }

  private def vs(t: Expr): List[Var] = t match {
    case v: Var => List(v)
    case Ctr(_, args) => args.foldLeft(List[Var]())(_ ++ vs(_))
    case FCall(_, args) => args.foldLeft(List[Var]())(_ ++ vs(_))
    case GCall(_, args) => args.foldLeft(List[Var]())(_ ++ vs(_))
    case Let(e, _) => vs(e)
    case Where(e, _) => vs(e)
  }

  def vars(t: Expr): List[Var] = vs(t).distinct

  def equiv(c1: Expr, c2: Expr): Boolean = instanceOf(c1, c2) && instanceOf(c2, c1)

  def instanceOf(t1: Expr, t2: Expr): Boolean = (t1.size >= t2.size) && (findSubst(t2, t1).isDefined)

  def findSubst(from: Expr, to: Expr): Option[Subst[Expr]] =
    walk((from, to), Map())

  private def walk(p: (Expr, Expr), s: Subst[Expr]): Option[Subst[Expr]] = p match {
    case (Ctr(n1, args1), Ctr(n2, args2)) if n1 == n2 => walk1(args1 zip args2, s)
    case (FCall(n1, args1), FCall(n2, args2)) if n1 == n2 => walk1(args1 zip args2, s)
    case (GCall(n1, args1), GCall(n2, args2)) if n1 == n2 => walk1(args1 zip args2, s)
    case (Var(n), to) => (s.get(n): @unchecked) match {
      case Some(to1) if to1 == to => Some(s)
      case Some(to1) if to1 != to => None
      case None => Some(s + (n -> to))
    }
    case _ => None
  }

  private def walk1(ps: List[(Expr, Expr)], s: Subst[Expr]) =
    ps.foldLeft[Option[Subst[Expr]]](Some(s)) { (s, p) => s.flatMap { walk(p, _) } }

}

trait SLLDriving extends PFPTransformer[Expr]
  with Reducer { this: DriveSteps[Expr] =>

  type R = List[GG]

  val program: Program

  def drive(g: G): List[G] =
    decompose(g.current.conf) map { _(g) }

  def caseDecLet(let: Let): List[GG] = {
    val (names, es) = let.bindings.unzip
    val compose = { parts: List[Expr] =>
      val in :: binds = parts
      val sub = (names zip binds).toMap
      subst(in, sub)
    }
    List(decomposeDriveStep(compose, let.term :: es))
  }

  def caseObservableCtr(ctr: Ctr): List[GG] =
    List(decomposeDriveStep({ Ctr(ctr.name, _: List[Expr]) }, ctr.args))

  def caseObservableVar(v: Var): List[GG] =
    List(stopDriveStep())

  def caseFRedex(ctx: Ctx, fcall: FCall): List[GG] = {
    val FFun(_, fargs, body) = program.f(fcall.name)
    val reduced = subst(body, (fargs zip fcall.args).toMap)
    List(transientDriveStep(ctx(reduced)))
  }

  def caseGRedexCtr(ctx: Ctx, gcall: GCall, ctr: Ctr): List[GG] = {
    val GFun(_, p, gargs, body) = program.g(gcall.name, ctr.name)
    val reduced = subst(
      body,
      ((p.args ++ gargs) zip (ctr.args ++ gcall.args.tail)).toMap)
    List(transientDriveStep(ctx(reduced)))
  }

  def caseGRedexVar(ctx: Ctx, gcall: GCall, v: Var): List[GG] = {
    val cases = program.gs(gcall.name) map {
      case GFun(_, p, gargs, body) =>
        val ctr = instantiate(p, v)
        val reduced = subst(
          body,
          ((p.args ++ gargs) zip (ctr.args ++ gcall.args.tail)).toMap)
        val contraction = Contraction(v.name, Ctr(p.name, ctr.args))
        val driven = subst(ctx(reduced), contraction.subst)
        (driven, contraction)
    }
    List(variantsDriveStep(cases))
  }

  def instantiate(p: Pat, v: Var): Ctr = {
    val vars = p.args.indices.toList.map { i => Var("de_" + p.name + "_" + i + "/" + v.name) }
    Ctr(p.name, vars)
  }
}