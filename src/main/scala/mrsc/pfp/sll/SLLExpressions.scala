package mrsc.pfp.sll

import mrsc.pfp.Name

object Lifting {

  def expr2Task(e: Expr): SLLTask = {
    val (e1, defs) = Lifting.lift(e)

    def compare(n1: String, n2: String): Boolean = {
      if (n1.length < n2.length) {
        true
      } else if (n1.length > n2.length) {
        false
      } else {
        n1 < n2
      }
    }

    val defs1 = defs sortWith { (d1, d2) =>
      if (compare(d1.name, d2.name) && compare(d2.name, d1.name)) {
        val GFun(n1, Pat(pn1, _), _, _) = d1
        val GFun(n2, Pat(pn2, _), _, _) = d2
        pn1 < pn2
      } else compare(d1.name, d2.name)
    }

    SLLTask(e1, Program(defs1))
  }

  def liftArgs(args: List[Expr]): (List[Expr], List[Def]) = {
    var args1 = List[Expr]()
    var defs = List[Def]()
    for (arg <- args) {
      val (arg1, defs1) = lift(arg)
      args1 = args1 ++ List(arg1)
      defs = defs ++ defs1
    }
    (args1, defs)
  }

  // the only thing it does is collect definitions
  def lift(e: Expr): (Expr, List[Def]) = (e: @unchecked) match {
    case v: Var =>
      (v, Nil)
    case Ctr(n, args) =>
      val (args1, defs) = liftArgs(args)
      (Ctr(n, args1), defs)
    case FCall(n, args) =>
      val (args1, defs) = liftArgs(args)
      (FCall(n, args1), defs)
    case GCall(n, args) =>
      val (args1, defs) = liftArgs(args)
      (GCall(n, args1), defs)
    case Where(e, defs) =>
      var newDefs = List[Def]()
      for (deff <- defs) deff match {
        case FFun(n, vs, body) =>
          val (body1, defs1) = lift(body)
          val def1 = FFun(n, vs, body1)
          newDefs = newDefs ++ defs1 ++ List(def1)
        case GFun(n, p, vs, body) =>
          val (body1, defs1) = lift(body)
          val def1 = GFun(n, p, vs, body1)
          newDefs = newDefs ++ defs1 ++ List(def1)
      }
      val (e1, defs1) = lift(e)
      newDefs = newDefs ++ defs1
      (e1, newDefs)
  }
}

// rename bound variables into v1, v2, v3 ..., f1, f2, f3...
object SyntaxNormalization {

  import scala.collection.immutable.ListMap

  // canonizes names of variables 
  def fixNames(e: Expr): Expr = {
    var fmap: Map[String, String] = Map()
    var f = 0

    // fresh function name
    def ff(x: Any = null): String = {
      f = f + 1
      "f." + f
    }

    def v(x: Int): Var = Var("v." + x)

    def vn(x: Int): Name = "v." + x

    def fixBoundVars(e: Expr, m: ListMap[Name, Int]): Expr = (e: @unchecked) match {
      case Var(n) => m.get(n).map(v).getOrElse(Var(n))
      case Ctr(n, args) => Ctr(n, args map {
        fixBoundVars(_, m)
      })
      case FCall(n, args) => FCall(n, args map {
        fixBoundVars(_, m)
      })
      case GCall(n, args) => GCall(n, args map {
        fixBoundVars(_, m)
      })
      case Where(e1, defs) =>
        val next = m.lastOption.map(_._2).getOrElse(0) + 1
        val defs1 = defs map {
          case FFun(name, args, body) =>
            val args1 = args.zipWithIndex map { case (k, v) => (k, next + v) }
            val m1 = m ++ args1
            val body1 = fixBoundVars(body, m1)
            FFun(name, args map m1.andThen(vn _), body1)
          case GFun(name, Pat(n, pargs), args, body) =>
            val delta = (pargs ++ args).zipWithIndex map { case (k, v) => (k, next + v) }
            val m1 = m ++ delta
            val body1 = fixBoundVars(body, m1)
            GFun(name, Pat(n, pargs map m1.andThen(vn _)), args map m1.andThen(vn _), body1)
        }
        val e2 = fixBoundVars(e1, m)
        Where(e2, defs1)
    }

    def fixDefFs(deff: Def): Def = deff match {
      case FFun(n, args, body) => fmap.get(n) match {
        case Some(n1) =>
          FFun(n1, args, fixFs(body))
        case None =>
          val n1 = ff(n)
          fmap = fmap + (n -> n1)
          FFun(n1, args, fixFs(body))
      }
      case GFun(n, pat, args, body) => fmap.get(n) match {
        case Some(n1) =>
          GFun(n1, pat, args, fixFs(body))
        case None =>
          val n1 = ff(n)
          fmap = fmap + (n -> n1)
          GFun(n1, pat, args, fixFs(body))
      }

    }

    def fixFs(e: Expr): Expr = (e: @unchecked) match {
      case v: Var => v
      case Ctr(n, args) => Ctr(n, args map fixFs)
      case FCall(n, args) => FCall(fmap(n), args map fixFs)
      case GCall(n, args) => GCall(fmap(n), args map fixFs)
      case Where(e1, defs) =>
        val defs1 = defs map fixDefFs
        val e2 = fixFs(e1)
        Where(e2, defs1)
    }

    val fixed1 = fixBoundVars(e, ListMap.empty)
    val fixed2 = fixFs(fixed1)
    fixed2
  }
}
