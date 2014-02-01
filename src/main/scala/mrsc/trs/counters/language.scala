package mrsc.trs.counters

import mrsc.trs._

sealed trait Expr {
  def +(comp: Expr): Expr
  def -(comp: Expr): Expr
  def >=(i: Int): Boolean
  def ===(i: Int): Boolean
}

case class Num(i: Int) extends Expr {
  override def +(comp: Expr) = comp match {
    case Omega => Omega
    case Num(j) => Num(i + j)
  }
  override def -(comp: Expr) = comp match {
    case Omega => Omega
    case Num(j) => Num(i - j)
  }
  override def ===(j: Int) = i == j
  override def >=(j: Int) = i >= j
  override def toString = i.toString
}

case object Omega extends Expr {
  def +(comp: Expr) = Omega
  def -(comp: Expr) = Omega
  def >=(comp: Int) = true
  override def ===(j: Int) = true
  override def toString = "ϖ"
}

trait CountersSyntax extends TRSSyntax[Conf] {
  def equiv(c1: Conf, c2: Conf) = CountersSyntax.equiv(c1, c2)
  def instanceOf(c1: Conf, c2: Conf) = CountersSyntax.instanceOf(c1, c2)
  def rebuildings(c: Conf) = CountersSyntax.rebuildings(c)
}

object CountersSyntax extends {
  def equiv(c1: Conf, c2: Conf) = c1 == c2

  def instanceOf(c1: Conf, c2: Conf): Boolean =
    (c1, c2).zipped.forall(instanceOf)

  def instanceOf(x: Expr, y: Expr) = (x, y) match {
    case (_, Omega) => true
    case (_, _) => x == y
  }

  private def cartProd[T](zzs: List[List[T]]): List[List[T]] = zzs match {
    case Nil => List(List())
    case xs :: xss => for (y <- xs; ys <- cartProd(xss)) yield y :: ys
  }

  def rebuildings(c: Conf) = cartProd(c map genExpr).filterNot(_ == c)

  private def genExpr(c: Expr): List[Expr] = c match {
    case Omega => List(Omega)
    case value => List(Omega, value)
  }
}

trait CountersSemantics extends RewriteSemantics[Conf] {
  val protocol: Protocol
  def driveConf(c: Conf) = protocol.rules.map { _.lift(c) }
}

trait LWhistle {
  val l: Int
  def dangerous(counter: Conf) = counter exists {
    case Num(i) => i >= l
    case Omega => false
  }
}
