package mrsc.pfp

import mrsc.core._

case class Contraction[+C](v: Name, pat: C) {
  override def toString: Name =
    if (v != null) v + " = " + pat else ""
  def subst(): Map[Name, C] = Map(v -> pat)
}

sealed trait DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString: Name = contr.toString
}

trait DriveSteps[C] {
  this: GraphBuilder[C, DriveInfo[C]] =>

  def transientDriveStep(next: C): GG = {
    val subSteps = List((next, TransientStepInfo)): List[(C, DriveInfo[C])]
    addChildNodes(subSteps)
  }

  def stopDriveStep(): GG = completeCurrentNode()

  def decomposeDriveStep(compose: List[C] => C, parts: List[C]): GG = {
    val stepInfo = DecomposeStepInfo(compose)
    val subSteps = parts map { a => (a, stepInfo) }
    addChildNodes(subSteps)
  }

  def variantsDriveStep(cases: List[(C, Contraction[C])]): GG = {
    val ns = cases map { v => (v._1, VariantsStepInfo(v._2)) }
    addChildNodes(ns)
  }
}
