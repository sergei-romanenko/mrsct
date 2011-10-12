package mrsc.pfp

import mrsc.core._

case class Contraction[+C](v: Name, pat: C) {
  override def toString =
    if (v != null) v + " = " + pat else ""
  def subst() = Map[Name, C](v -> pat)
}

sealed trait DriveInfo[+C]
case object TransientStepInfo extends DriveInfo[Nothing] {
  override def toString = "->"
}
case class DecomposeStepInfo[C](compose: List[C] => C) extends DriveInfo[C] {
  override def toString = ""
}
case class VariantsStepInfo[C](contr: Contraction[C]) extends DriveInfo[C] {
  override def toString = contr.toString
}

trait DriveSteps[C] { this: GraphBuilderSteps[C, DriveInfo[C]] =>

  type DriveStep[C] = SGraph[C, DriveInfo[C]] => SGraph[C, DriveInfo[C]]
  
  def transientDriveStep(next: C): DriveStep[C] = {
    val subSteps = List((next, TransientStepInfo)): List[(C, DriveInfo[C])]
    addChildNodesStep(subSteps)
  }

  def stopDriveStep(): DriveStep[C] = completeCurrentNodeStep()

  def decomposeDriveStep(compose: List[C] => C, parts: List[C]): DriveStep[C] = {
    val stepInfo = DecomposeStepInfo(compose)
    val subSteps = parts map { a => (a, stepInfo) }
    addChildNodesStep(subSteps)
  }

  def variantsDriveStep(cases: List[(C, Contraction[C])]): DriveStep[C] = {
    val ns = cases map { v => (v._1, VariantsStepInfo(v._2)) }
    addChildNodesStep(ns)
  }
}