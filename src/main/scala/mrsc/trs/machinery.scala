package mrsc.trs

import mrsc.core._

trait GenericMultiTransformer[C, D]
  extends Transformer[C, D] with GraphBuilder[C, D] {

  type Warning = SNode[C, D]
  def unsafe(g: G): Boolean = false
  def findBase(g: G): Option[SNode[C, D]]
  def inspect(g: G): Option[Warning]
  def drive(whistle: Option[Warning], g: G): List[G]
  def rebuildings(whistle: Option[Warning], g: G): List[G]

  /*! The logic of this transformer is straightforward:
     
     * If there are opportunities for folding, lets fold.
     
     * Otherwise, lets try all variants.
    
   Note that the whistle signal is passed to `drive`, `rebuildings` and `tricks`.
  */
  override def descendants(g: G): List[G] =
    if (unsafe(g))
      List()
    else findBase(g) match {
      case Some(node) =>
        List(fold(node)(g))
      case None =>
        val whistle = inspect(g)
        drive(whistle, g) ++ rebuildings(whistle, g)
    }
}

trait SafetyAware[C, D] extends GenericMultiTransformer[C, D] {
  def unsafe(c: C): Boolean
  override def unsafe(g: G): Boolean = {
    assert(!g.isComplete)
    unsafe(g.current.conf)
  }
}

trait SimpleInstanceFolding[C, D] extends GenericMultiTransformer[C, D] with TRSSyntax[C] {
  override def findBase(g: G): Option[SNode[C, D]] =
    g.current.ancestors.find { n => instanceOf(g.current.conf, n.conf) }
}

trait SimpleInstanceFoldingToAny[C, D] extends GenericMultiTransformer[C, D] with TRSSyntax[C] {
  override def findBase(g: G): Option[SNode[C, D]] =
    g.completeNodes.find { n => instanceOf(g.current.conf, n.conf) }
}

trait SimpleUnaryWhistle[C, D] extends GenericMultiTransformer[C, D] {
  def dangerous(c: C): Boolean
  override def inspect(g: G): Option[Warning] =
    if (dangerous(g.current.conf)) Some(g.current) else None
}

trait SimpleCurrentGensOnWhistle[C, D] extends GenericMultiTransformer[C, D] with TRSSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[Warning], g: G): List[G] = {
    whistle match {
      case None =>
        List()
      case Some(_) =>
        val rbs = rebuildings(g.current.conf) filterNot dangerous
        rbs map { rebuild(_)(g) }
    }
  }
}

trait SimpleGensWithUnaryWhistle[C, D] extends GenericMultiTransformer[C, D] with TRSSyntax[C] with SimpleUnaryWhistle[C, D] {
  override def rebuildings(whistle: Option[Warning], g: G): List[G] = {
    val rbs = rebuildings(g.current.conf) filterNot dangerous
    rbs map { rebuild(_)(g) }
  }
}

trait RuleDriving[C] extends GenericMultiTransformer[C, Int] with RewriteSemantics[C] {
  override def drive(whistle: Option[Warning], g: G): List[G] =
    whistle match {
      case Some(_) =>
        List()
      case None =>
        val subSteps =
          for ((next, i) <- driveConf(g.current.conf).zipWithIndex if next.isDefined)
            yield (next.get, i + 1)
        if (subSteps.isEmpty)
          List(completeCurrentNode()(g))
        else
          List(addChildNodes(subSteps)(g))
    }
}