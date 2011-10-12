package mrsc.core

/*!# Abstract steps
   Under the hood an abstract transformer deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/

trait GraphBuilderSteps[C, D] {
  type GG = SGraph[C, D] => SGraph[C, D]
  def completeCurrentNodeStep(): GG
  def addChildNodesStep(ns: List[(C, D)]): GG
  def foldStep(baseNode: SNode[C, D]): GG
  def rebuildStep(c: C): GG
  def rollbackStep(to: SNode[C, D], c: C): GG
}

trait GraphBuilder[C, D] extends GraphBuilderSteps[C, D] {

  def completeCurrentNodeStep() = (g: SGraph[C, D]) =>
    SGraph(g.incompleteLeaves.tail, g.current :: g.completeLeaves, g.current :: g.completeNodes)

  def addChildNodesStep(ns: List[(C, D)]) = (g: SGraph[C, D]) =>
    {
      val deltaLeaves: List[SNode[C, D]] = ns.zipWithIndex map {
        case ((conf, dInfo), i) =>
          val in = SEdge(g.current, dInfo)
          SNode(conf, in, None, i :: g.current.sPath)
      }
      // Now it is depth-first traversal. If you change 
      // deltaLeaves ++ ls -> ls ++ deltaLeaves,
      // you will have breadth-first traversal
      SGraph(deltaLeaves ++ g.incompleteLeaves.tail, g.completeLeaves, g.current :: g.completeNodes)
    }

  def foldStep(baseNode: SNode[C, D]) = (g: SGraph[C, D]) =>
    {
      val node = g.current.copy(base = Some(baseNode.sPath))
      SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
    }

  def rebuildStep(c: C) = (g: SGraph[C, D]) =>
    {
      val node = g.current.copy(conf = c)
      SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
    }

  def rollbackStep(to: SNode[C, D], c: C) = (g: SGraph[C, D]) =>
    {
      def prune_?(n: SNode[C, D]) = n.tPath.startsWith(to.tPath)
      val node = to.copy(conf = c)
      val completeNodes1 = g.completeNodes.remove(prune_?)
      val completeLeaves1 = g.completeLeaves.remove(prune_?)
      val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
      SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
    }
}
