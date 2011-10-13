package mrsc.core

/*!# Abstract steps
   Under the hood an abstract transformer deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/

trait GraphBuilder[C, D] {
  type GG = SGraph[C, D] => SGraph[C, D]
  def completeCurrentNode(): GG
  def addChildNodes(ns: List[(C, D)]): GG
  def fold(baseNode: SNode[C, D]): GG
  def rebuild(c: C): GG
  def rollback(to: SNode[C, D], c: C): GG
}

trait BasicGraphBuilder[C, D] extends GraphBuilder[C, D] {

  def completeCurrentNode() = (g: SGraph[C, D]) =>
    SGraph(g.incompleteLeaves.tail, g.current :: g.completeLeaves, g.current :: g.completeNodes)

  def addChildNodes(ns: List[(C, D)]) = (g: SGraph[C, D]) =>
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

  def fold(baseNode: SNode[C, D]) = (g: SGraph[C, D]) =>
    {
      val node = g.current.copy(base = Some(baseNode.sPath))
      SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
    }

  def rebuild(c: C) = (g: SGraph[C, D]) =>
    {
      val node = g.current.copy(conf = c)
      SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
    }

  def rollback(to: SNode[C, D], c: C) = (g: SGraph[C, D]) =>
    {
      def prune_?(n: SNode[C, D]) = n.tPath.startsWith(to.tPath)
      val node = to.copy(conf = c)
      val completeNodes1 = g.completeNodes.remove(prune_?)
      val completeLeaves1 = g.completeLeaves.remove(prune_?)
      val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
      SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
    }
}
