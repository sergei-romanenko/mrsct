package mrsc.core

/*!# Abstract steps
   Under the hood an abstract transformer deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/

trait GraphBuilder[C, D] extends GraphTypes[C, D] {
  def completeCurrentNode()(g: G): G

  def addChildNodes(ns: List[(C, D)])(g: G): G

  def fold(baseNode: N)(g: G): G

  def rebuild(c: C)(g: G): G

  def rollback(to: N, c: C)(g: G): G
}

trait BasicGraphBuilder[C, D] extends GraphBuilder[C, D] {

  def completeCurrentNode()(g: G) =
    SGraph(g.incompleteLeaves.tail,
      g.current :: g.completeLeaves,
      g.current :: g.completeNodes)

  def addChildNodes(ns: List[(C, D)])(g: G): G = {
    val deltaLeaves: List[N] = ns.zipWithIndex map {
      case ((conf, dInfo), i) =>
        val in = SEdge(g.current, dInfo)
        SNode(conf, in, None, i :: g.current.sPath)
    }
    // Now it is depth-first traversal. If you change
    // deltaLeaves ++ ls -> ls ++ deltaLeaves,
    // you will have breadth-first traversal
    SGraph(deltaLeaves ++ g.incompleteLeaves.tail, g.completeLeaves, g.current :: g.completeNodes)
  }

  def fold(baseNode: N)(g: G): G = {
    val node = g.current.copy(base = Some(baseNode.sPath))
    SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
  }

  def rebuild(c: C)(g: G): G = {
    val node = g.current.copy(conf = c)
    SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
  }

  def rollback(to: N, c: C)(g: G): G = {
    def prune_?(n: N) = n.tPath.startsWith(to.tPath)

    val node = to.copy(conf = c)
    val completeNodes1 = g.completeNodes.filterNot(prune_?)
    val completeLeaves1 = g.completeLeaves.filterNot(prune_?)
    val incompleteLeaves1 = g.incompleteLeaves.tail.filterNot(prune_?)
    SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }
}
