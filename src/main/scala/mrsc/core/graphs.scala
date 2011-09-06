package mrsc.core

import scala.annotation.tailrec
/*! # SC Graph Abstraction
 
 At the heart of MRSC is a mini-framework for manipulating SC graphs.
 In MRSC SC Graph is a special kind of graph that can be seen as a tree (skeleton), 
 some leaves of which can have loopbacks.
 Note that loopbacks can start only from leaves. 
 
 In some sense, SC Graph is similar to accessible
 pointed graphs (APGs) used in the theory of non-well-founded sets.
 
 Here is the simplest SC graph. There are two loopbacks: j⇢c and h⇢d. 
 There is also a concept of a path. The path to the node g is [0,0,1].

         . c
        .  ↓
      .    d
     .     ↓  .  
    .      e   . 
    .   ↙ ↓ ↘ .  
    .  f   g   h
     . ↓
       j
       
 SC graphs in MRSC are used in two representations:
 
 * `SGraph` - good for easy bottom-up traversals (a node knows about in) and
    for using in multi-result supercompilation 
    (graph consists of complete and incomplete parts, 
    and operation to add outs to incomplete nodes is cheap).
 * `TGraph` - good for top-down traversals (a node knows about its outs).
 
 The main idea here is to use functional (immutable) data structures in order to support
 multi-results composed of shared data.
 */

/*! `SNode[C, D]` is dual to `TNode[C, D]`. 
 */
case class SNode[C, D](
  conf: C,
  in: SEdge[C, D],
  back: Option[Path],
  sPath: Path) {

  lazy val tPath = sPath.reverse

  val ancestors: List[SNode[C, D]] =
    if (in == null) List() else in.node :: in.node.ancestors

  override def toString = conf.toString
}

case class SEdge[C, D](node: SNode[C, D], driveInfo: D)

/*! `Graph[C, D, E]` is a core data structure in MRSC.
 * It may represent (1) a "work in progress" (2) a completed graph and
 * (3) unfinished, and yet unworkable graph.
 * We know already processed part of an SC graph
 * (`completeLeaves`, `completeNodes`) and a frontier 
 * of incomplete part (`incompleteLeaves`).
 * 
 *`C` (configuration) is a type of node label; 
 *`D` (driving) is a type of edge label (driving info);
 */
case class SGraph[C, D](
  incompleteLeaves: List[SNode[C, D]],
  completeLeaves: List[SNode[C, D]],
  completeNodes: List[SNode[C, D]]) {
  
  val isComplete = incompleteLeaves.isEmpty
  val current = if (isComplete) null else incompleteLeaves.head
}

/*!# Abstract steps
   Under the hood an abstract machine deals with some kind of semantics of the language.
   Low-level operations should be translated into high-level abstract operations (or messages) 
   over SC graphs.
*/
sealed trait GraphStep[C, D] extends (SGraph[C, D] => SGraph[C, D])

case class CompleteCurrentNodeStep[C, D] extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) =
    SGraph(g.incompleteLeaves.tail, g.current :: g.completeLeaves, g.current :: g.completeNodes)
}

case class AddChildNodesStep[C, D](ns: List[(C, D)]) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
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
}

case class FoldStep[C, D](baseNode: SNode[C, D]) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    val node = g.current.copy(back = Some(baseNode.sPath))
    SGraph(g.incompleteLeaves.tail, node :: g.completeLeaves, node :: g.completeNodes)
  }
}

case class RebuildStep[C, D](c: C) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    val node = g.current.copy(conf = c)
    SGraph(node :: g.incompleteLeaves.tail, g.completeLeaves, g.completeNodes)
  }
}

case class RollbackStep[C, D](to: SNode[C, D], c: C) extends GraphStep[C, D] {
  def apply(g: SGraph[C, D]) = {
    def prune_?(n: SNode[C, D]) = n.tPath.startsWith(to.tPath)
    val node = to.copy(conf = c)
    val completeNodes1 = g.completeNodes.remove(prune_?)
    val completeLeaves1 = g.completeLeaves.remove(prune_?)
    val incompleteLeaves1 = g.incompleteLeaves.tail.remove(prune_?)
    SGraph(node :: incompleteLeaves1, completeLeaves1, completeNodes1)
  }
}

/*! The labeled directed edge. `N` is a destination node; `D` is driving info.
 */
case class TEdge[C, D](node: TNode[C, D], driveInfo: D)

/*! `TGraph[C, D, E]`.
 * `TGraph` is a representation of the graph of configurations
 * that is good for top-down traversals (a node knows about its outs).
 * 
 * A `TGraph` is a "transposed" representation of a `Graph`,
 * each Node being replaced with TNode,
 * and each Edge being replaced with TEdge.
 *`C` (configuration) is a type of node label; 
 *`D` (driving) is a type of edge label (driving info);
 *`E` (extra information) is a type of extra label of a node (extra info). 
 * Extra information may be seen as an additional "instrumentation" of SC graph.
 */
case class TGraph[C, D](root: TNode[C, D], leaves: List[TNode[C, D]]) {
  def get(tPath: TPath): TNode[C, D] = root.get(tPath)
  override def toString = root.toString
}

/*! `TNode[C, D, E]` is a very simple and straightforward implementation of
 * a top-down node. 
 */
case class TNode[C, D](
  conf: C,
  outs: List[TEdge[C, D]],
  back: Option[TPath],
  tPath: TPath) {

  lazy val sPath = tPath.reverse

  @tailrec
  final def get(relTPath: TPath): TNode[C, D] = relTPath match {
    case Nil => this
    case i :: rp => outs(i).node.get(rp)
  }

  val isLeaf = outs.isEmpty
  val isRepeat = back.isDefined
  
  override def toString = GraphPrettyPrinter.toString(this)
}

/*! Auxiliary data for transposing a graph into a transposed graph.
 */
case class Tmp[C, D](node: TNode[C, D], in: SEdge[C, D])

/*! A transformer of graphs into transposed graphs.
 */
object Transformations {
  /*! Transposition is done in the following simple way. Nodes are grouped according to the 
   levels (the root is 0-level). Then graphs are produced from in bottom-up fashion.
   */
  def transpose[C, D, E](g: SGraph[C, D]): TGraph[C, D] = {
    require(g.isComplete)
    val allLeaves = g.incompleteLeaves ++ g.completeLeaves
    val allNodes = g.incompleteLeaves ++ g.completeNodes
    val orderedNodes = allNodes.sortBy(_.sPath)(PathOrdering)
    val rootNode = orderedNodes.head

    val leafPathes = allLeaves.map(_.sPath)
    val levels = orderedNodes.groupBy(_.sPath.length).toList.sortBy(_._1).map(_._2)
    val sortedLevels = levels.map(_.sortBy(_.tPath)(PathOrdering))
    val (tNodes, tLeaves) = subTranspose(sortedLevels, leafPathes)
    val nodes = tNodes map { _.node }
    val leaves = tLeaves map { _.node }
    return TGraph(nodes(0), leaves)
  }

  // sub-transposes graph into transposed graph level-by-level
  private def subTranspose[C, D](
    nodes: List[List[SNode[C, D]]],
    leaves: List[TPath]): (List[Tmp[C, D]], List[Tmp[C, D]]) =
    nodes match {
      case Nil =>
        (Nil, Nil)

      // leaves only??
      case ns1 :: Nil =>
        val tmpNodes: List[Tmp[C, D]] = ns1 map { n =>
          val node = TNode[C, D](n.conf, Nil, n.back.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp =>
          leaves.contains(tmp.node.sPath)
        }
        (tmpNodes, tmpLeaves)

      case ns1 :: ns => {
        val (allCh, leaves1) = subTranspose(ns, leaves)
        val allchildren = allCh.groupBy { _.node.sPath.tail }
        val tmpNodes = ns1 map { n =>
          val children: List[Tmp[C, D]] = allchildren.getOrElse(n.sPath, Nil)
          val edges = children map { tmp => TEdge(tmp.node, tmp.in.driveInfo) }
          val node = new TNode(n.conf, edges, n.back.map(_.reverse), n.tPath)
          Tmp(node, n.in)
        }
        val tmpLeaves = tmpNodes.filter { tmp => leaves.contains(tmp.node.sPath) }
        (tmpNodes, tmpLeaves ++ leaves1)
      }
    }
}

/*! Ad Hoc console pretty printer for graphs.
 */
object GraphPrettyPrinter {
  def toString(node: TNode[_, _], indent: String = ""): String = {
    val sb = new StringBuilder(indent + "|__" + node.conf)
    if (node.back.isDefined) {
      sb.append("*******")
    }
    for (edge <- node.outs) {
      sb.append("\n  " + indent + "|" + (if (edge.driveInfo != null) edge.driveInfo else ""))
      sb.append("\n" + toString(edge.node, indent + "  "))
    }
    sb.toString
  }
}

/*! The simple lexicographic order on paths.
 */
object PathOrdering extends Ordering[TPath] {
  @tailrec
  final def compare(p1: TPath, p2: TPath) =
    if (p1.length < p2.length) {
      -1
    } else if (p1.length > p2.length) {
      +1
    } else {
      val result = p1.head compare p2.head
      if (result == 0) {
        compare(p1.tail, p2.tail)
      } else {
        result
      }
    }
}
