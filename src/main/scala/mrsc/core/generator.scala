package mrsc.core

import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

/*!# Abstract graph transformers
  
  An abstract graph transformer represents the semantics of the object language 
  (more precisely, meta-semantics) through operations over graphs of configurations. 
  Graph transformers are non-deterministic, so that they can be used for implementing
  multi-result supercompilation.
 */

trait Transformer[C, D] {
  type G = SGraph[C, D]
  type S = G => G
  def descendants(g: G): List[G]
}

/*!# Generating graphs.
 
 A graph generator knows only how to build a graph using a transformer, but not what to do with this graph later.
 */

/*! This class produces iterators producing graphs by demand. */

case class GraphGenerator[C, D](transformer: Transformer[C, D], conf: C)
  extends Iterator[SGraph[C, D]] {

  /*! It maintains a list of graphs
     * and starts with a one-element list of graphs. 
     */

  private val completeGs: Queue[SGraph[C, D]] = Queue()
  private var gs: List[SGraph[C, D]] = List(SGraph.initial[C, D](conf))

  private def normalize(): Unit =
    while (completeGs.isEmpty && !gs.isEmpty) {
      val pendingDelta = ListBuffer[SGraph[C, D]]()
      val h = gs.head
      val newGs = transformer.descendants(h)
      for (g1 <- newGs)
        if (g1.isComplete) {
          completeGs.enqueue(g1)
        } else {
          pendingDelta += g1
        }
      gs = pendingDelta ++: gs.tail
    }

  def hasNext: Boolean = {
    normalize()
    !completeGs.isEmpty
  }

  def next(): SGraph[C, D] = {
    if (!hasNext) throw new NoSuchElementException("no graph")
    completeGs.dequeue()
  }
}
