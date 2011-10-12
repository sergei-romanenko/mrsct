package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc.core._
import mrsc.pfp._

object TinyTransformer
  extends Transformer[Int, String] {

  override def descendants(g: G): List[G] = {
    g.current.conf match {
      case 0 =>
        List(AddChildNodesStep(List((1, "0 -> 1"), (2, "0 -> 2")))(g))
      case 1 =>
        List(CompleteCurrentNodeStep()(g))
      case 2 =>
        List(RebuildStep(21)(g))
      case 21 =>
        List(RollbackStep(g.current.in.node, -1)(g))
      case -1 =>
        List(AddChildNodesStep(List((11, "-1 -> 11")))(g))
      case 11 =>
        List(FoldStep(g.current.in.node)(g))
    }
  }
}

@RunWith(classOf[JUnitRunner])
class GraphGeneratorSpec extends mutable.Specification {
  args(sequential = true)

  val graph: TGraph[Int, String] = {
    val n1 = TNode[Int, String](conf = 11, outs = List(), base = Some(List()), tPath = List(0))
    val e1 = TEdge[Int, String](n1, "-1 -> 11")
    val n0 = TNode[Int, String](conf = -1, outs = List(e1), base = None, tPath = List())
    TGraph(root = n0, leaves = List(n1))
  }

  "GraphGenerator with deterministic transformer" should {

    val tgraphs = GraphGenerator(TinyTransformer, 0) map Transformations.transpose toList

    "produce just 1 result" in {
      tgraphs.size must_== 1
    }

    "build graph in depth first manner" in {
      tgraphs(0) must_== graph
    }
  }

}