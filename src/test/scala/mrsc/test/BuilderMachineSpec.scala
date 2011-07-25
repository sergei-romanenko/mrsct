package mrsc.test

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

import mrsc._

object TinyMachine extends Machine[Int, String, Extra] {
  def steps(pState: PState[Int, String, Extra]): List[Command[Int, String, Extra]] = pState.current.conf match {
    case 0 =>
      List(AddChildNodes(List(ChildNode(1, "0 -> 1", NoExtra), ChildNode(2, "0 -> 2", NoExtra))))
    case 1 =>
      List(ConvertToLeaf)
    case 2 =>
      List(ReplaceNode(21, NoExtra))
    case 21 =>
      List(RollbackSubGraph(pState.current.in.coNode, -1, NoExtra))
    case -1 =>
      List(AddChildNodes(List(ChildNode(11, "-1 -> 11", NoExtra))))
    case 11 =>
      List(MakeFold(List()))
  }
}

@RunWith(classOf[JUnitRunner])
class BuilderMachineSpec extends mutable.Specification {
  args(sequential = true)

  val graph: Graph[Int, String, Extra] = {
    val n1 = Node[Int, String, Extra](conf = 11, extraInfo = NoExtra, outs = List(), base = Some(List()), path = List(0))
    val e1 = Edge[Int, String, Extra](n1, "-1 -> 11")
    val n0 = Node[Int, String, Extra](conf = -1, extraInfo = NoExtra, outs = List(e1), base = None, path = List())
    Graph(root = n0, leaves = List(n1))
  }

  "CoGraphBuilder with deterministic machine" should {
    val consumer = new GraphConsumer[Int, String, Extra]()
    val builder = new CoGraphBuilder(TinyMachine, consumer)
    builder.buildCoGraph(0, NoExtra)
    val graphs = consumer.result
    
    "produce just 1 result" in {
      graphs.size must_== 1
    }

    "build cograph in depth first manner" in {
      graphs(0) must_== graph
    }
  }

}