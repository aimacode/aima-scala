package aima.core.environment.map2d

import org.specs2.mutable.Specification

/**
  * @author Shawn Garner
  */
class LabeledGraphSpec extends Specification {
  "add vertex" in {
    val graph = new LabeledGraph[Int, (Int, Int)]

    graph.vertexLabels must beEmpty

    graph.addVertex(2)

    graph.vertexLabels must haveSize(1)
    graph.isVertexLabel(2) must beTrue
    graph.isVertexLabel(1) must beFalse

    graph.addVertex(3)

    graph.vertexLabels must haveSize(2)
    graph.isVertexLabel(3) must beTrue
    graph.isVertexLabel(2) must beTrue
    graph.isVertexLabel(1) must beFalse

    graph.vertexLabels must contain(exactly(2, 3))
  }

  "edge creation" in {
    val graph = new LabeledGraph[Int, (Int, Int)]

    graph.addVertex(2)

    graph.vertexLabels must haveSize(1)
    graph.isVertexLabel(2) must beTrue

    graph.addVertex(6)

    graph.vertexLabels must haveSize(2)
    graph.isVertexLabel(6) must beTrue

    graph.set(2, 6, (2, 6))

    graph.vertexLabels must haveSize(2)

    graph.set(3, 12, (3, 12))

    graph.vertexLabels must haveSize(4)
    graph.isVertexLabel(3) must beTrue
    graph.isVertexLabel(12) must beTrue

    graph.set(2, 12, (2, 12))

    graph.vertexLabels must haveSize(4)

    graph.set(3, 6, (3, 6))

    graph.vertexLabels must haveSize(4)

    graph.set(6, 12, (6, 12))

    graph.vertexLabels must haveSize(4)

    graph.get(2, 3) must beNone
    graph.get(2, 6) must beSome((2, 6))
    graph.get(3, 6) must beSome((3, 6))
    graph.get(3, 12) must beSome((3, 12))
    graph.get(6, 12) must beSome((6, 12))
  }

  "get successors" in {
    val graph = new LabeledGraph[Int, (Int, Int)]

    graph.successors(2) must beEmpty

    graph.set(2, 12, (2, 12))
    graph.set(3, 12, (3, 12))
    graph.set(2, 6, (2, 6))
    graph.set(3, 6, (3, 6))
    graph.set(6, 12, (6, 12))

    graph.successors(2) must contain(exactly(6, 12))
    graph.successors(3) must contain(exactly(6, 12))

    graph.successors(6) must contain(exactly(12))
    graph.successors(12) must beEmpty
  }

}
