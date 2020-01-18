package aima.core.environment.map2d

/**
  * @author Shawn Garner
  */
final class LabeledGraph[Vertex, Edge] {
  import scala.collection.mutable
  val globalEdgeLookup = new mutable.LinkedHashMap[Vertex, mutable.LinkedHashMap[Vertex, Edge]]() // TODO: get rid of mutability; ListMap should work
  val vertexLabelsList = new mutable.ArrayBuffer[Vertex]()                                        // TODO: get rid of mutability

  def addVertex(v: Vertex): Unit = {
    checkForNewVertex(v)
    ()
  }

  def set(from: Vertex, to: Vertex, edge: Edge): Unit = {
    val localEdgeLookup = checkForNewVertex(from)
    localEdgeLookup.put(to, edge)
    checkForNewVertex(to)
    ()
  }

  def remove(from: Vertex, to: Vertex): Unit = {
    val localEdgeLookup = globalEdgeLookup.get(from)
    localEdgeLookup.foreach(l => l.remove(to))
  }

  def get(from: Vertex, to: Vertex): Option[Edge] = {
    val localEdgeLookup = globalEdgeLookup.get(from)
    localEdgeLookup.flatMap(_.get(to))
  }

  def successors(v: Vertex): List[Vertex] = {
    val localEdgeLookup = globalEdgeLookup.get(v)
    localEdgeLookup.toList.flatMap(_.keySet.toList)
  }

  def vertexLabels =
    vertexLabelsList.toList

  def isVertexLabel(v: Vertex): Boolean =
    globalEdgeLookup.get(v).isDefined

  def clear(): Unit = {
    vertexLabelsList.clear()
    globalEdgeLookup.clear()
  }

  private def checkForNewVertex(v: Vertex): mutable.LinkedHashMap[Vertex, Edge] = {
    val maybeExisting = globalEdgeLookup.get(v)
    maybeExisting match {
      case None =>
        val m = new mutable.LinkedHashMap[Vertex, Edge]
        globalEdgeLookup.put(v, m)
        vertexLabelsList.append(v)
        m
      case Some(existing) =>
        existing
    }
  }
}
