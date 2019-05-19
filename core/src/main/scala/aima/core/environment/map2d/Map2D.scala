package aima.core.environment.map2d

/**
  * Provides a general interface for two dimensional maps.
  *
  * @author Shawn Garner
  */
trait Map2D {
  /**
    *
    * @return a list of all locations in the map.
    */
  def locations: List[String]


  /**
    * Answers to the question: Where can I get, following one of the
    * connections starting at the specified location?
    *
    * @param fromLocation
    * locations linked from.
    * @return a list of the locations that are connected from the given
    *         location.
    */
  def locationsLinkedTo(fromLocation: String): List[String]


  /**
    * Get the travel distance between the two specified locations if they are
    * linked by a connection and null otherwise.
    *
    * @param fromLocation
    * the starting from location.
    * @param toLocation
    * the to location.
    * @return the travel distance between the two specified locations if they
    *         are linked by a connection and null otherwise.
    */
  def distance(fromLocation: String, toLocation: String): Option[Distance]


  /**
    * Get the position of the specified location.
    *
    * @param location
    * the location whose position is to be returned.
    * @return the position of the specified location in the two dimensional
    *         space.
    */
  def position(location: String): Option[Point2D]
}

final class LabeledGraph[Vertex, Edge] {
    import scala.collection.mutable
    val globalEdgeLookup = new mutable.LinkedHashMap[Vertex, mutable.LinkedHashMap[Vertex, Edge]]() // TODO: get rid of mutability; ListMap should work
    val vertexLabelsList = new mutable.ArrayBuffer[Vertex]() // TODO: get rid of mutability

    def addVertex(v: Vertex): Unit = {
      checkForNewVertex(v)
    }

    def set(from: Vertex, to: Vertex, edge: Edge): Unit = {
      val localEdgeLookup = checkForNewVertex(from)
      localEdgeLookup.put(to, edge)
      checkForNewVertex(to)
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

  def vertexLabels  =
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


final case class Point2D(x: Double, y: Double)
final case class Distance(value: Double) extends AnyVal
object Point2D {
  def distance(p1: Point2D, p2: Point2D): Distance = {
    val x_distance: Double = (p1.x - p2.x) * (p1.x - p2.x)
    // Distance Between Y Coordinates
    val y_distance: Double = (p1.y - p2.y) * (p1.y - p2.y)
    // Distance Between 2d Points
    val total_distance = math.sqrt(x_distance + y_distance)

    Distance(total_distance)
  }
}


import scala.collection.mutable
class ExtendableMap2D(
                     val links: LabeledGraph[String, Distance],
                     val locationPositions: mutable.LinkedHashMap[String, Point2D]
                     ) extends Map2D {

  def this() = this(new LabeledGraph[String, Distance], new mutable.LinkedHashMap[String, Point2D])


  override def locations: List[String] = links.vertexLabels


  override def locationsLinkedTo(fromLocation: String): List[String] = links.successors(fromLocation)


  override def distance(fromLocation: String, toLocation: String): Option[Distance] = links.get(fromLocation, toLocation)

  override def position(location: String): Option[Point2D] = locationPositions.get(location)

  def clear(): Unit = {
    links.clear()
    locationPositions.clear()
  }

  def clearLinks(): Unit = links.clear()

  /**
    * Add a one-way connection to the map.
    *
    * @param fromLocation
    * the from location.
    * @param toLocation
    * the to location.
    * @param distance
    * the distance between the two given locations.
    */
  def addUnidirectionalLink(fromLocation: String, toLocation: String, distance: Distance): Unit = {
    links.set(fromLocation, toLocation, distance)
  }

  /**
    * Adds a connection which can be traveled in both direction. Internally,
    * such a connection is represented as two one-way connections.
    *
    * @param fromLocation
    * the from location.
    * @param toLocation
    * the to location.
    * @param distance
    * the distance between the two given locations.
    */
  def addBidirectionalLink(fromLocation: String, toLocation: String, distance: Distance): Unit = {
    links.set(fromLocation, toLocation, distance)
    links.set(toLocation, fromLocation, distance)
  }

  /**
    * Remove a one-way connection.
    *
    * @param fromLocation
    * the from location.
    * @param toLocation
    * the to location.
    */
  def removeUnidirectionalLink(fromLocation: String, toLocation: String): Unit  = {
    links.remove(fromLocation, toLocation)
  }

  /**
    * Remove the two corresponding one-way connections.
    *
    * @param fromLocation
    * the from location.
    * @param toLocation
    * the to location.
    */
  def removeBidirectionalLink(fromLocation: String, toLocation: String): Unit  = {
    links.remove(fromLocation, toLocation)
    links.remove(toLocation, fromLocation)
  }


  /**
    * Defines the position of a location with respect to a 2 dimensional
    * coordinate system.
    *
    * @param loc
    * the location.
    * @param pos the x, y position
    */
  def setPosition(loc: String, pos: Point2D): Unit =
    locationPositions.put(loc, pos)

  /**
    * Defines the position of a location within the map. Using this method, one
    * location should be selected as a reference position (<code>dist=0</code>
    * and <code>dir=0</code>) and all the other locations should be placed
    * relative to it.
    *
    * @param loc
    * location name
    * @param dist
    * distance to a reference position
    * @param dir
    * bearing (compass direction) in which the location is seen from
    * the reference position
    */
  def setDistAndDirToRefLocation(loc: String, dist: Distance, dir: Int): Unit = {
    val coords = Point2D(-math.sin(dir * math.Pi / 180.0) * dist.value, math.cos(dir * math.Pi / 180.0) * dist.value)
    links.addVertex(loc)
    locationPositions.put(loc, coords)
  }


}
