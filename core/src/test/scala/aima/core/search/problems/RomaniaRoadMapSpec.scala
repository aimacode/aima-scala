package aima.core.search.problems

import org.specs2.mutable.Specification
import RomaniaRoadMap._
/**
  * @author Shawn Garner
  */
class RomaniaRoadMapSpec extends Specification {

  "Arad name" in {
    Arad.name must_== "Arad"
  }

  "Arad Connections" in {
    edges.getOrElse(Arad, List.empty[Edge]).map(_.node2) must_== List(Timisoara, Zerind)
  }

  "Bucharest Connections" in {
    edges.getOrElse(Bucharest, List.empty[Edge]).map(_.node2) must_== List(Giurgiu, Urziceni, Fagaras, Pitesti)
  }
}
