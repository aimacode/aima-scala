package aima.core.search.problems

import org.specs2.mutable.Specification
import Romania._

/**
  * @author Shawn Garner
  */
class RomaniaRoadMapSpec extends Specification {

  "Arad name" in {
    Arad.name must_== "Arad"
  }

  "Arad Connections" in {
    roadsFromCity.getOrElse(Arad, List.empty[Road]).map(_.to) must_== List(
      Sibiu,
      Timisoara,
      Zerind
    )
  }

  "Bucharest Connections" in {
    roadsFromCity.getOrElse(Bucharest, List.empty[Road]).map(_.to) must_== List(
      Giurgiu,
      Urziceni,
      Fagaras,
      Pitesti
    )
  }
}
