package aima.core.search.uninformed

import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class RomaniaBreadthFirstSearchTest extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad))) must beEmpty
  }

  "going from Arad to Bucharest must return a list of actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest))) must_== List(GoTo(Sibiu), GoTo(Fagaras), GoTo(Bucharest))
  }

  trait context extends Scope with BreadthFirstSearch {


  }



}
