package aima.core.search.uninformed

import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class RomaniaIterativeDeepeningSearchSpec extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad))) must beSuccessfulTry.like {
      case Solution(Nil) => ok
    }
  }

  "going from Arad to Bucharest must return a list of actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest))) must beSuccessfulTry.like {
      case Solution(GoTo(Sibiu) :: GoTo(Fagaras) :: GoTo(Bucharest) :: Nil) => ok
    }
  }

  trait context extends Scope with IterativeDeepeningSearch {

    lazy val depthLimitedTreeSearch = new DepthLimitedTreeSearch {}
  }

}
