package aima.core.search.uninformed

import aima.core.search.StateNode
import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.reflect.ClassTag

/**
  * @author Shawn Garner
  */
class RomaniaIterativeDeepeningSearchSpec extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad)), NoAction) must beSuccessfulTry
      .like {
        case Solution(Nil) => ok
      }
  }

  "going from Arad to Bucharest must return a list of actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), NoAction) must beSuccessfulTry
      .like {
        case Solution(GoTo(Sibiu) :: GoTo(Fagaras) :: GoTo(Bucharest) :: Nil) =>
          ok
      }
  }

  trait context extends Scope with IterativeDeepeningSearch[RomaniaState, RomaniaAction] {

    val depthLimitedTreeSearch =
      new DepthLimitedTreeSearch[RomaniaState, RomaniaAction] {
        override implicit val nCT: ClassTag[StateNode[RomaniaState, RomaniaAction]] = snCTag
      }
  }

}
