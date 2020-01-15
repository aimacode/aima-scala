package aima.core.search.uninformed

import aima.core.search.StateNode
import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.reflect.ClassTag

/**
  * @author Shawn Garner
  */
class RomaniaDepthLimitedTreeSearchSpec extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad)), 1, NoAction) must beSuccessfulTry
      .like {
        case Solution(Nil) => ok
      }
  }

  "going from Arad to Bucharest must return a list of actions with depth 19" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), 19, NoAction) must beSuccessfulTry
      .like {
        case Solution(
            GoTo(Sibiu) :: GoTo(RimnicuVilcea) :: GoTo(Pitesti) :: GoTo(
              Bucharest
            ) :: Nil
            ) =>
          ok
      }
  }

  "going from Arad to Bucharest must return a list of actions with depth 9" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), 9, NoAction) must beSuccessfulTry
      .like {
        case Solution(
            GoTo(Sibiu) :: GoTo(RimnicuVilcea) :: GoTo(Pitesti) :: GoTo(
              Bucharest
            ) :: Nil
            ) =>
          ok
      }
  }

  "going from Arad to Bucharest must return a Cuttoff with depth 1" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), 1, NoAction) must beSuccessfulTry
      .like {
        case CutOff(GoTo(Zerind) :: Nil) => ok
      }
  }

  trait context
      extends Scope
      with DepthLimitedTreeSearch[RomaniaState, RomaniaAction] {
    override implicit val nCT
        : ClassTag[StateNode[RomaniaState, RomaniaAction]] = snCTag
  }

}
