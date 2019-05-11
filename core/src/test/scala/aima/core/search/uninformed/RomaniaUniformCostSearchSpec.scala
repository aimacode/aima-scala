package aima.core.search.uninformed

import aima.core.search.CostNode
import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.reflect.ClassTag

/**
  * @author Shawn Garner
  */
class RomaniaUniformCostSearchSpec extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad)), NoAction) must beEmpty
  }

  "going from Arad to Bucharest must return a list of actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), NoAction) must_== List(
      GoTo(Sibiu),
      GoTo(RimnicuVilcea),
      GoTo(Pitesti),
      GoTo(Bucharest)
    )
  }

  trait context extends Scope with UniformCostSearch[RomaniaState, RomaniaAction] {
    override implicit val nCT: ClassTag[CostNode[RomaniaState, RomaniaAction]] = cnCTag
  }

}
