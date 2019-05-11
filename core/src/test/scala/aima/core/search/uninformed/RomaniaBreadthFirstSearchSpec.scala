package aima.core.search.uninformed

import aima.core.search.StateNode
import aima.core.search.problems.Romania._
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.reflect.ClassTag

/**
  * @author Shawn Garner
  */
class RomaniaBreadthFirstSearchSpec extends Specification {

  "going from Arad to Arad must return no actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Arad)), NoAction) must beEmpty
  }

  "going from Arad to Bucharest must return a list of actions" in new context {
    search(new RomaniaRoadProblem(In(Arad), In(Bucharest)), NoAction) must_== List(
      GoTo(Sibiu),
      GoTo(Fagaras),
      GoTo(Bucharest)
    )
  }

  trait context extends Scope with BreadthFirstSearch[RomaniaState, RomaniaAction] {

    override implicit val sCT: ClassTag[RomaniaState]                           = sCTag
    override implicit val aCT: ClassTag[RomaniaAction]                          = aCTag
    override implicit val nCT: ClassTag[StateNode[RomaniaState, RomaniaAction]] = snCTag
  }

}
