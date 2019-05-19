package aima.core.agent.basic

import aima.core.environment.map2d.{
  Distance,
  ExtendableMap2D,
  InState,
  IntPercept,
  Map2DAction,
  Map2DFunctionFactory,
  NoAction
}
import aima.core.fp.Eqv
import aima.core.search.api.OnlineSearchProblem
import org.specs2.mutable.Specification

/**
  * @author Shawn Garner
  */
class LRTAStarAgentSpec extends Specification {
  val distanceOne = Distance(1.0d)
  val mapAtoF = new ExtendableMap2D() {
    addBidirectionalLink("A", "B", distanceOne)
    addBidirectionalLink("B", "C", distanceOne)
    addBidirectionalLink("C", "D", distanceOne)
    addBidirectionalLink("D", "E", distanceOne)
    addBidirectionalLink("E", "F", distanceOne)
  }

  val alphabetPerceptToState: IntPercept => InState =
    percept => InState(new String(Array(('A' + percept.value).toChar)))

  val alphabetStateToPercept: InState => IntPercept =
    inState => IntPercept(inState.location.charAt(0) - 'A')

  "already at goal" in {
    val problem = new OnlineSearchProblem[Map2DAction, InState] {
      override def actions(s: InState): List[Map2DAction] =
        Map2DFunctionFactory.actions(mapAtoF)(s)

      import Eqv.Implicits.stringEq
      override def isGoalState(s: InState): Boolean =
        Eqv[String].eqv("A", s.location)

      override def stepCost(s: InState, a: Map2DAction, sPrime: InState): Double =
        Map2DFunctionFactory.stepCost(mapAtoF)(s, a, sPrime)
    }

    val lrtasa = new LRTAStarAgent[IntPercept, Map2DAction, InState](
      alphabetPerceptToState,
      problem,
      _ => 1.0d,
      NoAction
    )

    val resultAction = lrtasa.agentFunction(IntPercept(0), LRTAStarAgentState[Map2DAction, InState])
    resultAction._1 must_== NoAction
  }

}
