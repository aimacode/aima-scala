package aima.core.agent.basic

import aima.core.environment.map2d.{
  Distance,
  ExtendableMap2D,
  Go,
  InState,
  IntPercept,
  Map2DAction,
  Map2DFunctionFactory,
  NoOp
}
import aima.core.fp.Eqv
import aima.core.search.api.OnlineSearchProblem
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
class LRTAStarAgentSpec extends Specification with ScalaCheck {
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

      override def stepCost(
          s: InState,
          a: Map2DAction,
          sPrime: InState
      ): Double =
        Map2DFunctionFactory.stepCost(mapAtoF)(s, a, sPrime)
    }

    val lrtasa = new LRTAStarAgent[IntPercept, Map2DAction, InState](
      alphabetPerceptToState,
      problem,
      _ => 1.0d,
      NoOp
    )

    val resultAction = lrtasa.agentFunction(
      IntPercept(0),
      LRTAStarAgentState[Map2DAction, InState]
    )
    resultAction._1 must_== NoOp
  }

  "normal search A to F" in {
    val problem = new OnlineSearchProblem[Map2DAction, InState] {
      override def actions(s: InState): List[Map2DAction] =
        Map2DFunctionFactory.actions(mapAtoF)(s)

      import Eqv.Implicits.stringEq
      override def isGoalState(s: InState): Boolean =
        Eqv[String].eqv("F", s.location)

      override def stepCost(
          s: InState,
          a: Map2DAction,
          sPrime: InState
      ): Double =
        Map2DFunctionFactory.stepCost(mapAtoF)(s, a, sPrime)
    }

    val lrtasa = new LRTAStarAgent[IntPercept, Map2DAction, InState](
      alphabetPerceptToState,
      problem,
      inState => ('F' - inState.location.charAt(0)).toDouble,
      NoOp
    )

    val actions = actionSequence(lrtasa, alphabetStateToPercept(InState("A")))
    actions must_==
      List(
        Go("B"),
        Go("A"),
        Go("B"),
        Go("C"),
        Go("B"),
        Go("C"),
        Go("D"),
        Go("C"),
        Go("D"),
        Go("E"),
        Go("D"),
        Go("E"),
        Go("F"),
        NoOp
      )

  }

  "normal search F to A" in {
    val problem = new OnlineSearchProblem[Map2DAction, InState] {
      override def actions(s: InState): List[Map2DAction] =
        Map2DFunctionFactory.actions(mapAtoF)(s)

      import Eqv.Implicits.stringEq
      override def isGoalState(s: InState): Boolean =
        Eqv[String].eqv("A", s.location)

      override def stepCost(
          s: InState,
          a: Map2DAction,
          sPrime: InState
      ): Double =
        Map2DFunctionFactory.stepCost(mapAtoF)(s, a, sPrime)
    }

    val lrtasa = new LRTAStarAgent[IntPercept, Map2DAction, InState](
      alphabetPerceptToState,
      problem,
      inState => (inState.location.charAt(0) - 'A').toDouble,
      NoOp
    )

    val actions = actionSequence(lrtasa, alphabetStateToPercept(InState("F")))
    actions must_==
      List(
        Go("E"),
        Go("D"),
        Go("C"),
        Go("B"),
        Go("A"),
        NoOp
      )

  }

  implicit val arbInState: Arbitrary[InState] = Arbitrary {
    for {
      perceptInt <- Gen.choose(0, 5)
    } yield alphabetPerceptToState(IntPercept(perceptInt))
  }

  "find solutions for all start and goal states" >> prop { (initialState: InState, goalState: InState) =>
    val problem = new OnlineSearchProblem[Map2DAction, InState] {
      override def actions(s: InState): List[Map2DAction] =
        Map2DFunctionFactory.actions(mapAtoF)(s)

      import Eqv.Implicits.stringEq
      override def isGoalState(s: InState): Boolean =
        Eqv[String].eqv(goalState.location, s.location)

      override def stepCost(
          s: InState,
          a: Map2DAction,
          sPrime: InState
      ): Double =
        Map2DFunctionFactory.stepCost(mapAtoF)(s, a, sPrime)
    }

    val lrtasa = new LRTAStarAgent[IntPercept, Map2DAction, InState](
      alphabetPerceptToState,
      problem,
      inState =>
        math
          .abs(inState.location.charAt(0) - goalState.location.charAt(0))
          .toDouble,
      NoOp
    )

    val actions = actionSequence(lrtasa, alphabetStateToPercept(initialState))
    import Eqv.Implicits.stringEq
    if (Eqv[String].eqv(initialState.location, goalState.location)) {
      actions must_== List(NoOp)
    } else {
      actions must contain[Map2DAction](Go(goalState.location))
    }

  }

  def actionSequence(
      lrtasa: LRTAStarAgent[IntPercept, Map2DAction, InState],
      initialPercept: IntPercept
  ): List[Map2DAction] = {
    @tailrec def findActions(
        agentState: LRTAStarAgentState[Map2DAction, InState],
        percept: IntPercept,
        acc: List[Map2DAction]
    ): List[Map2DAction] = {
      val (action, updatedAgentState) =
        lrtasa.agentFunction(percept, agentState)

      action match {
        case NoOp => action :: acc
        case a @ Go(goTo) =>
          val nextPercept = alphabetStateToPercept(InState(goTo))
          findActions(updatedAgentState, nextPercept, a :: acc)
      }
    }

    findActions(LRTAStarAgentState[Map2DAction, InState], initialPercept, Nil).reverse
  }

}
