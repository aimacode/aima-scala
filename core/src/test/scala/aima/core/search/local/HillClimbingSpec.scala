package aima.core.search.local

import aima.core.agent.Action
import aima.core.search.{Problem, State}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
  * @author Shawn Garner
  */
class HillClimbingSpec extends Specification with ScalaCheck {
  import HillClimbingSpec._

  implicit val arbXCoordinate: Arbitrary[XCoordinate] = Arbitrary {
    for {
      x <- Gen.choose[Double](0.00d, math.Pi)
    } yield XCoordinate(x)
  }

  "must find pi/2 on sin graph between zero and pi" >> prop { xCoord: XCoordinate =>
    val stateToValue: State => Double = {
      case XCoordinate(x) => math.sin(x)
    }

    val sinProblem = new Problem {
      override def initialState: State                 = xCoord
      override def isGoalState(state: State): Boolean  = false // Not used
      override def actions(state: State): List[Action] = List(StepLeft, StepRight)
      override def result(state: State, action: Action): State = (state, action) match {
        case (XCoordinate(x), StepLeft) =>
          val newX = x - 0.001d
          if (x < 0) {
            XCoordinate(0)
          } else {
            XCoordinate(newX)
          }

        case (XCoordinate(x), StepRight) =>
          val newX = x + 0.001d
          if (x > math.Pi) {
            XCoordinate(math.Pi)
          } else {
            XCoordinate(newX)
          }
      }

      override def stepCost(state: State, action: Action, childPrime: State): Int = -1 // Not Used
    }

    val result = HillClimbing(stateToValue)(sinProblem)
    result match {
      case XCoordinate(x) => x must beCloseTo((math.Pi / 2) within 3.significantFigures)
      case other          => ko(other.toString)
    }
  }
}

object HillClimbingSpec {
  final case class XCoordinate(x: Double) extends State
  sealed trait StepAction                 extends Action
  case object StepLeft                    extends StepAction
  case object StepRight                   extends StepAction
}
