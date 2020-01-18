package aima.core.search.local

import aima.core.search.Problem
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
    val stateToValue: XCoordinate => Double = {
      case XCoordinate(x) => math.sin(x)
    }

    val sinProblem = new Problem[XCoordinate, StepAction] {
      override def initialState: XCoordinate = xCoord
      override def isGoalState(state: XCoordinate): Boolean =
        false // Not used
      override def actions(state: XCoordinate): List[StepAction] =
        List(StepLeft, StepRight)
      override def result(
          state: XCoordinate,
          action: StepAction
      ): XCoordinate = (state, action) match {
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

      override def stepCost(
          state: XCoordinate,
          action: StepAction,
          childPrime: XCoordinate
      ): Int = -1 // Not Used
    }

    val result = HillClimbing(stateToValue)(sinProblem)
    result match {
      case XCoordinate(x) =>
        x must beCloseTo((math.Pi / 2) within 3.significantFigures)
      case other => ko(other.toString)
    }
  }
}

object HillClimbingSpec {
  final case class XCoordinate(x: Double)
  sealed trait StepAction
  case object StepLeft  extends StepAction
  case object StepRight extends StepAction
}
