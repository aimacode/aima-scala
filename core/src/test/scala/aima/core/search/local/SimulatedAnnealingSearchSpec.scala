package aima.core.search.local

import aima.core.search.Problem
import aima.core.search.local.SimulatedAnnealingSearch.{
  BasicSchedule,
  OverTimeStepLimit,
  Temperature,
  TemperatureResult
}
import aima.core.search.local.time.TimeStep
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.{Success, Try}

/**
  * @author Shawn Garner
  */
class SimulatedAnnealingSearchSpec extends Specification with ScalaCheck {

  "BasicSchedule" >> {
    import aima.core.search.local.SimulatedAnnealingSearch.BasicSchedule.schedule
    import aima.core.search.local.time.TimeStep.Implicits._

    def increment(ts: TimeStep, times: Int): Try[TimeStep] = times match {
      case 0 => Success(ts)
      case _ => ts.step.flatMap(increment(_, times - 1))
    }

    "lower limit check" in {
      schedule()(TimeStep.start) match {
        case Temperature(t) =>
          t must beCloseTo(19.1d within 3.significantFigures)
        case other => ko(other.toString)
      }
    }

    "upper limit check" in {
      increment(TimeStep.start, 98).map(schedule()(_)) match {
        case Success(Temperature(t)) =>
          t must beCloseTo(0.232d within 3.significantFigures)
        case other => ko(other.toString)
      }

    }

    "over limit check" in {
      increment(TimeStep.start, 99)
        .map(schedule()(_)) must beSuccessfulTry[TemperatureResult](
        OverTimeStepLimit
      )
    }

    implicit val arbTimeStep: Arbitrary[TimeStep] = Arbitrary {
      for {
        numSteps <- Gen.choose[Int](0, 98)
      } yield increment(TimeStep.start, numSteps).get
    }

    "must not create negative temperature" >> prop { ts: TimeStep =>
      schedule()(ts) match {
        case Temperature(t) => t must be_>(0.00d)
        case other          => ko(other.toString)
      }
    }

  }

  "8 queens problem problem" >> {
    import SimulatedAnnealingSearchSpec._
    implicit val arbTimeStep: Arbitrary[EightQueensState] = Arbitrary {
      val all = (0 to 7).toList
      for {
        q0row <- Gen.oneOf(all)
        l1    = all.filterNot(_ == q0row)
        q1row <- Gen.oneOf(l1)
        l2    = l1.filterNot(_ == q1row)
        q2row <- Gen.oneOf(l2)
        l3    = l2.filterNot(_ == q2row)
        q3row <- Gen.oneOf(l3)
        l4    = l3.filterNot(_ == q3row)
        q4row <- Gen.oneOf(l4)
        l5    = l4.filterNot(_ == q4row)
        q5row <- Gen.oneOf(l5)
        l6    = l5.filterNot(_ == q5row)
        q6row <- Gen.oneOf(l6)
        l7    = l6.filterNot(_ == q6row)
        q7row <- Gen.oneOf(l7)
      } yield EightQueensState(
        List(
          QueenPosition(q0row),
          QueenPosition(q1row),
          QueenPosition(q2row),
          QueenPosition(q3row),
          QueenPosition(q4row),
          QueenPosition(q5row),
          QueenPosition(q6row),
          QueenPosition(q7row)
        )
      )
    }

    "find solution" >> prop { s: EightQueensState =>
      val eightQueensProblem =
        new Problem[EightQueensState, EightQueensAction] {
          override def initialState = s

          override def isGoalState(state: EightQueensState): Boolean =
            false // Not used

          override def actions(
              state: EightQueensState
          ): List[EightQueensAction] = state match {
            case EightQueensState(cols) =>
              cols.zipWithIndex.flatMap {
                case (QueenPosition(rowIndex), colIndex) =>
                  (0 to 7).toList
                    .filterNot(r => rowIndex == r)
                    .map(newRowIndex => MoveTo(colIndex, newRowIndex))
              }
          }

          override def result(
              state: EightQueensState,
              action: EightQueensAction
          ): EightQueensState =
            (state, action) match {
              case (EightQueensState(cols), MoveTo(colIndex, newRowIndex)) =>
                EightQueensState(
                  cols.updated(colIndex, QueenPosition(newRowIndex))
                )

            }

          override def stepCost(
              state: EightQueensState,
              action: EightQueensAction,
              childPrime: EightQueensState
          ): Int =
            -1 // Not used
        }

      val result =
        SimulatedAnnealingSearch.apply(
          queenStateToValue,
          eightQueensProblem,
          BasicSchedule.schedule(
            BasicSchedule.defaultScheduleParams.copy(limit = 10000)
          )
        )

      result must beSuccessfulTry.like {
        case s @ EightQueensState(_) =>
          queenStateToValue(s) must be beCloseTo (8.00d within 2.significantFigures)
      }

      result.foreach {
        case s @ EightQueensState(cols) =>
          val numQueensRightScore = queenStateToValue(s)
          println(s"*** Score: $numQueensRightScore")
          println(List.fill(8)("-").mkString(" ", " ", " "))

          for {
            currentRow <- 0 to 7
            row = cols.zipWithIndex.map {
              case (QueenPosition(r), _) if r == currentRow => "Q"
              case _                                        => " "
            }
          } {
            println(row.mkString("|", "|", "|"))
            println(List.fill(8)("-").mkString(" ", " ", " "))
          }
      }

      ok
    }.set(minTestsOk = 1)
  }

}

object SimulatedAnnealingSearchSpec {
  sealed trait EightQueensAction
  final case class MoveTo(columnIndex: Int, newRowIndex: Int) extends EightQueensAction

  final case class QueenPosition(row: Int) extends AnyVal
  final case class EightQueensState(columns: List[QueenPosition])

  def canAttackHorizontal(columnIndex: Int, s: EightQueensState): Boolean = {
    val currentRow = s.columns(columnIndex).row
    val rows: List[Int] =
      s.columns.zipWithIndex.filterNot(_._2 == columnIndex).map(_._1.row)
    rows.contains(currentRow)
  }

  def canAttackDiagonal(columnIndex: Int, s: EightQueensState): Boolean = {
    val currentRow = s.columns(columnIndex).row
    val rows: List[Boolean] =
      s.columns.zipWithIndex.filterNot(_._2 == columnIndex).map {
        case (QueenPosition(rowIdx), colIdx) =>
          val run  = math.abs(colIdx - columnIndex)
          val rise = math.abs(rowIdx - currentRow)
          rise == run
      }

    rows.contains(true)
  }

  val queenStateToValue: EightQueensState => Double = {
    case state @ EightQueensState(cols) =>
      cols.zipWithIndex.foldLeft(0.0d) {
        case (acc, elem) =>
          val currentColumnIndex = elem._2
          if (canAttackHorizontal(currentColumnIndex, state) || canAttackDiagonal(
                currentColumnIndex,
                state
              )) {
            acc
          } else {
            acc + 1.0d
          }

      }
  }

}
