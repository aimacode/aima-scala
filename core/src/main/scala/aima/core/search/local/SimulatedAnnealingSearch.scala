package aima.core.search.local

import aima.core.search.local.time.TimeStep
import aima.core.search.Problem

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}
import scala.util.control.NoStackTrace

package time {
  case object UpperLimitExceeded extends RuntimeException with NoStackTrace

  final case class TimeStep private[time] (value: Int) extends AnyVal

  object TimeStep {
    val start = TimeStep(1)

    object Implicits {
      implicit class TimeStepOps(t: TimeStep) {
        def step: Try[TimeStep] = t.value match {
          case Int.MaxValue => Failure(UpperLimitExceeded)
          case v            => Success(TimeStep(v + 1))
        }
      }
    }
  }

}

/**
  * <pre>
  * function SIMULATED-ANNEALING(problem, schedule) returns a solution state
  *   inputs: problem, a problem
  *           schedule, a mapping from time to "temperature"
  *
  *   current &larr; MAKE-NODE(problem.INITIAL-STATE)
  *   for t = 1 to &infin; do
  *     T &larr; schedule(t)
  *     if T = 0 then return current
  *     next &larr; a randomly selected successor of current
  *     &Delta;E &larr; next.VALUE - current.value
  *     if &Delta;E &gt; 0 then current &larr; next
  *     else current &larr; next only with probability e<sup>&Delta;E/T</sup>
  * </pre>
  *
  * @author Shawn Garner
  */
object SimulatedAnnealingSearch {

  sealed trait TemperatureResult
  final case class Temperature private[SimulatedAnnealingSearch] (
      double: Double
  ) extends TemperatureResult
  case object OverTimeStepLimit extends TemperatureResult

  type Schedule = TimeStep => TemperatureResult

  object BasicSchedule {

    final case class ScheduleParams(k: Int, lam: Double, limit: Int)
    val defaultScheduleParams = ScheduleParams(
      k = 20,
      lam = 0.045d,
      limit = 100
    )

    def schedule(params: ScheduleParams = defaultScheduleParams): Schedule = {
      t: TimeStep =>
        import params._
        if (t.value < limit) {
          Temperature(k * math.exp((-1) * lam * t.value))
        } else {
          OverTimeStepLimit
        }
    }
  }

  final case class StateValueNode[State](state: State, value: Double)

  def apply[State, Action](
      stateToValue: State => Double,
      problem: Problem[State, Action]
  ): Try[State] =
    apply(stateToValue, problem, BasicSchedule.schedule())

  def apply[State, Action](
      stateToValue: State => Double,
      problem: Problem[State, Action],
      schedule: Schedule
  ): Try[State] = {
    val random = new Random()

    def makeNode(state: State): StateValueNode[State] =
      StateValueNode(state, stateToValue(state))

    def randomlySelectSuccessor(
        current: StateValueNode[State]
    ): StateValueNode[State] = {
      // Default successor to current, so that in the case we reach a dead-end
      // state i.e. one without reversible actions we will return something.
      // This will not break the code above as the loop will exit when the
      // temperature winds down to 0.
      val actions = problem.actions(current.state)
      val successor = {
        if (actions.nonEmpty) {
          makeNode(
            problem.result(current.state, actions(random.nextInt(actions.size)))
          )
        } else {
          current
        }
      }

      successor
    }

    @tailrec def recurse(
        current: StateValueNode[State],
        t: Try[TimeStep]
    ): Try[StateValueNode[State]] = {
      import time.TimeStep.Implicits._
      t match {
        case Failure(f) => Failure(f)
        case Success(timeStep) =>
          val T = schedule(timeStep)
          T match {
            case OverTimeStepLimit => Success(current)

            case Temperature(temperatureT) =>
              val randomSuccessor = randomlySelectSuccessor(current)
              val DeltaE          = randomSuccessor.value - current.value
              lazy val acceptDownHillMove = math.exp(DeltaE / temperatureT) > random
                .nextDouble()

              val nextNode = {
                if (DeltaE > 0.0d || acceptDownHillMove) {
                  randomSuccessor
                } else {
                  current
                }
              }

              recurse(nextNode, timeStep.step)

          }

      }

    }

    recurse(makeNode(problem.initialState), Success(TimeStep.start))
      .map(_.state)
  }
}
