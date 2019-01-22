package aima.core.search.local

import aima.core.search.local.time.TimeStep
import aima.core.search.{Problem, State}

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

  type Schedule = TimeStep => Double

  object BasicSchedule {

    val k: Int      = 20
    val lam: Double = 0.045
    val limit: Int  = 100

    val schedule: Schedule = { t: TimeStep => // time steps 1 to infinity (Integer.Max)
      if (t.value < limit) {
        k * math.exp((-1) * lam * t.value)
      } else {
        0.0
      }
    }
  }

  final case class StateValueNode(state: State, value: Double) extends State

  def apply(stateToValue: State => Double, problem: Problem): Try[State] =
    apply(stateToValue, problem, BasicSchedule.schedule)

  def apply(stateToValue: State => Double, problem: Problem, sched: Schedule): Try[State] = {
    val random = new Random()

    def makeNode(state: State): StateValueNode = StateValueNode(state, stateToValue(state))

    def schedule(t: TimeStep): Try[Double] = {
      val T = sched(t)
      if (T < 0.0d) {
        Failure(new IllegalArgumentException("Configured schedule returns negative temperatures: t=" + t + ", T=" + T)) // TODO: seems like smart constructor of Temperature type
      } else {
        Success(T)
      }
    }

    def randomlySelectSuccessor(current: StateValueNode): StateValueNode = {
      // Default successor to current, so that in the case we reach a dead-end
      // state i.e. one without reversible actions we will return something.
      // This will not break the code above as the loop will exit when the
      // temperature winds down to 0.
      val actions = problem.actions(current.state)
      val successor = {
        if (actions.nonEmpty) {
          makeNode(problem.result(current.state, actions(random.nextInt(actions.size))))
        } else {
          current
        }
      }

      successor
    }

    def recurse(current: StateValueNode, t: TimeStep): Try[State] = {
      import time.TimeStep.Implicits._

      for {
        temperatureT <- schedule(t)
        result <- {
          if (temperatureT == 0.0d) { //TODO: don't think this is good practice to compare 0.0 double against constant, could be really close but not exact
            Success(current)
          } else {
            val randomSuccessor         = randomlySelectSuccessor(current)
            val DeltaE                  = randomSuccessor.value - current.value
            lazy val acceptDownHillMove = math.exp(DeltaE / temperatureT) > random.nextDouble()

            val nextNode = {
              if (DeltaE > 0.0d || acceptDownHillMove) {
                randomSuccessor
              } else {
                current
              }
            }

            val nextTimeStep: Try[TimeStep] = t.step
            nextTimeStep.flatMap(recurse(nextNode, _))
          }
        }
      } yield result

    }

    recurse(makeNode(problem.initialState), TimeStep.start)
  }
}
