package aima.core.search.local

import aima.core.search.{Problem, State}

import scala.annotation.tailrec
import scala.util.Random

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

  type Schedule = Int => Double

  object BasicSchedule {

    val k: Int      = 20
    val lam: Double = 0.045
    val limit: Int  = 100

    val schedule: Schedule = { t: Int => // time steps 1 to infinity (Integer.Max)
      if (t < limit) {
        k * math.exp((-1) * lam * t)
      } else {
        0.0
      }
    }
  }

  final case class StateValueNode(state: State, value: Double) extends State

  def apply(stateToValue: State => Double, problem: Problem): State =
    apply(stateToValue, problem, BasicSchedule.schedule)

  def apply(stateToValue: State => Double, problem: Problem, sched: Schedule): State = {
    val random = new Random()

    def makeNode(state: State): StateValueNode = StateValueNode(state, stateToValue(state))

    def schedule(t: Int): Double = {
      val T = sched(t)
      if (T < 0.0d) {
        throw new IllegalArgumentException("Configured schedule returns negative temperatures: t=" + t + ", T=" + T) // TODO: we don't throw in Scala
      }
      T
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

    @tailrec def recurse(current: StateValueNode, t: Int): State = {
      val T = schedule(t)
      if (T == 0.0d) {
        current
      } else {
        val next                    = randomlySelectSuccessor(current)
        val DeltaE                  = next.value - current.value
        lazy val acceptDownHillMove = math.exp(DeltaE / T) > random.nextDouble()
        if (DeltaE > 0.0d || acceptDownHillMove) {
          recurse(next, t + 1)
        } else {
          recurse(current, t + 1)
        }
      }
    }

    recurse(makeNode(problem.initialState), 1)
  }
}
