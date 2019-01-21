package aima.core.search.local

import aima.core.agent.Action
import aima.core.search.{Problem, ProblemSearch, State, StateNode}

import scala.annotation.tailrec

/**
  *
  * <pre>
  * function HILL-CLIMBING(problem) returns a state that is a local maximum
  *
  *   current &larr; MAKE-NODE(problem.INITIAL-STATE)
  *   loop do
  *     neighbor &larr; a highest-valued successor of current
  *     if neighbor.VALUE &le; current.VALUE then return current.STATE
  *     current &larr; neighbor
  * </pre>
  * <p>
  *
  * @author Shawn Garner
  */
object HillClimbing {

  final case class StateValueNode(state: State, value: Double) extends State

  def apply(stateToValue: State => Double)(problem: Problem): State = {

    def makeNode(state: State) = StateValueNode(state, stateToValue(state))

    def highestValuedSuccessor(current: StateValueNode): StateValueNode = {
      val successors = problem.actions(current).map(a => problem.result(current, a)).map(makeNode)

      if (successors.isEmpty) {
        current
      } else {
        successors.maxBy(_.value)
      }

    }

    @tailrec def recurse(current: StateValueNode): State = {
      val neighbor = highestValuedSuccessor(current)
      if (neighbor.value <= current.value) {
        current.state
      } else {
        recurse(neighbor)
      }
    }

    recurse(makeNode(problem.initialState))
  }

}
