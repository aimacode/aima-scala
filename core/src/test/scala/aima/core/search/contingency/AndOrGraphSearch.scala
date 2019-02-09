package aima.core.search.contingency

import aima.core.agent.Action
import aima.core.search.State

import scala.annotation.tailrec

/**
  *
  * <pre>
  * <code>
  * function AND-OR-GRAPH-SEARCH(problem) returns a conditional plan, or failure
  *   OR-SEARCH(problem.INITIAL-STATE, problem, [])
  *
  * ---------------------------------------------------------------------------------
  *
  * function OR-SEARCH(state, problem, path) returns a conditional plan, or failure
  *   if problem.GOAL-TEST(state) then return the empty plan
  *   if state is on path then return failure
  *   for each action in problem.ACTIONS(state) do
  *       plan <- AND-SEARCH(RESULTS(state, action), problem, [state | path])
  *       if plan != failure then return [action | plan]
  *   return failure
  *
  * ---------------------------------------------------------------------------------
  *
  * function AND-SEARCH(states, problem, path) returns a conditional plan, or failure
  *   for each s<sub>i</sub> in states do
  *      plan<sub>i</sub> <- OR-SEARCH(s<sub>i</sub>, problem, path)
  *      if plan<sub>i</sub> = failure then return failure
  *   return [if s<sub>1</sub> then plan<sub>1</sub> else if s<sub>2</sub> then plan<sub>2</sub> else ... if s<sub>n-1</sub> then plan<sub>n-1</sub> else plan<sub>n</sub>]
  * </code>
  * </pre>
  *
  * @author Shawn Garner
  */
trait AndOrGraphSearch {
  def andOrGraphSearch(problem: NondeterministicProblem[Action, State]): ConditionPlanResult =
    orSearch(problem.initialState(), problem, Nil)

  def orSearch(state: State, problem: NondeterministicProblem[Action, State], path: List[State]): ConditionPlanResult = {
    if (problem.isGoalState(state)) {
      ConditionalPlan.emptyPlan
    } else if (path.contains(state)) {
      ConditionalPlanningFailure
    } else {
      val statePlusPath         = state :: path
      val actions: List[Action] = problem.actions(state)
      @tailrec def recurse(a: List[Action]): ConditionPlanResult = a match {
        case Nil => ConditionalPlanningFailure
        case action :: rest =>
          andSearch(problem.results(state, action), problem, statePlusPath) match {
            case ConditionalPlan(v)         => ConditionalPlan(v + (state -> action))
            case ConditionalPlanningFailure => recurse(rest)
          }
      }

      recurse(actions)
    }
  }

  def andSearch(states: List[State],
                problem: NondeterministicProblem[Action, State],
                path: List[State]): ConditionPlanResult = {

  }
}

sealed trait ConditionPlanResult
case object ConditionalPlanningFailure                      extends ConditionPlanResult
final case class ConditionalPlan(value: Map[State, Action]) extends ConditionPlanResult

object ConditionalPlan {
  val emptyPlan = ConditionalPlan(Map.empty)
}

  /*

	// function AND-SEARCH(states, problem, path) returns a conditional plan, or
	// failure
	public ConditionalPlan<A, S> andSearch(List<S> states, NondeterministicProblem<A, S> problem, List<S> path) {
		List<Pair<S, ConditionalPlan<A, S>>> conditionedPlans = new ArrayList<>();
		// for each s<sub>i</sub> in states do
		for (S s_i : states) {
			// plan<sub>i</sub> <- OR-SEARCH(s<sub>i</sub>, problem, path)
			ConditionalPlan<A, S> plan_i = orSearch(s_i, problem, path);
			// if plan<sub>i</sub> = failure then return failure
			if (plan_i == failure()) {
				return failure();
			}
			conditionedPlans.add(new Pair<>(s_i, plan_i));
		}
		// return [if s<sub>1</sub> then plan<sub>1</sub> else if s<sub>2</sub>
		// then plan<sub>2</sub> else ... if s<sub>n-1</sub> then
		// plan<sub>n-1</sub> else plan<sub>n</sub>]
		return newPlan(conditionedPlans);
	}
 */

trait NondeterministicProblem[ACTION, STATE] {
  def initialState(): STATE
  def actions(s: STATE): List[ACTION]
  def results(s: STATE, a: ACTION): List[STATE]
  def isGoalState(s: STATE): Boolean
  def stepCost(s: STATE, a: ACTION, childPrime: STATE): Double
}
