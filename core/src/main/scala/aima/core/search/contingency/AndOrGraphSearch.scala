package aima.core.search.contingency

import aima.core.fp.Show

import scala.annotation.tailrec
import scala.reflect.ClassTag

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
trait AndOrGraphSearch[ACTION, STATE] {
  implicit val aCT: ClassTag[ACTION]
  implicit val sCT: ClassTag[STATE]
  def andOrGraphSearch(
      problem: NondeterministicProblem[ACTION, STATE]
  ): ConditionPlanResult =
    orSearch(problem.initialState(), problem, Nil)

  def orSearch(
      state: STATE,
      problem: NondeterministicProblem[ACTION, STATE],
      path: List[STATE]
  ): ConditionPlanResult = {
    if (problem.isGoalState(state)) {
      ConditionalPlan.emptyPlan
    } else if (path.contains(state)) {
      ConditionalPlanningFailure
    } else {
      val statePlusPath         = state :: path
      val actions: List[ACTION] = problem.actions(state)

      @tailrec def recurse(a: List[ACTION]): ConditionPlanResult = a match {
        case Nil => ConditionalPlanningFailure
        case action :: rest =>
          andSearch(problem.results(state, action), problem, statePlusPath) match {
            case conditionalPlan: ConditionalPlan =>
              newPlan(action, conditionalPlan)
            case ConditionalPlanningFailure => recurse(rest)
          }
      }

      recurse(actions)
    }
  }

  def andSearch(
      states: List[STATE],
      problem: NondeterministicProblem[ACTION, STATE],
      path: List[STATE]
  ): ConditionPlanResult = {

    @tailrec def recurse(
        currentStates: List[STATE],
        acc: List[(STATE, ConditionalPlan)]
    ): ConditionPlanResult =
      currentStates match {
        case Nil => newPlan(acc)
        case si :: rest =>
          orSearch(si, problem, path) match {
            case ConditionalPlanningFailure => ConditionalPlanningFailure
            case plani: ConditionalPlan     => recurse(rest, acc :+ (si -> plani))
          }
      }

    recurse(states, List.empty)
  }

  def newPlan(l: List[(STATE, ConditionalPlan)]): ConditionalPlan = l match {
    case (_, cp: ConditionalPlan) :: Nil => cp
    case ls =>
      ConditionalPlan(
        ls.map(statePlan => ConditionedSubPlan(statePlan._1, statePlan._2))
      )

  }

  def newPlan(action: ACTION, plan: ConditionalPlan): ConditionalPlan =
    ConditionalPlan(ActionStep(action) :: plan.steps)

}

sealed trait Step
final case class ActionStep[ACTION: ClassTag](action: ACTION) extends Step
final case class ConditionedSubPlan[STATE: ClassTag](
    state: STATE,
    subPlan: ConditionalPlan
) extends Step

sealed trait ConditionPlanResult
case object ConditionalPlanningFailure              extends ConditionPlanResult
final case class ConditionalPlan(steps: List[Step]) extends ConditionPlanResult

object ConditionalPlan {
  val emptyPlan = ConditionalPlan(List.empty)

  object Implicits {
    import Show.Implicits._
    implicit def showConditionalPlan[
        STATE: ClassTag: Show,
        ACTION: ClassTag: Show
    ]: Show[ConditionalPlan] =
      new Show[ConditionalPlan] {

        override def show(conditionalPlan: ConditionalPlan): String = {

          @tailrec def recurse(
              steps: List[Step],
              acc: String,
              lastStepAction: Boolean
          ): String = steps match {
            case Nil => acc
            case ActionStep(a: ACTION) :: Nil =>
              recurse(Nil, acc + a.show, true)
            case ActionStep(a: ACTION) :: rest =>
              recurse(rest, acc + a.show + ", ", true)
            case ConditionedSubPlan(state: STATE, subPlan) :: rest if lastStepAction =>
              recurse(
                rest,
                acc + s"if State = ${state.show} then ${show(subPlan)}",
                false
              )
            case ConditionedSubPlan(_, subPlan) :: Nil =>
              recurse(Nil, acc + s" else ${show(subPlan)}", false)
            case ConditionedSubPlan(_, subPlan) :: ActionStep(a) :: rest =>
              recurse(
                ActionStep(a) :: rest,
                acc + s" else ${show(subPlan)}",
                false
              )
            case ConditionedSubPlan(state: STATE, subPlan) :: rest =>
              recurse(
                rest,
                acc + s" else if State = ${state.show} then ${show(subPlan)}",
                false
              )
          }

          recurse(conditionalPlan.steps, "[", true) + "]"
        }
      }
  }

}

trait NondeterministicProblem[ACTION, STATE] {
  def initialState(): STATE
  def actions(s: STATE): List[ACTION]
  def results(s: STATE, a: ACTION): List[STATE]
  def isGoalState(s: STATE): Boolean
  def stepCost(s: STATE, a: ACTION, childPrime: STATE): Double
}
