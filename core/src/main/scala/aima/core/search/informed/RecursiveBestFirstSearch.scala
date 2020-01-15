package aima.core.search.informed

import aima.core.search.{HeuristicsNode, Problem, ProblemSearch}
import Ordering.Double.TotalOrdering

import scala.annotation.tailrec
import scala.reflect.ClassTag

sealed trait RBFSearchResult
final case class Solution[Action](actions: List[Action]) extends RBFSearchResult
final case class SearchFailure(updatedFCost: Double)     extends RBFSearchResult

/**
  * @author Shawn Garner
  */
trait RecursiveBestFirstSearch[State, Action]
    extends ProblemSearch[State, Action, HeuristicsNode[State, Action]] {
  implicit val sCT: ClassTag[State]
  implicit val aCT: ClassTag[Action]

  type Node      = HeuristicsNode[State, Action]
  type Heuristic = Node => Double

  def search(
      problem: Problem[State, Action],
      noAction: Action,
      h: Heuristic
  ): RBFSearchResult = {
    def rbfs(node: Node, fLimit: Double): RBFSearchResult = {
      if (problem.isGoalState(node.state))
        Solution(solution(node))
      else {
        val successors = for {
          action <- problem.actions(node.state)
        } yield newChildNode(problem, node, action)

        if (successors.isEmpty)
          SearchFailure(Double.PositiveInfinity)
        else {
          val updated = successors.collect {
            case s @ HeuristicsNode(_, gValue, Some(hValue), _, _, _) =>
              val updatedFValue = node.fValue.map(
                nodeFValue => math.max(gValue + hValue, nodeFValue)
              )
              s.copy(fValue = updatedFValue)
          }

          @tailrec def getBestFValue(
              updatedSuccessors: List[Node]
          ): RBFSearchResult = {
            val sortedSuccessors =
              updatedSuccessors.sortBy(_.fValue.getOrElse(Double.MaxValue))
            sortedSuccessors match {
              case HeuristicsNode(_, _, _, Some(fValue), _, _) :: _
                  if fValue > fLimit =>
                SearchFailure(fValue)
              case best :: (second @ HeuristicsNode(
                    _,
                    _,
                    _,
                    Some(fValue),
                    _,
                    _
                  )) :: rest =>
                val result = rbfs(best, math.min(fLimit, fValue))
                result match {
                  case s: Solution[Action] => s
                  case SearchFailure(updatedFValue) =>
                    getBestFValue(
                      best.copy(fValue = Some(updatedFValue)) :: second :: rest
                    )
                }
            }
          }

          getBestFValue(updated)
        }
      }
    }

    rbfs(makeNode(problem.initialState, noAction, h), Double.PositiveInfinity)
  }

  def makeNode(state: State, noAction: Action, h: Heuristic): Node = {
    val basic = HeuristicsNode(
      state = state,
      gValue = 0,
      hValue = None,
      fValue = None,
      noAction,
      None
    )
    val hValue: Double = h(basic)
    val fValue: Double = basic.gValue + hValue
    basic.copy(hValue = Some(hValue), fValue = Some(fValue))
  }
}
