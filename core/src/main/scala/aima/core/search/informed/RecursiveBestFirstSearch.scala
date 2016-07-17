package aima.core.search.informed

import aima.core.agent.Action
import aima.core.search.{State, ProblemSearch, Problem}

import scala.annotation.tailrec

sealed trait RBFSearchResult
final case class Solution(actions: List[Action])     extends RBFSearchResult
final case class SearchFailure(updatedFCost: Double) extends RBFSearchResult

/**
  * @author Shawn Garner
  */
trait RecursiveBestFirstSearch extends ProblemSearch {
  case class HeuristicsNode(state: State, gValue: Double, hValue: Option[Double], fValue: Option[Double])
  type Node      = HeuristicsNode
  type Heuristic = Node => Double

  def h: Heuristic

  def search(problem: Problem): RBFSearchResult = {
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
          val updatedSuccessors = successors.collect {
            case s @ HeuristicsNode(_, gValue, Some(hValue), _) =>
              val updatedFValue = node.fValue.map(nodeFValue => math.max(gValue + hValue, nodeFValue))
              s.copy(fValue = updatedFValue)
          }

          val sorted = updatedSuccessors.sortBy(_.fValue.getOrElse(Double.MaxValue))

          @tailrec def getBestFValue(sortedSuccessors: List[HeuristicsNode]): RBFSearchResult = {
            sortedSuccessors match {
              case HeuristicsNode(_, _, _, Some(fValue)) :: rest if fValue > fLimit => SearchFailure(fValue)
              case best :: HeuristicsNode(state, gValue, hValue, Some(fValue)) :: rest =>
                val result = rbfs(best, math.min(fLimit, fValue))
                result match {
                  case s: Solution => s
                  case SearchFailure(updatedFValue) =>
                    getBestFValue(best.copy(fValue = Some(updatedFValue)) :: HeuristicsNode(state,
                                                                                            gValue,
                                                                                            hValue,
                                                                                            Some(fValue)) :: rest)
                }
            }
          }

          getBestFValue(sorted)
        }
      }
    }

    rbfs(makeNode(problem.initialState), Double.PositiveInfinity)
  }

  def makeNode(state: State): Node = {
    val basic          = HeuristicsNode(state = state, gValue = 0, hValue = None, fValue = None)
    val hValue: Double = h(basic)
    val fValue: Double = basic.gValue + hValue
    basic.copy(hValue = Some(hValue), fValue = Some(fValue))
  }
}
