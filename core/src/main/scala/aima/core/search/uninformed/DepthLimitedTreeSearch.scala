package aima.core.search.uninformed

import aima.core.agent.Action

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

/**
  * @author Shawn Garner
  */
trait DepthLimitedTreeSearch extends ProblemSearch {
  case class StateNode(state: State)
  type Node = StateNode

  sealed trait DLSResult {
    def actions: List[Action]
  }

  case class Solution(actions: List[Action]) extends DLSResult
  case class CutOff(actions: List[Action])   extends DLSResult

  def search(problem: Problem, initialLimit: Int): Try[DLSResult] = {

    def recursiveDLS(node: Node, currentLimit: Int): Try[DLSResult] = {
      if (problem.isGoalState(node.state)) {
        Success(Solution(solution(node)))
      } else if (currentLimit == 0) {
        Success(CutOff(solution(node)))
      } else {
        val childNodes = for {
          action <- problem.actions(node.state)
        } yield newChildNode(problem, node, action)

        @tailrec def shortCircuitChildSearch(children: List[Node]): Try[DLSResult] = {
          children match {
            case Nil => Failure[DLSResult](new Exception("Exhausted child nodes"))
            case lastChild :: Nil =>
              recursiveDLS(lastChild, currentLimit - 1)
            case firstChild :: rest =>
              recursiveDLS(firstChild, currentLimit - 1) match {
                case result @ Success(s: Solution) => result
                case _                             => shortCircuitChildSearch(rest)
              }
          }
        }

        shortCircuitChildSearch(childNodes)
      }
    }

    recursiveDLS(makeNode(problem.initialState), initialLimit)
  }

  def makeNode(state: State): Node = StateNode(state)

}
