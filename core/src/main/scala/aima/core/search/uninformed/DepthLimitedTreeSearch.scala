package aima.core.search.uninformed

import aima.core.agent.{NoAction, Action}
import aima.core.search.{StateNode, State, Problem, ProblemSearch}

import scala.annotation.tailrec
import scala.util.{Success, Failure, Try}

sealed trait DLSResult {
  def actions: List[Action]
}

final case class Solution(actions: List[Action]) extends DLSResult
final case class CutOff(actions: List[Action])   extends DLSResult

/**
  * @author Shawn Garner
  */
trait DepthLimitedTreeSearch extends ProblemSearch {
  type Node = StateNode

  def search(problem: Problem, initialLimit: Int): Try[DLSResult] =
    Try {

      def recursiveDLS(node: Node, currentLimit: Int): Try[DLSResult] =
        Try {
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
        }.flatten

      recursiveDLS(makeNode(problem.initialState), initialLimit)
    }.flatten

  def makeNode(state: State): Node = StateNode(state, NoAction, None)

  def newChildNode(problem: Problem, parent: Node, action: Action): Node = {
    val childState = problem.result(parent.state, action)
    StateNode(childState, action, Some(parent))
  }

  def solution(node: Node): List[Action] = {
    @tailrec def solutionHelper(n: Node, actions: List[Action]): List[Action] = {
      n.parent match {
        case None         => actions
        case Some(parent) => solutionHelper(parent, n.action :: actions)
      }
    }

    solutionHelper(node, Nil)
  }
}
