package aima.core.search.uninformed

import aima.core.search.{Problem, ProblemSearch, StateNode}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait DLSResult[Action] {
  def actions: List[Action]
}

final case class Solution[Action](actions: List[Action]) extends DLSResult[Action]
final case class CutOff[Action](actions: List[Action])   extends DLSResult[Action]

/**
  * @author Shawn Garner
  */
trait DepthLimitedTreeSearch[State, Action] extends ProblemSearch[State, Action, StateNode[State, Action]] {

  type Node = StateNode[State, Action]

  def search(problem: Problem[State, Action], initialLimit: Int, noAction: Action): Try[DLSResult[Action]] =
    Try {

      def recursiveDLS(node: Node, currentLimit: Int): Try[DLSResult[Action]] =
        Try {
          if (problem.isGoalState(node.state)) {
            Success(Solution(solution(node)))
          } else if (currentLimit == 0) {
            Success(CutOff(solution(node)))
          } else {
            val childNodes = for {
              action <- problem.actions(node.state)
            } yield newChildNode(problem, node, action)

            @tailrec def shortCircuitChildSearch(children: List[Node]): Try[DLSResult[Action]] = {
              children match {
                case Nil => Failure[DLSResult[Action]](new Exception("Exhausted child nodes"))
                case lastChild :: Nil =>
                  recursiveDLS(lastChild, currentLimit - 1)
                case firstChild :: rest =>
                  recursiveDLS(firstChild, currentLimit - 1) match {
                    case result @ Success(Solution(_)) => result
                    case _                             => shortCircuitChildSearch(rest)
                  }
              }
            }

            shortCircuitChildSearch(childNodes)
          }
        }.flatten

      recursiveDLS(makeNode(problem.initialState, noAction), initialLimit)
    }.flatten

  def makeNode(state: State, noAction: Action): Node = StateNode(state, noAction, None)

  def newChildNode(problem: Problem[State, Action], parent: Node, action: Action): Node = {
    val childState = problem.result(parent.state, action)
    StateNode(childState, action, Some(parent))
  }
}
