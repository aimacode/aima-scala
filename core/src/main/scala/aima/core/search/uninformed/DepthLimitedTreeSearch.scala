package aima.core.search.uninformed

import aima.core.agent.Action

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
  case class CutOff(actions: List[Action]) extends DLSResult


  def search(problem: Problem, initialLimit: Int): Try[DLSResult] = {

    def recursiveDLS(node: Node, currentLimit: Int): Try[DLSResult] = {
      if(problem.isGoalState(node.state)) {
        Success(Solution(solution(node)))
      } else if (currentLimit == 0) {
        Success(CutOff(solution(node)))
      } else {
        val results = for {
          action <- problem.actions(node.state)
          child = newChildNode(problem, node, action)
        } yield recursiveDLS(child, currentLimit - 1)

        val solution = results.find {
          case Success(s: Solution) => true
          case _ => false
        }
        lazy val cutoff = results.find {
          case Success(c: CutOff) => true
          case _ => false
        }
        lazy val failure = results.find {
          case f: Failure[_] => true
          case _ => false
        }

        solution.getOrElse {
          cutoff.getOrElse {
            failure.getOrElse {
              Failure[DLSResult](new Exception("Could not find solution, cutoff, or failure"))
            }
          }
        }
      }
    }

    recursiveDLS(makeNode(problem.initialState), initialLimit)
  }

  def makeNode(state: State): Node = StateNode(state)


}
