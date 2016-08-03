package aima.core.search.uninformed

import aima.core.agent.Action
import aima.core.search.{FrontierSearch, State, Problem}

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait GraphSearch extends FrontierSearch {
  case class StateNode(state: State, action: Action, parent: Option[StateNode])
  type Node = StateNode
  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Frontier, exploredSet: Set[State] = Set.empty[State]): List[Action] = {
      frontier.removeLeaf match {
        case None                                               => List.empty[Action]
        case Some((leaf, _)) if problem.isGoalState(leaf.state) => solution(leaf)
        case Some((leaf, updatedFrontier)) =>
          val updatedExploredSet = exploredSet + leaf.state
          val childNodes = for {
            action <- problem.actions(leaf.state)
            childNode = newChildNode(problem, leaf, action)
            if !(updatedExploredSet.contains(childNode.state)
                  || updatedFrontier.contains(childNode.state))
          } yield childNode

          val frontierWithChildNodes = updatedFrontier.addAll(childNodes)

          searchHelper(frontierWithChildNodes, updatedExploredSet)
      }
    }

    searchHelper(initialFrontier)
  }

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
