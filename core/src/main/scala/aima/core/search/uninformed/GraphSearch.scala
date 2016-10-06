package aima.core.search.uninformed

import aima.core.agent.Action
import aima.core.search._

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait GraphSearch extends ProblemSearch with FrontierSearch {
  type Node = StateNode
  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Frontier[Node], exploredSet: Set[State] = Set.empty[State]): List[Action] = {
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
}
