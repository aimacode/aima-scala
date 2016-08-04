package aima.core.search.uninformed

import aima.core.agent.Action
import aima.core.search._

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait TreeSearch extends ProblemSearch with FrontierSearch {
  type Node = StateNode

  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Frontier[Node]): List[Action] = {
      frontier.removeLeaf match {
        case None                                               => List.empty[Action]
        case Some((leaf, _)) if problem.isGoalState(leaf.state) => solution(leaf)
        case Some((leaf, updatedFrontier)) =>
          val childNodes = for {
            action <- problem.actions(leaf.state)
          } yield newChildNode(problem, leaf, action)

          val frontierWithChildNodes = updatedFrontier.addAll(childNodes)

          searchHelper(frontierWithChildNodes)
      }
    }

    searchHelper(initialFrontier)
  }

}
