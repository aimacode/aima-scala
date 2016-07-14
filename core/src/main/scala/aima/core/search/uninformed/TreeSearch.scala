package aima.core.search.uninformed

import aima.core.agent.Action

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * @author Shawn Garner
  */
trait TreeSearch extends ProblemSearch {

  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Queue[Node]): List[Action] = {
      frontier.dequeueOption match {
        case None => List.empty[Action]
        case Some((leaf, _)) if problem.isGoalState(leaf.state) => solution(leaf)
        case Some((leaf, updatedFrontier)) =>

          val childNodes = for {
            action <- problem.actions(leaf.state)
            childNode = newChildNode(problem, leaf, action)
          } yield childNode

          val frontierWithChildNodes = updatedFrontier.enqueue(childNodes)

          searchHelper(frontierWithChildNodes)
      }
    }

    searchHelper(initialFrontier)
  }


}


