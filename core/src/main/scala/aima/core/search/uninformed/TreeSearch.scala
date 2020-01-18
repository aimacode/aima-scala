package aima.core.search.uninformed

import aima.core.search._

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait TreeSearch[State, Action]
    extends ProblemSearch[State, Action, StateNode[State, Action]]
    with FrontierSearch[State, Action, StateNode[State, Action]] {
  type Node = StateNode[State, Action]

  def search(problem: Problem[State, Action], noAction: Action): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState, noAction)

    @tailrec def searchHelper(frontier: Frontier[State, Action, Node]): List[Action] = {
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
