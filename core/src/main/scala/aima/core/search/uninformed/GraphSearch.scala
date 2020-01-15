package aima.core.search.uninformed

import aima.core.search._

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * @author Shawn Garner
  */
trait GraphSearch[State, Action]
    extends ProblemSearch[State, Action, StateNode[State, Action]]
    with FrontierSearch[State, Action, StateNode[State, Action]] {
  implicit val sCT: ClassTag[State]
  implicit val aCT: ClassTag[Action]

  type Node = StateNode[State, Action]

  def search(
      problem: Problem[State, Action],
      noAction: Action
  ): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState, noAction)

    @tailrec def searchHelper(
        frontier: Frontier[State, Action, Node],
        exploredSet: Set[State] = Set.empty[State]
    ): List[Action] = {
      frontier.removeLeaf match {
        case None => List.empty[Action]
        case Some((leaf, _)) if problem.isGoalState(leaf.state) =>
          solution(leaf)
        case Some((leaf, updatedFrontier)) =>
          val updatedExploredSet = exploredSet + leaf.state
          val childNodes = for {
            action    <- problem.actions(leaf.state)
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

  def newChildNode(
      problem: Problem[State, Action],
      parent: Node,
      action: Action
  ): Node = {
    val childState = problem.result(parent.state, action)
    StateNode(childState, action, Some(parent))
  }
}
