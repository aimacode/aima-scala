package aima.core.search.uninformed

import aima.core.agent.{NoAction, Action}
import aima.core.search._

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait UniformCostSearch extends ProblemSearch with FrontierSearch {

  type Node = CostNode

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
          } yield newChildNode(problem, leaf, action)

          val frontierWithChildNodes = childNodes.foldLeft(updatedFrontier) { (accFrontier, childNode) =>
            if (!(updatedExploredSet.contains(childNode.state) || accFrontier.contains(childNode.state))) {
              accFrontier.add(childNode)
            } else if (accFrontier
                         .getNode(childNode.state)
                         .exists(existingNode => existingNode.cost > childNode.cost)) {
              accFrontier.replaceByState(childNode)
            } else {
              accFrontier
            }
          }

          searchHelper(frontierWithChildNodes, updatedExploredSet)
      }
    }

    searchHelper(initialFrontier)
  }

  def newFrontier(state: State) = {
    val costNodeOrdering: Ordering[CostNode] = new Ordering[CostNode] {
      def compare(n1: CostNode, n2: CostNode): Int = Ordering.Int.reverse.compare(n1.cost, n2.cost)
    }
    new PriorityQueueHashSetFrontier[CostNode](CostNode(state, 0, NoAction, None), costNodeOrdering)
  }

  def newChildNode(problem: Problem, parent: CostNode, action: Action): CostNode = {
    val childState = problem.result(parent.state, action)
    CostNode(
      state = childState,
      cost = parent.cost + problem.stepCost(parent.state, action, childState),
      action = action,
      parent = Some(parent)
    )
  }
}
