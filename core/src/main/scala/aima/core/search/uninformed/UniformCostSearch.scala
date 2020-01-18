package aima.core.search.uninformed

import aima.core.search._

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
trait UniformCostSearch[State, Action]
    extends ProblemSearch[State, Action, CostNode[State, Action]]
    with FrontierSearch[State, Action, CostNode[State, Action]] {

  type Node = CostNode[State, Action]

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
            action <- problem.actions(leaf.state)
          } yield newChildNode(problem, leaf, action)

          val frontierWithChildNodes = childNodes.foldLeft(updatedFrontier) { (accFrontier, childNode) =>
            if (!(updatedExploredSet.contains(childNode.state) || accFrontier
                  .contains(childNode.state))) {
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

  def newFrontier(state: State, noAction: Action) = {
    val costNodeOrdering: Ordering[Node] = new Ordering[Node] {
      def compare(n1: Node, n2: Node): Int =
        Ordering.Int.reverse.compare(n1.cost, n2.cost)
    }
    new PriorityQueueHashSetFrontier[State, Action, Node](
      CostNode(state, 0, noAction, None),
      costNodeOrdering
    )
  }

  def newChildNode(
      problem: Problem[State, Action],
      parent: Node,
      action: Action
  ): Node = {
    val childState = problem.result(parent.state, action)
    CostNode(
      state = childState,
      cost = parent.cost + problem.stepCost(parent.state, action, childState),
      action = action,
      parent = Some(parent)
    )
  }
}
