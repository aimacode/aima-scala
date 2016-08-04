package aima.core.search.uninformed

import aima.core.agent.{NoAction, Action}
import aima.core.search._

import scala.annotation.tailrec
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.util.Try

class PriorityQueueHashSetFrontier[Node <: SearchNode](queue: mutable.PriorityQueue[Node],
                                                       stateMap: mutable.Map[State, Node])
    extends Frontier[Node] { self =>

  def this(n: Node, costNodeOrdering: Ordering[Node]) =
    this(mutable.PriorityQueue(n)(costNodeOrdering), mutable.Map(n.state -> n))

  def removeLeaf: Option[(Node, Frontier[Node])] =
    Try {
      val leaf = queue.dequeue
      stateMap -= leaf.state
      (leaf, self)
    }.toOption

  def addAll(iterable: Iterable[Node]): Frontier[Node] = {
    iterable.foreach { costNode =>
      queue += costNode
      stateMap += (costNode.state -> costNode)
    }
    self
  }

  def contains(state: State): Boolean = stateMap.contains(state)

  def replaceByState(node: Node): Frontier[Node] = {
    if (contains(node.state)) {
      val updatedElems = node :: queue.toList.filterNot(_.state == node.state)
      queue.clear()
      queue.enqueue(updatedElems: _*)
      stateMap += (node.state -> node)
    }
    self
  }

  def getNode(state: State): Option[Node] = {
    if (contains(state)) {
      queue.find(_.state == state)
    } else {
      None
    }
  }

  def add(node: Node): Frontier[Node] = {
    val costNode = node
    queue.enqueue(costNode)
    stateMap += (node.state -> costNode)
    self
  }
}

/**
  * @author Shawn Garner
  */
trait UniformCostSearch extends ProblemSearch with FrontierSearch {

  type Node = CostNode

  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Frontier[Node], exploredSet: Set[State] = Set.empty[State]): List[Action] = {
      frontier.removeLeaf match {
        case None                                                     => List.empty[Action]
        case Some((leaf: Node, _)) if problem.isGoalState(leaf.state) => solution(leaf)
        case Some((leaf: Node, updatedFrontier)) =>
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

  def solution(node: CostNode): List[Action] = {
    @tailrec def solutionHelper(n: Node, actions: List[Action]): List[Action] = {
      n.parent match {
        case None         => actions
        case Some(parent) => solutionHelper(parent, n.action :: actions)
      }
    }

    solutionHelper(node, Nil)
  }
}
