package aima.core.search.uninformed

import aima.core.agent.{NoAction, Action}
import aima.core.search.{FrontierSearch, State, Problem}

import scala.annotation.tailrec
import scala.collection.immutable.Iterable
import scala.collection.mutable
import scala.util.Try

/**
  * @author Shawn Garner
  */
trait UniformCostSearch extends FrontierSearch {
  case class CostNode(state: State, cost: Int, action: Action, parent: Option[CostNode])
  type Node = CostNode

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

  object PriorityQueueHashSetFrontier {
    val costNodeOrdering: Ordering[CostNode] = new Ordering[CostNode] {
      def compare(x: CostNode, y: CostNode): Int = Ordering.Int.reverse.compare(x.cost, y.cost)
    }
  }

  import PriorityQueueHashSetFrontier._
  class PriorityQueueHashSetFrontier(queue: mutable.PriorityQueue[CostNode], stateMap: mutable.Map[State, CostNode])
      extends Frontier { self =>

    def this(s: State) =
      this(mutable.PriorityQueue(CostNode(s, 0, NoAction, None))(costNodeOrdering),
           mutable.Map(s -> CostNode(s, 0, NoAction, None)))

    def removeLeaf: Option[(Node, Frontier)] =
      Try {
        val leaf = queue.dequeue
        stateMap -= leaf.state
        (leaf, self)
      }.toOption

    def addAll(iterable: Iterable[Node]): Frontier = {
      iterable.foreach { costNode =>
        queue += costNode
        stateMap += (costNode.state -> costNode)
      }
      self
    }

    def contains(state: State): Boolean = stateMap.contains(state)

    override def replaceByState(node: Node): Frontier = {
      if (contains(node.state)) {
        val updatedElems = node :: queue.toList.filterNot(_.state == node.state)
        queue.clear()
        queue.enqueue(updatedElems: _*)
        stateMap += (node.state -> node)
      }
      self

    }
    override def getNode(state: State): Option[Node] = {
      if (contains(state)) {
        queue.find(_.state == state)
      } else {
        None
      }
    }

    override def add(node: Node): Frontier = {
      val costNode = node
      queue.enqueue(costNode)
      stateMap += (node.state -> costNode)
      self
    }
  }

  override def newFrontier(state: State): Frontier = new PriorityQueueHashSetFrontier(state)

  override def newChildNode(problem: Problem, parent: CostNode, action: Action): CostNode = {
    val childState = problem.result(parent.state, action)
    CostNode(
        state = childState,
        cost = parent.cost + problem.stepCost(parent.state, action, childState),
        action = action,
        parent = Some(parent)
    )
  }
  override def solution(node: CostNode): List[Action] = {
    @tailrec def solutionHelper(n: Node, actions: List[Action]): List[Action] = {
      n.parent match {
        case None         => actions
        case Some(parent) => solutionHelper(parent, n.action :: actions)
      }
    }

    solutionHelper(node, Nil)
  }
}
