package aima.core.search.uninformed

import aima.core.agent.NoAction
import aima.core.search.State

import scala.collection.immutable.{Queue, Iterable}

/**
  * @author Shawn Garner
  */
trait BreadthFirstSearch extends GraphSearch {

  def newFrontier(state: State): Frontier = new FIFOQueueFrontier(state)

  object FIFOQueueFrontier {
    def toQueue(s: State) =
      Queue(StateNode(s, NoAction, None))
  }

  class FIFOQueueFrontier(queue: Queue[Node], stateSet: Set[State]) extends Frontier { self =>
    def this(s: State) = this(FIFOQueueFrontier.toQueue(s), Set(s))

    def removeLeaf: Option[(Node, Frontier)] = queue.dequeueOption.map {
      case (leaf, updatedQueue) => (leaf, new FIFOQueueFrontier(updatedQueue, stateSet - leaf.state))
    }
    def addAll(iterable: Iterable[Node]): Frontier =
      new FIFOQueueFrontier(queue.enqueue(iterable), stateSet ++ iterable.map(_.state))
    def contains(state: State): Boolean = stateSet.contains(state)

    def replaceByState(node: Node): Frontier = {
      if (contains(node.state)) {
        new FIFOQueueFrontier(queue.filterNot(_.state == node.state).enqueue(node), stateSet)
      } else {
        self
      }
    }
    def getNode(state: State): Option[Node] = {
      if (contains(state)) {
        queue.find(_.state == state)
      } else {
        None
      }
    }

    def add(node: Node): Frontier = new FIFOQueueFrontier(queue.enqueue(node), stateSet + node.state)
  }
}
