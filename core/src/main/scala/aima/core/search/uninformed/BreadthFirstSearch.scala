package aima.core.search.uninformed

import aima.core.agent.NoAction
import aima.core.search._

import scala.collection.immutable.{Queue, Iterable}

class FIFOQueueFrontier[Node <: SearchNode](queue: Queue[Node], stateSet: Set[State]) extends Frontier[Node] { self =>
  def this(n: Node) = this(Queue(n), Set(n.state))

  def removeLeaf: Option[(Node, Frontier[Node])] = queue.dequeueOption.map {
    case (leaf, updatedQueue) => (leaf, new FIFOQueueFrontier(updatedQueue, stateSet - leaf.state))
  }
  def addAll(iterable: Iterable[Node]): Frontier[Node] =
    new FIFOQueueFrontier(queue.enqueue(iterable), stateSet ++ iterable.map(_.state))
  def contains(state: State): Boolean = stateSet.contains(state)

  def replaceByState(node: Node): Frontier[Node] = {
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

  def add(node: Node): Frontier[Node] = new FIFOQueueFrontier[Node](queue.enqueue(node), stateSet + node.state)
}

/**
  * @author Shawn Garner
  */
trait BreadthFirstSearch extends GraphSearch { self =>
  def newFrontier(state: State) = new FIFOQueueFrontier(StateNode(state, NoAction, None))
}
