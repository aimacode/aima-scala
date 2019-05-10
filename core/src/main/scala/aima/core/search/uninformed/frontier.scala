package aima.core.search.uninformed

import aima.core.search.{Frontier, State, SearchNode}

import scala.collection.immutable.{Queue, Iterable}
import scala.collection.mutable
import scala.util.Try

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

class PriorityQueueHashSetFrontier[Node <: SearchNode](
    queue: mutable.PriorityQueue[Node],
    stateMap: mutable.Map[State, Node]
) extends Frontier[Node] { self =>

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
