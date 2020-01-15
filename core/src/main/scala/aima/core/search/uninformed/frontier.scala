package aima.core.search.uninformed

import aima.core.search.{Frontier, SearchNode}

import scala.collection.immutable.{Queue, Iterable}
import scala.collection.mutable
import scala.util.Try

class FIFOQueueFrontier[State, Action, Node <: SearchNode[State, Action]](
    queue: Queue[Node],
    stateSet: Set[State]
) extends Frontier[State, Action, Node] { self =>
  def this(n: Node) = this(Queue(n), Set(n.state))

  def removeLeaf: Option[(Node, Frontier[State, Action, Node])] =
    queue.dequeueOption.map {
      case (leaf, updatedQueue) =>
        (
          leaf,
          new FIFOQueueFrontier[State, Action, Node](
            updatedQueue,
            stateSet - leaf.state
          )
        )
    }
  def addAll(iterable: Iterable[Node]): Frontier[State, Action, Node] =
    new FIFOQueueFrontier(
      queue.enqueueAll(iterable),
      stateSet ++ iterable.map(_.state)
    )
  def contains(state: State): Boolean = stateSet.contains(state)

  def replaceByState(node: Node): Frontier[State, Action, Node] = {
    if (contains(node.state)) {
      new FIFOQueueFrontier(
        queue.filterNot(_.state == node.state).enqueue(node),
        stateSet
      )
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

  def add(node: Node): Frontier[State, Action, Node] =
    new FIFOQueueFrontier[State, Action, Node](
      queue.enqueue(node),
      stateSet + node.state
    )
}

class PriorityQueueHashSetFrontier[State, Action, Node <: SearchNode[
  State,
  Action
]](
    queue: mutable.PriorityQueue[Node],
    stateMap: mutable.Map[State, Node]
) extends Frontier[State, Action, Node] { self =>

  def this(n: Node, costNodeOrdering: Ordering[Node]) =
    this(mutable.PriorityQueue(n)(costNodeOrdering), mutable.Map(n.state -> n))

  def removeLeaf: Option[(Node, Frontier[State, Action, Node])] =
    Try {
      val leaf = queue.dequeue
      stateMap -= leaf.state
      (leaf, self)
    }.toOption

  def addAll(iterable: Iterable[Node]): Frontier[State, Action, Node] = {
    iterable.foreach { costNode =>
      queue += costNode
      stateMap += (costNode.state -> costNode)
    }
    self
  }

  def contains(state: State): Boolean = stateMap.contains(state)

  def replaceByState(node: Node): Frontier[State, Action, Node] = {
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

  def add(node: Node): Frontier[State, Action, Node] = {
    val costNode = node
    queue.enqueue(costNode)
    stateMap += (node.state -> costNode)
    self
  }
}
