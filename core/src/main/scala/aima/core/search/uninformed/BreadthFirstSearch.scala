package aima.core.search.uninformed

import scala.collection.immutable.{Queue, Iterable}

/**
  * @author Shawn Garner
  */
trait BreadthFirstSearch extends GraphSearch {

  def newFrontier(state: State): Frontier = new FIFOQueueFrontier(state)

  object FIFOQueueFrontier {
    def toQueue(s: State) =
      Queue(new Node {
        lazy val state: State = s
      })
  }

  class FIFOQueueFrontier(queue: Queue[Node], stateSet: Set[State]) extends Frontier {
    def this(s: State) = this(FIFOQueueFrontier.toQueue(s), Set(s))

    def removeLeaf: Option[(Node, Frontier)] = queue.dequeueOption.map {
      case (leaf, updatedQueue) => (leaf, new FIFOQueueFrontier(updatedQueue, stateSet - leaf.state))
    }
    def addAll(iterable: Iterable[Node]): Frontier =
      new FIFOQueueFrontier(queue.enqueue(iterable), stateSet ++ iterable.map(_.state))
    def contains(state: State): Boolean = stateSet.contains(state)

  }
}
