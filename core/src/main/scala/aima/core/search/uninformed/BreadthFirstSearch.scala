package aima.core.search.uninformed

import aima.core.search._

/**
  * @author Shawn Garner
  */
trait BreadthFirstSearch[State, Action] extends GraphSearch[State, Action] {
  self =>
  def newFrontier(state: State, noAction: Action) =
    new FIFOQueueFrontier(StateNode(state, noAction, None))
}
