package aima.core.search.uninformed

import aima.core.agent.NoAction
import aima.core.search._

/**
  * @author Shawn Garner
  */
trait BreadthFirstSearch extends GraphSearch { self =>
  def newFrontier(state: State) = new FIFOQueueFrontier(StateNode(state, NoAction, None))
}
