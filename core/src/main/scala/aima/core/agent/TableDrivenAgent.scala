package aima.core.agent

import scala.collection.mutable
/**
  * @author Shawn Garner
  */
trait TableDrivenAgent extends Agent {
  def lookupTable: Map[List[Percept], Action]
  lazy val percepts = new mutable.ListBuffer[Percept]()
  lazy val agentFunction: AgentFunction = { percept =>
    percepts += percept
    lookupTable.getOrElse(percepts.toList, NoAction)
  }
}
