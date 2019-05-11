package aima.core.agent

import scala.collection.mutable

/**
  * @author Shawn Garner
  */
trait TableDrivenAgent[Action, Percept] extends Agent[Action, Percept] {
  type LookupTable = List[Percept] => Action

  val percepts = new mutable.ListBuffer[Percept]()

  def lookupTable: LookupTable

  val agentFunction: AgentFunction = { percept =>
    percepts += percept
    lookup(lookupTable, percepts.toList)
  }

  def lookup(lt: LookupTable, percepts: List[Percept]): Action = {
    lt(percepts)
  }
}
