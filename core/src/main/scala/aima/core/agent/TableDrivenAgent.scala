package aima.core.agent

import scala.collection.mutable

/**
  * @author Shawn Garner
  */
trait TableDrivenAgent extends Agent {
  type LookupTable = PartialFunction[List[Percept], Action]

  def lookupTable: LookupTable
  lazy val percepts = new mutable.ListBuffer[Percept]()

  lazy val agentFunction: AgentFunction = { percept =>
    percepts += percept
    lookup(lookupTable, percepts.toList)
  }

  def lookup(lookup: LookupTable, percepts: List[Percept]): Action = {
    lookup.applyOrElse(percepts, (_: List[Percept]) => NoAction)
  }
}
