package aima.core.agent

import scala.collection.mutable

/**
  * @author Shawn Garner
  */
trait TableDrivenAgentProgram[PERCEPT, ACTION]
    extends AgentProgram[PERCEPT, ACTION] {
  type LookupTable = List[PERCEPT] => ACTION

  val percepts = new mutable.ListBuffer[PERCEPT]()

  def lookupTable: LookupTable

  val agentFunction: AgentFunction = { percept =>
    percepts += percept
    lookup(lookupTable, percepts.toList)
  }

  def lookup(lt: LookupTable, percepts: List[PERCEPT]): ACTION = {
    lt(percepts)
  }
}
