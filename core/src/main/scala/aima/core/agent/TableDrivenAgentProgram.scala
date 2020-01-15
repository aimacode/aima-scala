package aima.core.agent

import scala.collection.mutable

/**
  * @author Shawn Garner
  */
trait TableDrivenAgentProgram[P, A] extends AgentProgram[P, A] {
  type LookupTable = List[P] => A

  val percepts = new mutable.ListBuffer[P]()

  def lookupTable: LookupTable

  val agentFunction: AgentFunction = { percept =>
    percepts += percept
    lookup(lookupTable, percepts.toList)
  }

  def lookup(lt: LookupTable, percepts: List[P]): A = {
    lt(percepts)
  }
}
