package aima.core.environment.vacuum

import aima.core.agent.{Action, Percept, TableDrivenAgent}

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgent extends TableDrivenAgent {
  override def lookupTable: Map[List[Percept], Action] = ???

}
