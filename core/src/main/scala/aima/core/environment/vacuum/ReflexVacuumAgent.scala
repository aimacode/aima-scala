package aima.core.environment.vacuum

import aima.core.agent.{Percept, SimpleReflexAgent}
import aima.core.environment.vacuum.DirtStatusPercepts.Dirty
import aima.core.environment.vacuum.LocationPercepts.{B, A}
import aima.core.environment.vacuum.MoveActions.{Left, Right}
import aima.core.environment.vacuum.SuckerActions.Suck

/**
  * @author Shawn Garner
  */
class ReflexVacuumAgent extends SimpleReflexAgent {
  type State = Percept

  def interpretInput: InterpretInput = {
    case s => s
  }

  def rules: RuleMatch = {
    case Dirty => Suck
    case A => Right
    case B => Left
  }
}
