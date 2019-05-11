package aima.core.environment.vacuum

import aima.core.agent.SimpleReflexAgent

/**
  * @author Shawn Garner
  */
class SimpleReflexVacuumAgent extends SimpleReflexAgent[VacuumPercept, VacuumAction, VacuumPercept] {
  val interpretInput: InterpretInput = identity

  val rules: RuleMatch = {
    case DirtyPercept     => Suck
    case LocationAPercept => RightMoveAction
    case LocationBPercept => LeftMoveAction
    case CleanPercept     => NoAction
    case NoPercept        => NoAction
  }
}
