package aima.core.environment.vacuum

import aima.core.agent.{Percept, SimpleReflexAgent}

/**
  * @author Shawn Garner
  */
class SimpleReflexVacuumAgent extends SimpleReflexAgent {
  type State = Percept

  lazy val interpretInput: InterpretInput = {
    case s => s
  }

  lazy val rules: RuleMatch = {
    case DirtyPercept     => Suck
    case LocationAPercept => RightMoveAction
    case LocationBPercept => LeftMoveAction
  }
}
