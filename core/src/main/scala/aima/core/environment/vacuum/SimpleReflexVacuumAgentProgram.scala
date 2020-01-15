package aima.core.environment.vacuum

import aima.core.agent.SimpleReflexAgentProgram

/**
  * @author Shawn Garner
  */
class SimpleReflexVacuumAgentProgram
    extends SimpleReflexAgentProgram[VacuumPercept, VacuumAction] {
  override type State = VacuumPercept
  val interpretInput: InterpretInput = identity

  val rules: RuleMatch = {
    case DirtyPercept     => Suck
    case LocationAPercept => RightMoveAction
    case LocationBPercept => LeftMoveAction
    case CleanPercept     => NoAction
    case NoPercept        => NoAction
  }
}
