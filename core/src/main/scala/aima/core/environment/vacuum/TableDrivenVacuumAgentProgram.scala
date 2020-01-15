package aima.core.environment.vacuum

import aima.core.agent._

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgentProgram
    extends TableDrivenAgentProgram[VacuumPercept, VacuumAction] {
  val lookupTable: LookupTable = {
    case List(_, DirtyPercept)                            => Suck
    case List(LocationAPercept, CleanPercept)             => RightMoveAction
    case List(LocationBPercept, CleanPercept)             => LeftMoveAction
    case List(_, _, _, DirtyPercept)                      => Suck
    case List(_, _, LocationAPercept, CleanPercept)       => RightMoveAction
    case List(_, _, LocationBPercept, CleanPercept)       => LeftMoveAction
    case List(_, _, _, _, _, DirtyPercept)                => Suck
    case List(_, _, _, _, LocationAPercept, CleanPercept) => RightMoveAction
    case List(_, _, _, _, LocationBPercept, CleanPercept) => LeftMoveAction
    case List(_, _, _, _, _, _, _, DirtyPercept)          => Suck
    case List(_, _, _, _, _, _, LocationAPercept, CleanPercept) =>
      RightMoveAction
    case List(_, _, _, _, _, _, LocationBPercept, CleanPercept) =>
      LeftMoveAction
    case _ => NoAction
  }
}
