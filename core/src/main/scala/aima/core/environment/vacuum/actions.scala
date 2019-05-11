package aima.core.environment.vacuum

import aima.core.util.{DefaultRandomness, SetRandomness}

/**
  * @author Shawn Garner
  */
sealed trait VacuumAction

sealed trait MoveAction     extends VacuumAction
case object LeftMoveAction  extends MoveAction
case object RightMoveAction extends MoveAction

object MoveAction extends SetRandomness[MoveAction] with DefaultRandomness {
  val valueSet: Set[MoveAction] = Set(LeftMoveAction, RightMoveAction)
}

sealed trait SuckerAction extends VacuumAction
case object Suck          extends SuckerAction

object SuckerAction extends SetRandomness[SuckerAction] with DefaultRandomness {
  val valueSet: Set[SuckerAction] = Set(Suck)
}

case object NoAction extends VacuumAction
