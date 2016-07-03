package aima.core.environment.vacuum

import aima.core.agent.Action
import aima.core.util.{DefaultRandomness, SetRandomness}

/**
  * @author Shawn Garner
  */
sealed trait MoveAction extends Action
case object LeftMoveAction extends MoveAction
case object RightMoveAction extends MoveAction

object MoveAction extends SetRandomness[MoveAction] with DefaultRandomness {
  lazy val valueSet: Set[MoveAction] = Set(LeftMoveAction, RightMoveAction)
}

sealed trait SuckerAction extends Action
case object Suck extends SuckerAction

object SuckerAction extends SetRandomness[SuckerAction] with DefaultRandomness {
  lazy val valueSet: Set[SuckerAction] = Set(Suck)
}
