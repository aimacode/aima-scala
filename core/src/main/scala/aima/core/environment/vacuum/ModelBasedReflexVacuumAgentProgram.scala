package aima.core.environment.vacuum

import aima.core.agent.{ModelBasedReflexAgentProgram}
import ModelBasedReflexVacuumAgentProgram._

object ModelBasedReflexVacuumAgentProgram {
  val MOVE_BATTERY_COST = 2
  val SUCK_BATTERY_COST = 1
}

final case class VacuumWorldState(
    locationA: Boolean = false,
    locationB: Boolean = false,
    dirty: Boolean = true,
    batteryLife: Int = 100
)

/**
  * @author Shawn Garner
  */
class ModelBasedReflexVacuumAgentProgram
    extends ModelBasedReflexAgentProgram[
      VacuumPercept,
      VacuumAction,
      VacuumWorldState
    ] {
//  override type State = VacuumWorldState
  val model: Model = {
    case (currentState, RightMoveAction) =>
      currentState.copy(
        locationA = false,
        locationB = true,
        dirty = true,
        batteryLife = currentState.batteryLife - MOVE_BATTERY_COST
      )
    case (currentState, LeftMoveAction) =>
      currentState.copy(
        locationA = true,
        locationB = false,
        dirty = true,
        batteryLife = currentState.batteryLife - MOVE_BATTERY_COST
      )
    case (currentState, Suck) =>
      currentState.copy(
        dirty = false,
        batteryLife = currentState.batteryLife - SUCK_BATTERY_COST
      )
    case (currentState, NoAction) => currentState
  }

  lazy val noAction: VacuumAction = NoAction

  val rules: RuleMatch = {
    case VacuumWorldState(_, _, _, batteryLife) if batteryLife < 10 =>
      NoAction //too costly to continue
    case VacuumWorldState(_, _, dirty, _) if dirty         => Suck
    case VacuumWorldState(locationA, _, _, _) if locationA => RightMoveAction
    case VacuumWorldState(_, locationB, _, _) if locationB => LeftMoveAction
  }

  lazy val initialState: VacuumWorldState = VacuumWorldState()
  val updateState: UpdateState = {
    (s: VacuumWorldState, a: VacuumAction, p: VacuumPercept, m: Model) =>
      val s2 = m(s, a)
      p match {
        case CleanPercept     => s2.copy(dirty = false)
        case DirtyPercept     => s2.copy(dirty = true)
        case LocationAPercept => s2.copy(locationA = true, locationB = false)
        case LocationBPercept => s2.copy(locationB = true, locationA = false)
        case _                => s2
      }
  }
}
