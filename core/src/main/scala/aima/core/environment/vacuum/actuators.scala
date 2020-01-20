package aima.core.environment.vacuum

import aima.core.agent.{Actuator, Agent}

/**
  * @author Shawn Garner
  */
class SuckerActuator(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction]) extends Actuator[VacuumEnvironment, VacuumAction] {
  def act(action: VacuumAction, vacuum: VacuumEnvironment): Option[VacuumEnvironment] = action match {
    case Suck =>
      Some(vacuum.copy(vacuum.map.updateStatus(agent, CleanPercept)))
    case _ => None
  }
}
class MoveActuator(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction]) extends Actuator[VacuumEnvironment, VacuumAction] {
  def act(action: VacuumAction, vacuum: VacuumEnvironment): Option[VacuumEnvironment] = action match {
    case move: MoveAction =>
      Some(vacuum.copy(vacuum.map.moveAgent(agent, move)))
    case _ => None
  }
}
