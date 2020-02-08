package aima.core.environment.vacuum

import aima.core.agent.{Actuator, Agent}

/**
  * @author Shawn Garner
  */
class SuckerActuator(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Actuator[VacuumEnvironment, VacuumAction] {
  def act(action: VacuumAction, vacuum: VacuumEnvironment): VacuumEnvironment = action match {
    case Suck =>
      vacuum.copy(vacuum.map.updateStatus(agent, CleanPercept))
    case _ => vacuum
  }
}
class MoveActuator(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Actuator[VacuumEnvironment, VacuumAction] {
  def act(action: VacuumAction, vacuum: VacuumEnvironment): VacuumEnvironment = action match {
    case move: MoveAction =>
      vacuum.copy(vacuum.map.moveAgent(agent, move))
    case _ => vacuum
  }
}
