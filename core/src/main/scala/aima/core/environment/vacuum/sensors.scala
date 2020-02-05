package aima.core.environment.vacuum

import aima.core.agent.{Agent, Sensor}

/**
  * @author Shawn Garner
  */
class AgentLocationSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Sensor[VacuumEnvironment, VacuumPercept] {
  def perceive(vacuum: VacuumEnvironment): Option[VacuumPercept] =
    Some(vacuum.map.getAgentLocation(agent).getOrElse(NoPercept))
}

class DirtSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Sensor[VacuumEnvironment, VacuumPercept] {
  def perceive(vacuum: VacuumEnvironment): Option[VacuumPercept] =
    Some(vacuum.map.getDirtStatus(agent).getOrElse(NoPercept))
}
