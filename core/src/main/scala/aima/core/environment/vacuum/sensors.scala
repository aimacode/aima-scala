package aima.core.environment.vacuum

import aima.core.agent.{Agent, Sensor}

/**
  * @author Shawn Garner
  */
class AgentLocationSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Sensor[VacuumEnvironment, VacuumPercept] {
  def perceive(vacuum: VacuumEnvironment): Option[VacuumPercept] =
    vacuum.map.getAgentLocation(agent)
}

class DirtSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction])
    extends Sensor[VacuumEnvironment, VacuumPercept] {
  def perceive(vacuum: VacuumEnvironment): Option[VacuumPercept] =
    vacuum.map.getDirtStatus(agent)
}
