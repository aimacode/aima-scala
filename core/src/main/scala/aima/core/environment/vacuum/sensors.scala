package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableSensor, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class AgentLocationSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction], noPercept: VacuumPercept)
    extends UnreliableSensor[VacuumEnvironment, VacuumPercept]
    with DefaultRandomness {
  def perceive(vacuum: VacuumEnvironment): VacuumPercept =
    unreliably() {
      vacuum.map.getAgentLocation(agent).getOrElse(NoPercept)
    }(noPercept)
}

class DirtSensor(val agent: Agent[VacuumEnvironment, VacuumPercept, VacuumAction], noPercept: VacuumPercept)
    extends UnreliableSensor[VacuumEnvironment, VacuumPercept]
    with DefaultRandomness { self =>
  def perceive(vacuum: VacuumEnvironment): VacuumPercept =
    unreliably() {
      vacuum.map.getDirtStatus(agent).getOrElse(NoPercept)
    }(noPercept)
}
