package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableSensor, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class AgentLocationSensor(val agent: Agent[Vacuum, VacuumPercept, VacuumAction], noPercept: VacuumPercept)
    extends UnreliableSensor[Vacuum, VacuumPercept]
    with DefaultRandomness {
  def perceive(vacuum: Vacuum): VacuumPercept =
    unreliably() {
      vacuum.map.getAgentLocation(agent).getOrElse(NoPercept)
    }(noPercept)
}

class DirtSensor(val agent: Agent[Vacuum, VacuumPercept, VacuumAction], noPercept: VacuumPercept)
    extends UnreliableSensor[Vacuum, VacuumPercept]
    with DefaultRandomness { self =>
  def perceive(vacuum: Vacuum): VacuumPercept =
    unreliably() {
      vacuum.map.getDirtStatus(agent).getOrElse(NoPercept)
    }(noPercept)
}
