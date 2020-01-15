package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableSensor, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class AgentLocationSensor(
    val agent: Agent[Vacuum, VacuumPercept, VacuumAction],
    noPercept: VacuumPercept
) extends UnreliableSensor[Vacuum, VacuumPercept]
    with DefaultRandomness { self =>
  def perceive(environment: Vacuum): VacuumPercept =
    unreliably() {
      environment.perceive(self)
    }(noPercept)
}

class DirtSensor(
    val agent: Agent[Vacuum, VacuumPercept, VacuumAction],
    noPercept: VacuumPercept
) extends UnreliableSensor[Vacuum, VacuumPercept]
    with DefaultRandomness { self =>
  def perceive(environment: Vacuum): VacuumPercept =
    unreliably() {
      environment.perceive(self)
    }(noPercept)
}
