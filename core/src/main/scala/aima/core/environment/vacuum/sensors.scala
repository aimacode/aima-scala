package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableSensor, Agent}
import aima.core.util.DefaultRandomness

/**
  * @author Shawn Garner
  */
class AgentLocationSensor[Action, Percept](val agent: Agent[Action, Percept], noPercept: Percept)
    extends UnreliableSensor[Action, Percept]
    with DefaultRandomness { self =>
  def perceive(environment: Environment[Action, Percept]): Percept =
    unreliably() {
      environment.perceive(self)
    }(noPercept)
}

class DirtSensor[Action, Percept](val agent: Agent[Action, Percept], noPercept: Percept)
    extends UnreliableSensor[Action, Percept]
    with DefaultRandomness { self =>
  def perceive(environment: Environment[Action, Percept]): Percept =
    unreliably() {
      environment.perceive(self)
    }(noPercept)
}
