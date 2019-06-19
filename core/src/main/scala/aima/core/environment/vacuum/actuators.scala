package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableActuator, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class SuckerActuator[Action, Percept](val agent: Agent[Action, Percept])
    extends UnreliableActuator[Action, Percept]
    with DefaultRandomness { self =>
  def act(action: Action, environment: Environment[Action, Percept]): Environment[Action, Percept] = {
    unreliably(environment) {
      environment.actuate(self, action)
    }
  }
}
class MoveActuator[Action, Percept](val agent: Agent[Action, Percept])
    extends UnreliableActuator[Action, Percept]
    with DefaultRandomness { self =>
  def act(action: Action, environment: Environment[Action, Percept]): Environment[Action, Percept] = {
    unreliably(environment) {
      environment.actuate(self, action)
    }
  }
}
