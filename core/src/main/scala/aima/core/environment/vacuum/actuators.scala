package aima.core.environment.vacuum

import aima.core.agent.{Environment, UnreliableActuator, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class SuckerActuator(val agent: Agent[Vacuum, VacuumPercept, VacuumAction])
    extends UnreliableActuator[Vacuum, VacuumAction]
    with DefaultRandomness { self =>
  def act(action: VacuumAction, environment: Vacuum): Vacuum = {
    unreliably(environment) {
      environment.actuate(self, action)
    }
  }
}
class MoveActuator(val agent: Agent[Vacuum, VacuumPercept, VacuumAction])
    extends UnreliableActuator[Vacuum, VacuumAction]
    with DefaultRandomness { self =>
  def act(action: VacuumAction, environment: Vacuum): Vacuum = {
    unreliably(environment) {
      environment.actuate(self, action)
    }
  }
}
