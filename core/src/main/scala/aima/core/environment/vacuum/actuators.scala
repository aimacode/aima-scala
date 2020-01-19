package aima.core.environment.vacuum

import aima.core.agent.{UnreliableActuator, Agent}
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
class SuckerActuator(val agent: Agent[Vacuum, VacuumPercept, VacuumAction])
    extends UnreliableActuator[Vacuum, VacuumAction]
    with DefaultRandomness {
  def act(action: VacuumAction, vacuum: Vacuum): Vacuum = {
    unreliably(vacuum) {
      action match {
        case Suck =>
          vacuum.copy(vacuum.map.updateStatus(agent, CleanPercept))
        case _ => vacuum
      }
    }
  }
}
class MoveActuator(val agent: Agent[Vacuum, VacuumPercept, VacuumAction])
    extends UnreliableActuator[Vacuum, VacuumAction]
    with DefaultRandomness { self =>
  def act(action: VacuumAction, vacuum: Vacuum): Vacuum = {
    unreliably(vacuum) {
      action match {
        case move: MoveAction =>
          vacuum.copy(vacuum.map.moveAgent(agent, move))
        case _ => vacuum
      }
    }
  }
}
