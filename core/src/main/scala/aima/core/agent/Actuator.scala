package aima.core.agent

import aima.core.random.Randomness

/**
  * @author Shawn Garner
  */
trait Actuator[Action, Percept] {
  def agent: Agent[Action, Percept]
  def act(action: Action, environment: Environment[Action, Percept]): Environment[Action, Percept]
}

trait UnreliableActuator[Action, Percept] extends Actuator[Action, Percept] with Randomness {
  def unreliably(original: Environment[Action, Percept], reliability: Int = 50)(
      act: => Environment[Action, Percept]
  ): Environment[Action, Percept] = {
    if (rand.nextInt(100) < reliability) act else original
  }
}
