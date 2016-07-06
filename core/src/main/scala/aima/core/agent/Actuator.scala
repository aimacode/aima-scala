package aima.core.agent

import aima.core.util.Randomness

/**
  * @author Shawn Garner
  */
trait Actuator {
  def agent: Agent
  def act(action: Action, environment: Environment): Environment
}

trait UnreliableActuator extends Actuator with Randomness {
  def unreliably(original: Environment, reliability: Int = 50)(act: => Environment): Environment = {
    if (rand.nextInt(100) < reliability) act else original
  }
}
