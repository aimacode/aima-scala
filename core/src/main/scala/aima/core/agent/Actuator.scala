package aima.core.agent

import aima.core.random.Randomness

/**
  * @author Shawn Garner
  */
trait Actuator[ENVIRONMENT, ACTION] {
  def act(action: ACTION, e: ENVIRONMENT): ENVIRONMENT //could be Action => E => E
}

trait UnreliableActuator[ENVIRONMENT, ACTION] extends Actuator[ENVIRONMENT, ACTION] with Randomness {
  def unreliably(original: ENVIRONMENT, reliability: Int = 50)(
      act: => ENVIRONMENT
  ): ENVIRONMENT = {
    if (rand.nextInt(100) < reliability) act else original
  }
}
