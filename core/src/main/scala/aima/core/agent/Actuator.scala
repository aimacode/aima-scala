package aima.core.agent

import aima.core.random.Randomness

/**
  * @author Shawn Garner
  */
trait Actuator[E, A] {
  def act(action: A, e: E): E //could be Action => E => E
}

trait UnreliableActuator[E, A] extends Actuator[E, A] with Randomness {
  def unreliably(original: E, reliability: Int = 50)(
      act: => E
  ): E = {
    if (rand.nextInt(100) < reliability) act else original
  }
}
