package aima.core.agent

import aima.core.random.{DefaultRandomness, Randomness}

/**
  * @author Shawn Garner
  * @author Damien Favre
  */
trait Actuator[ENVIRONMENT, ACTION] {
  def act(action: ACTION, e: ENVIRONMENT): ENVIRONMENT
}

object UnreliableActuator {
  def fromActuator[ENVIRONMENT, ACTION](
      actuator: Actuator[ENVIRONMENT, ACTION]
  )(
      reliability: Int = 50,
      randomness: Randomness = new DefaultRandomness {}
  ): Actuator[ENVIRONMENT, ACTION] = new Actuator[ENVIRONMENT, ACTION] {
    override def act(action: ACTION, e: ENVIRONMENT): ENVIRONMENT =
      randomness.unreliably(reliability)(actuator.act(action, e)).getOrElse(e)
  }
}
