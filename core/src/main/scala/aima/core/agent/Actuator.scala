package aima.core.agent

import aima.core.random.Randomness

/**
  * @author Shawn Garner
  * @author Damien Favre
  */
trait Actuator[ENVIRONMENT, ACTION] {
  def act(action: ACTION, e: ENVIRONMENT): ENVIRONMENT //could be Action => E => E
}

object UnreliableActuator {
  def fromActuator[ENVIRONMENT, ACTION](
      actuator: Actuator[ENVIRONMENT, ACTION]
  )(
      randomness: Randomness,
      reliability: Int = 50
  ): Actuator[ENVIRONMENT, ACTION] = new Actuator[ENVIRONMENT, ACTION] {
    override def act(action: ACTION, e: ENVIRONMENT): ENVIRONMENT =
      if (randomness.rand.nextInt(100) < reliability) actuator.act(action, e)
      else e
  }
}
