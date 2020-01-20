package aima.core.agent

import aima.core.random.Randomness

trait Sensor[ENVIRONMENT, PERCEPT] {
  def perceive(e: ENVIRONMENT): PERCEPT
}

object UnreliableSensor {
  def fromSensor[ENVIRONMENT, PERCEPT](
      sensor: Sensor[ENVIRONMENT, PERCEPT]
  )(
      noPercept: PERCEPT,
      randomness: Randomness,
      reliability: Int = 50
  ): Sensor[ENVIRONMENT, PERCEPT] = new Sensor[ENVIRONMENT, PERCEPT] {
    override def perceive(e: ENVIRONMENT): PERCEPT =
      if (randomness.rand.nextInt(100) < reliability) sensor.perceive(e)
      else noPercept
  }
}
