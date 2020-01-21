package aima.core.agent

import aima.core.random.{DefaultRandomness, Randomness}

trait Sensor[ENVIRONMENT, PERCEPT] {
  def perceive(e: ENVIRONMENT): Option[PERCEPT]
}

object UnreliableSensor {
  def fromSensor[ENVIRONMENT, PERCEPT](
      sensor: Sensor[ENVIRONMENT, PERCEPT]
  )(
      reliability: Int = 50,
      randomness: Randomness = new DefaultRandomness {}
  ): Sensor[ENVIRONMENT, PERCEPT] = new Sensor[ENVIRONMENT, PERCEPT] {
    override def perceive(e: ENVIRONMENT): Option[PERCEPT] =
      randomness.unreliably(reliability)(sensor.perceive(e)).flatten
  }

}
