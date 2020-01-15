package aima.core.agent

import aima.core.random.Randomness

trait Sensor[ENVIRONMENT, PERCEPT] {
  def perceive(e: ENVIRONMENT): PERCEPT
}

trait UnreliableSensor[ENVIRONMENT, PERCEPT]
    extends Sensor[ENVIRONMENT, PERCEPT]
    with Randomness {
  def unreliably(
      reliability: Int = 50
  )(perceive: => PERCEPT)(noPercept: PERCEPT): PERCEPT = {
    if (rand.nextInt(100) < reliability) perceive else noPercept
  }
}
