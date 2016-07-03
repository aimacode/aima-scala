package aima.core.agent

import aima.core.util.Randomness

trait Sensor {
  def agent: Agent
  def perceive(environment: Environment): Percept
}

trait UnreliableSensor extends Sensor with Randomness {
  def unreliably(reliability: Int = 50)(perceive: => Percept): Percept = {
    if (rand.nextInt(100) < reliability) perceive else NoPercept
  }
}
