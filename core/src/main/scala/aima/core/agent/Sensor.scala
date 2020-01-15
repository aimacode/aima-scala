package aima.core.agent

import aima.core.random.Randomness

trait Sensor[E, P] {
  def perceive(e: E): P
}

trait UnreliableSensor[E, P] extends Sensor[E, P] with Randomness {
  def unreliably(reliability: Int = 50)(perceive: => P)(noPercept: P): P = {
    if (rand.nextInt(100) < reliability) perceive else noPercept
  }
}
