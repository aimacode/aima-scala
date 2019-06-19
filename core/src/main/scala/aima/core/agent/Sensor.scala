package aima.core.agent

import aima.core.random.Randomness

trait Sensor[Action, Percept] {
  def agent: Agent[Action, Percept]
  def perceive(environment: Environment[Action, Percept]): Percept
}

trait UnreliableSensor[Action, Percept] extends Sensor[Action, Percept] with Randomness {
  def unreliably(reliability: Int = 50)(perceive: => Percept)(noPercept: Percept): Percept = {
    if (rand.nextInt(100) < reliability) perceive else noPercept
  }
}
