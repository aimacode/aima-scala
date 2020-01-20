package aima.core.agent

trait Sensor[ENVIRONMENT, PERCEPT] {
  def perceive(e: ENVIRONMENT): Option[PERCEPT]
}
