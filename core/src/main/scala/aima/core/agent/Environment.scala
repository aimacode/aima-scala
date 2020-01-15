package aima.core.agent

/**
  * @author Shawn Garner
  *         TODO: Not sure if it would be better to have a Simulation Class that contains Agents + Environment
  */
trait Environment[E, P, A] {
  def addAgent(agent: Agent[E, P, A]): E
  def removeAgent(agent: Agent[E, P, A]): E
//  def actuate(actuator: Actuator[E, ], action: Action[E]): E
//  def perceive(sensor: Sensor[E]): Percept[E]
}
