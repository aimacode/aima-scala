package aima.core.agent

/**
  * @author Shawn Garner
  */
trait Environment {
  def addAgent(agent: Agent): Environment
  def removeAgent(agent: Agent): Environment
  def actuate(actuator: Actuator, action: Action): Environment
  def perceive(sensor: Sensor): Percept
}
