package aima.core.agent

/**
  * @author Shawn Garner
  */
trait Environment[ENVIRONMENT, PERCEPT, ACTION] {
  def addAgent(agent: Agent[ENVIRONMENT, PERCEPT, ACTION]): ENVIRONMENT
  def removeAgent(agent: Agent[ENVIRONMENT, PERCEPT, ACTION]): ENVIRONMENT
  def actuate(actuator: Actuator[ENVIRONMENT, ACTION], action: ACTION): ENVIRONMENT
  def perceive(sensor: Sensor[ENVIRONMENT, PERCEPT]): PERCEPT
}
