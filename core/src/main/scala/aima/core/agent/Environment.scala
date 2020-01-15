package aima.core.agent

/**
  * @author Shawn Garner
  *         TODO: Not sure if it would be better to have a Simulation Class that contains Agents + Environment
  */
trait Environment[ENVIRONMENT, PERCEPT, ACTION] {
  def addAgent(agent: Agent[ENVIRONMENT, PERCEPT, ACTION]): ENVIRONMENT
  def removeAgent(agent: Agent[ENVIRONMENT, PERCEPT, ACTION]): ENVIRONMENT
}
