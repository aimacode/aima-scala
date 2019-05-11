package aima.core.agent

/**
  * @author Shawn Garner
  */
trait Environment[Action, Percept] {
  def addAgent(agent: Agent[Action, Percept]): Environment[Action, Percept]
  def removeAgent(agent: Agent[Action, Percept]): Environment[Action, Percept]
  def actuate(actuator: Actuator[Action, Percept], action: Action): Environment[Action, Percept]
  def perceive(sensor: Sensor[Action, Percept]): Percept
}
