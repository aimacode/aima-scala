package aima.core.agent


/**
  * @author Shawn Garner
  */
trait Agent {
  type AgentFunction = Percept => Action

  def actuators: Set[Actuator]
  def sensors: Set[Sensor]
  def agentFunction: AgentFunction

  def run(): Unit
}

trait Percept
trait Sensor {
  def perceive(environment: Environment): Stream[Percept]
}

trait Action
trait Actuator {
  def act(action: Action, environment: Environment): Environment
}

trait Environment