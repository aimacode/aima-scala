package aima.core.agent

/**
  * @author Shawn Garner
  */
trait AgentProgram {
  def actuators: Seq[Actuator]
  def sensors: Seq[Sensor]
  def agent: Agent

  def run(environment: Environment): Environment = {
      val actions = for {
        sensor <- sensors
      } yield agent.agentFunction(sensor.perceive(environment))

      actions.foldLeft(environment){ (env, action) =>
          actuators.foldLeft(env){ (env2, actuator) =>
            actuator.act(action, environment)
          }
      }
  }
}

trait Agent {
  type AgentFunction = Percept => Action

  def agentFunction: AgentFunction
}

trait Percept extends Any

case object NoPercept extends Percept


trait Sensor {
  def agent: Agent
  def perceive(environment: Environment): Percept
}

trait Action extends Any

case object NoAction extends Action

trait Actuator {
  def agent: Agent
  def act(action: Action, environment: Environment): Environment
}

trait Environment {
  def addAgent(agent: Agent): Environment
  def removeAgent(agent: Agent): Environment
  def actuate(actuator: Actuator, action: Action): Environment
  def perceive(sensor: Sensor): Percept
}