package aima.core.agent

import scala.util.Random

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

trait UnreliableSensor extends Sensor with Randomness {
  def unreliably(reliability: Int = 50)(perceive: => Percept): Percept = {
    if(rand.nextInt(100) < reliability) perceive else NoPercept
  }
}

trait Action extends Any

case object NoAction extends Action

trait Actuator {
  def agent: Agent
  def act(action: Action, environment: Environment): Environment
}


trait UnreliableActuator extends Actuator with Randomness {
  def unreliably(original: Environment, reliability: Int = 50)(act: => Environment): Environment = {
    if(rand.nextInt(100) < reliability) act else original
  }
}


trait Environment {
  def addAgent(agent: Agent): Environment
  def removeAgent(agent: Agent): Environment
  def actuate(actuator: Actuator, action: Action): Environment
  def perceive(sensor: Sensor): Percept
}



trait Randomness {
  def rand: Random
}

trait DefaultRandomness extends Randomness {
  lazy val rand = new Random()
}