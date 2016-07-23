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

    actions.foldLeft(environment) { (env, action) =>
      actuators.foldLeft(env) { (env2, actuator) =>
        actuator.act(action, env2)
      }
    }
  }
}

trait Agent {
  type AgentFunction = Percept => Action

  def agentFunction: AgentFunction
}
