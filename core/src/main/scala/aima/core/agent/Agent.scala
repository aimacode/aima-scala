package aima.core.agent

/**
  * @author Shawn Garner
  */
trait AgentProgram[Action, Percept] {
  def actuators: Seq[Actuator[Action, Percept]]
  def sensors: Seq[Sensor[Action, Percept]]
  def agent: Agent[Action, Percept]

  def run(environment: Environment[Action, Percept]): Environment[Action, Percept] = {
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

trait Agent[Action, Percept] {
  type AgentFunction = Percept => Action

  def agentFunction: AgentFunction
}
