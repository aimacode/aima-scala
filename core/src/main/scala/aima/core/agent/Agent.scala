package aima.core.agent

/**
  * @author Shawn Garner
  */
trait Agent[E, P, A] {
  def actuators: List[Actuator[E, A]]
  def sensors: List[Sensor[E, P]]
  def agentProgram: AgentProgram[P, A]

  def run(e: E): E = {
    val actions: Seq[A] = for {
      sensor <- sensors
    } yield agentProgram.agentFunction(sensor.perceive(e))

    actions.foldLeft(e) { (env, action) =>
      actuators.foldLeft(env) { (env2, actuator) =>
        actuator.act(action, env2)
      }
    }
  }
}

trait AgentProgram[P, A] {
  type AgentFunction = P => A

  def agentFunction: AgentFunction
}

trait StatelessAgent[PERCEPT, ACTION, AGENT_STATE] {
  type AgentFunction = (PERCEPT, AGENT_STATE) => (ACTION, AGENT_STATE)

  def agentFunction: AgentFunction
}
