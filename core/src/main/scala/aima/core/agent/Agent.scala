package aima.core.agent

/**
  * @author Shawn Garner
  */
trait Agent[ENVIRONMENT, PERCEPT, ACTION] {
  def actuators: List[Actuator[ENVIRONMENT, ACTION]]
  def sensors: List[Sensor[ENVIRONMENT, PERCEPT]]
  def agentProgram: AgentProgram[PERCEPT, ACTION]

  def run(e: ENVIRONMENT): ENVIRONMENT = {
    val actions: Seq[ACTION] = for {
      sensor <- sensors
    } yield agentProgram.agentFunction(sensor.perceive(e))

    actions.foldLeft(e) { (env, action) =>
      actuators.foldLeft(env) { (env2, actuator) =>
        actuator.act(action, env2)
      }
    }
  }
}

trait AgentProgram[PERCEPT, ACTION] {
  type AgentFunction = PERCEPT => ACTION

  def agentFunction: AgentFunction
}

trait StatelessAgent[PERCEPT, ACTION, AGENT_STATE] {
  type AgentFunction = (PERCEPT, AGENT_STATE) => (ACTION, AGENT_STATE)

  def agentFunction: AgentFunction
}
