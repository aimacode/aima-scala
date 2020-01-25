package aima.core.agent

/**
  * @author Shawn Garner
  * @author Damien Favre
  */
trait Agent[ENVIRONMENT, PERCEPT, ACTION] {
  def actuators: List[Actuator[ENVIRONMENT, ACTION]]
  def sensors: List[Sensor[ENVIRONMENT, PERCEPT]]
  def agentProgram: AgentProgram[PERCEPT, ACTION]

  def run(e: ENVIRONMENT): (ENVIRONMENT, RunDetail[PERCEPT, ACTION]) = {
    val percepts: List[PERCEPT] = sensors.flatMap(_.perceive(e))
    val actions: List[ACTION]   = percepts.map(agentProgram.agentFunction)

    val newEnvironment = actions.foldLeft(e) { (env, action) =>
      actuators.foldLeft(env) { (env2, actuator) =>
        actuator.act(action, env2).getOrElse(env2)
      }
    }
    (newEnvironment, RunDetail(percepts, actions))
  }
}

case class RunDetail[PERCEPT, ACTION](
    percepts: List[PERCEPT],
    actions: List[ACTION]
)

trait AgentProgram[PERCEPT, ACTION] {
  type AgentFunction = PERCEPT => ACTION

  def agentFunction: AgentFunction
}

trait StatelessAgent[PERCEPT, ACTION, AGENT_STATE] {
  type AgentFunction = (PERCEPT, AGENT_STATE) => (ACTION, AGENT_STATE)

  def agentFunction: AgentFunction
}
