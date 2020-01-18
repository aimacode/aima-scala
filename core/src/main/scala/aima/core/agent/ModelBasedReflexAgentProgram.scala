package aima.core.agent

/**
  * @author Shawn Garner
  */
trait ModelBasedReflexAgentProgram[PERCEPT, ACTION, STATE] extends AgentProgram[PERCEPT, ACTION] {
  type Model       = (STATE, ACTION) => STATE //a description of how the next state depends on the current state and action
  type UpdateState = (STATE, ACTION, PERCEPT, Model) => STATE
  type RuleMatch   = STATE => ACTION

  def initialState: STATE
  def noAction: ACTION

  var state: STATE   = initialState // the agent's current conception of the world state
  var action: ACTION = noAction     //most recent action

  val agentFunction: AgentFunction = { percept =>
    state = updateState(state, action, percept, model)
    action = ruleMatch(state)
    action
  }

  def rules: RuleMatch
  def model: Model // Figure depicts as persistent but doesn't define how to update it?

  def ruleMatch(state: STATE): ACTION = {
    rules(state)
  }

  def updateState: UpdateState
}
