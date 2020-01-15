package aima.core.agent

/**
  * @author Shawn Garner
  */
trait ModelBasedReflexAgentProgram[P, A, S] extends AgentProgram[P, A] {
  type Model       = (S, A) => S //a description of how the next state depends on the current state and action
  type UpdateState = (S, A, P, Model) => S
  type RuleMatch   = S => A

  def initialState: S
  def noAction: A

  var state
      : S       = initialState // the agent's current conception of the world state
  var action: A = noAction     //most recent action

  val agentFunction: AgentFunction = { percept =>
    state = updateState(state, action, percept, model)
    action = ruleMatch(state)
    action
  }

  def rules: RuleMatch
  def model: Model // Figure depicts as persistent but doesn't define how to update it?

  def ruleMatch(state: S): A = {
    rules(state)
  }

  def updateState: UpdateState
}
