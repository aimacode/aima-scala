package aima.core.agent

/**
  * @author Shawn Garner
  */
trait ModelBasedReflexAgent[State, Action, Percept] extends Agent[Action, Percept] {
  type Model       = (State, Action) => State //a description of how the next state depends on the current state and action
  type UpdateState = (State, Action, Percept, Model) => State
  type RuleMatch   = State => Action

  def initialState: State
  def noAction: Action

  var state: State   = initialState // the agent's current conception of the world state
  var action: Action = noAction     //most recent action

  val agentFunction: AgentFunction = { percept =>
    state = updateState(state, action, percept, model)
    action = ruleMatch(state)
    action
  }

  def rules: RuleMatch
  def model: Model // Figure depicts as persistent but doesn't define how to update it?

  def ruleMatch(state: State): Action = {
    rules(state)
  }

  def updateState: UpdateState
}
