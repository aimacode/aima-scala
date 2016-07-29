package aima.core.agent

/**
  * @author Shawn Garner
  */
trait ModelBasedReflexAgent extends Agent {
  type State
  type Model       = PartialFunction[(State, Action), State] //a description of how the next state depends on the current state and action
  type UpdateState = (State, Action, Percept, Model) => State
  type RuleMatch   = PartialFunction[State, Action]

  def initialState: State

  var state: State   = initialState // the agent's current conception of the world state
  var action: Action = NoAction     //most recent action

  lazy val agentFunction: AgentFunction = { percept =>
    state = updateState(state, action, percept, model)
    action = ruleMatch(state)
    action
  }

  def rules: RuleMatch
  def model: Model // Figure depicts as persistent but doesn't define how to update it?

  def ruleMatch(state: State): Action = {
    rules.applyOrElse(state, (_: State) => NoAction)
  }

  def updateState: UpdateState
}
