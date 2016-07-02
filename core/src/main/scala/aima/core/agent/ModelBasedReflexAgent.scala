package aima.core.agent

/**
  * @author Shawn Garner
  */
trait ModelBasedReflexAgent extends Agent {
  type State
  type Model
  type UpdateState = (Action, Percept, State, Model) => State
  type RuleMatch = PartialFunction[State, Action]

  def initialModel: Model
  def initialState: State

  var action: Action = NoAction
  var state: State = initialState
  var model: Model = initialModel

  lazy val agentFunction: AgentFunction = { percept =>
    state = updateState(action, percept, state, model)
    action = ruleMatch(state)
    action
  }

  def rules: RuleMatch

  def ruleMatch(state: State): Action = {
    rules.applyOrElse(state, (_: State) => NoAction)
  }

  def updateState: UpdateState
}
