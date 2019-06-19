package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleReflexAgent[State, Action, Percept] extends Agent[Action, Percept] {
  type InterpretInput = Percept => State

  type RuleMatch = State => Action

  val agentFunction: AgentFunction = { percept =>
    val state = interpretInput(percept)
    ruleMatch(state)
  }

  def rules: RuleMatch

  def ruleMatch(state: State): Action = {
    rules(state)
  }

  def interpretInput: InterpretInput
}
