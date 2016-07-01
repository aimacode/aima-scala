package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleReflexAgent extends Agent {
  type State
  type InterpretInput = PartialFunction[Percept, State]

  type RuleMatch = PartialFunction[State, Action]

  lazy val agentFunction: AgentFunction = { percept =>
    val state = interpretInput(percept)
    ruleMatch(state)
  }

  def rules: RuleMatch

  def ruleMatch(state: State): Action = {
    rules.applyOrElse(state, _ => NoAction)
  }

  def interpretInput: InterpretInput
}
