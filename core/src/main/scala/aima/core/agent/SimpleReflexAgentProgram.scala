package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleReflexAgentProgram[PERCEPT, ACTION]
    extends AgentProgram[PERCEPT, ACTION] {
  type State
  type InterpretInput = PERCEPT => State

  type RuleMatch = State => ACTION

  val agentFunction: AgentFunction = { percept =>
    val state = interpretInput(percept)
    ruleMatch(state)
  }

  def rules: RuleMatch

  def ruleMatch(state: State): ACTION = {
    rules(state)
  }

  def interpretInput: InterpretInput
}
