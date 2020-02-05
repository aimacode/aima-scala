package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleReflexAgentProgram[PERCEPT, ACTION, STATE] extends AgentProgram[PERCEPT, ACTION] {
  type InterpretInput = PERCEPT => STATE

  type RuleMatch = STATE => ACTION

  val agentFunction: AgentFunction = { percept =>
    val state = interpretInput(percept)
    ruleMatch(state)
  }

  def rules: RuleMatch

  def ruleMatch(state: STATE): ACTION = {
    rules(state)
  }

  def interpretInput: InterpretInput
}
