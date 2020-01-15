package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleReflexAgentProgram[P, A] extends AgentProgram[P, A] {
  type State
  type InterpretInput = P => State

  type RuleMatch = State => A

  val agentFunction: AgentFunction = { percept =>
    val state = interpretInput(percept)
    ruleMatch(state)
  }

  def rules: RuleMatch

  def ruleMatch(state: State): A = {
    rules(state)
  }

  def interpretInput: InterpretInput
}
