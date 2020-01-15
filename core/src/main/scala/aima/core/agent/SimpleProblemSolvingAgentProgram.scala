package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleProblemSolvingAgentProgram[P, A, S, G, PB]
    extends AgentProgram[P, A] {

  def initialState: S
  def noAction: A

  var actions = List.empty[A]
  var state   = initialState

  def agentFunction: AgentFunction = { percept =>
    state = updateState(state, percept)
    if (actions.isEmpty) {
      val goal    = formulateGoal(state)
      val problem = formulateProblem(state, goal)
      actions = search(problem)
    }

    val (firstAction, restOfActions) = actions match {
      case Nil           => (noAction, Nil)
      case first :: rest => (first, rest)
    }

    actions = restOfActions
    firstAction
  }

  def updateState(state: S, percept: P): S
  def formulateGoal(state: S): G
  def formulateProblem(state: S, goal: G): PB
  def search(problem: PB): List[A]
}
