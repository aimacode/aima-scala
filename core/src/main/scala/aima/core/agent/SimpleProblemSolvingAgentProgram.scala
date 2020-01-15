package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleProblemSolvingAgentProgram[PERCEPT, ACTION, STATE, GOAL, PROBLEM]
    extends AgentProgram[PERCEPT, ACTION] {

  def initialState: STATE
  def noAction: ACTION

  var actions = List.empty[ACTION]
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

  def updateState(state: STATE, percept: PERCEPT): STATE
  def formulateGoal(state: STATE): GOAL
  def formulateProblem(state: STATE, goal: GOAL): PROBLEM
  def search(problem: PROBLEM): List[ACTION]
}
