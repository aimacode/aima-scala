package aima.core.agent

/**
  * @author Shawn Garner
  */
trait SimpleProblemSolvingAgent[State, Action, Percept, Goal, Problem] extends Agent[Action, Percept] {

  def initialState: State
  def noAction: Action

  var actions = List.empty[Action]
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

  def updateState(state: State, percept: Percept): State
  def formulateGoal(state: State): Goal
  def formulateProblem(state: State, goal: Goal): Problem
  def search(problem: Problem): List[Action]
}
