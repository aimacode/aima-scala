package aima.core.agent.basic

import aima.core.agent.basic.OnlineDFSAgent.IdentifyState

import scala.collection.immutable.Stack

/**
  * <pre>
  * function ONLINE-DFS-AGENT(s&prime;) returns an action
  *   inputs: s&prime;, a percept that identifies the current state
  *   persistent: result, a table, indexed by state and action, initially empty
  *               untried, a table that lists, for each state, the actions not yet tried
  *               unbacktracked, a table that lists, for each state, the backtracks not yet tried
  *               s, a, the previous state and action, initially null
  *
  *   if GOAL-TEST(s&prime;) then return stop
  *   if s&prime; is a new state (not in untried) then untried[s&prime;] &larr; ACTIONS(s&prime;)
  *   if s is not null and s&prime; &ne; result[s, a] then
  *       result[s, a] &larr; s&prime;
  *       add s to the front of the unbacktracked[s&prime;]
  *   if untried[s&prime;] is empty then
  *       if unbacktracked[s&prime;] is empty then return stop
  *       else a &larr; an action b such that result[s&prime;, b] = POP(unbacktracked[s&prime;])
  *   else a &larr; POP(untried[s&prime;])
  *   s &larr; s&prime;
  *   return a
  * </pre>
  *
  * @author Shawn Garner
  */
final case class OnlineDFSAgentState[ACTION, STATE](
    result: Map[(STATE, ACTION), STATE],
    untried: Map[STATE, Stack[ACTION]], //TODO: use List with some helpers to look the same
    unbacktracked: Map[STATE, Stack[STATE]],
    previousState: Option[STATE], // s
    previousAction: Option[ACTION] // a
)

final class OnlineDFSAgent[PERCEPT, ACTION, STATE](identifyStateFor: IdentifyState[PERCEPT, STATE],
                                                   onlineProblem: OnlineSearchProblem[STATE, ACTION],
                                                   stop: ACTION)
    extends StatelessAgent[PERCEPT, ACTION, OnlineDFSAgentState[ACTION, STATE]] {

  override val agentFunction: AgentFunction = {
    case (percept, priorAgentState) =>
      val sPrime = identifyStateFor(percept)
      if (onlineProblem.isGoalState(sPrime)) {
        (stop, priorAgentState.copy(previousAction = Some(stop)))
      } else {
        val updatedUntried: Map[STATE, Stack[ACTION]] = priorAgentState.untried
          .get(sPrime)
          .fold {
            priorAgentState.untried + (sPrime -> Stack[ACTION](onlineProblem.actions(sPrime): _*))
          }(_ => priorAgentState.untried)

        val (updatedResult, updatedUnbacktracked): (Map[(STATE, ACTION), STATE], Map[STATE, Stack[STATE]]) =
          (priorAgentState.previousState, priorAgentState.previousAction) match {
            case (Some(_s), Some(_a)) if !priorAgentState.result.get((_s, _a)).contains(sPrime) =>
              (
                priorAgentState.result + ((_s, _a) -> sPrime),
                priorAgentState.unbacktracked
                  .get(sPrime)
                  .fold {
                    priorAgentState.unbacktracked + (sPrime -> Stack(_s))
                  } { existing =>
                    priorAgentState.unbacktracked.updated(sPrime, existing.push(_s))
                  }
              )
            case _ =>
              (
                priorAgentState.result,
                priorAgentState.unbacktracked
              )
          }

        val updatedAgentState: OnlineDFSAgentState[ACTION, STATE] = {
          if (updatedUntried.getOrElse(sPrime, Stack[STATE]()).isEmpty) {
            if (updatedUnbacktracked.getOrElse(sPrime, Stack[STATE]()).isEmpty) {

              priorAgentState.copy(
                previousAction = Some(stop),
                previousState = Some(sPrime),
                untried = updatedUntried,
                result = updatedResult,
                unbacktracked = updatedUnbacktracked
              )

            } else {
              val (popped, updatedUnbacktracked2): (STATE, Map[STATE, Stack[STATE]]) = {
                val st: (STATE, Stack[STATE]) =
                  updatedUnbacktracked.getOrElse[Stack[STATE]](sPrime, Stack[STATE]()).pop2
                (st._1, updatedUnbacktracked.updated(sPrime, st._2))
              }
              val action: Option[ACTION] = updatedResult.toList.collectFirst {
                case ((ks, ka), vs) if ks == sPrime && vs == popped => ka
              }

              priorAgentState.copy(
                previousAction = action,
                previousState = Some(sPrime),
                untried = updatedUntried,
                result = updatedResult,
                unbacktracked = updatedUnbacktracked2
              )
            }
          } else {
            val (popped, updatededUntried2): (ACTION, Map[STATE, Stack[ACTION]]) = {
              val st: (ACTION, Stack[ACTION]) = updatedUntried.getOrElse(sPrime, Stack[ACTION]()).pop2 // TODO: pop2 is not safe
              (st._1, updatedUntried.updated(sPrime, st._2))
            }
            priorAgentState.copy(
              previousAction = Some(popped),
              previousState = Some(sPrime),
              untried = updatededUntried2,
              result = updatedResult,
              unbacktracked = updatedUnbacktracked
            )
          }
        }

        (updatedAgentState.previousAction.getOrElse(stop), updatedAgentState)
      }
  }

}

trait StatelessAgent[PERCEPT, ACTION, AGENT_STATE] {
  type AgentFunction = (PERCEPT, AGENT_STATE) => (ACTION, AGENT_STATE)

  def agentFunction: AgentFunction
}

trait OnlineSearchProblem[STATE, ACTION] {
  def actions(s: STATE): List[ACTION]
  def isGoalState(s: STATE): Boolean
  def stepCost(s: STATE, a: ACTION, sPrime: STATE): Double
}

object OnlineDFSAgent {
  type IdentifyState[PERCEPT, STATE] = PERCEPT => STATE
}
