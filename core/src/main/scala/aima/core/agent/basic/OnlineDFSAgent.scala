package aima.core.agent.basic

import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.agent.{Action, Agent, Percept}
import aima.core.search.State

import scala.collection.immutable.Stack
import scala.collection.mutable

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
                                                      untried: Map[STATE, Stack[ACTION]],
                                                      unbacktracked: Map[STATE, Stack[STATE]],
                                                      previousState: Option[STATE], // s
                                                      previousAction: Option[ACTION]  // a

                                                   )

final class OnlineDFSAgent[PERCEPT, ACTION, STATE](identifyStateFor: IdentifyState[PERCEPT, STATE], onlineProblem: OnlineSearchProblem[STATE, ACTION], stop: ACTION) extends StatelessAgent[PERCEPT, ACTION, OnlineDFSAgentState[ACTION, STATE]] {


  override val agentFunction: AgentFunction = {
    case (percept, priorAgentState) =>

    val sPrime = identifyStateFor(percept)
    if (onlineProblem.isGoalState(sPrime)) {
      stop
    } else {
      val newUntried = priorAgentState.untried.get(sPrime).fold {
          priorAgentState.untried + (sPrime -> onlineProblem.actions(sPrime))
        } (_ => priorAgentState.untried)


      val (updatedResult, updateUnbacktracked) = (priorAgentState.previousState, priorAgentState.previousAction) match {
        case (Some(_s), Some(_a)) if !priorAgentState.result.get((_s, _a)).contains(sPrime) =>
          (
            priorAgentState.result + ((_s, _a) -> sPrime),
            priorAgentState.unbacktracked.get(sPrime).fold{
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

      val action = {
        if (untried.getOrElse(sPrime, mutable.Stack.empty).isEmpty) {
          if (unbacktracked.getOrElse(sPrime, mutable.Stack.empty).isEmpty) {
            stop
          } else {
            val popped = unbacktracked.getOrElse(sPrime, mutable.Stack.empty).pop()
            result.toList.collectFirst {
              case ((ks, ka), vs) if ks == sPrime && vs == popped => ka
            }.get // TODO: safe?
          }
        } else {
          untried.getOrElse(sPrime, mutable.Stack.empty).pop()
        }
      }

      a = Some(action)
      s = Some(sPrime)

      action
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
