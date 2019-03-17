package aima.core.agent.basic

import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.agent.basic.OnlineDFSAgentState.{RESULT, UNBACKTRACKED, UNTRIED}

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
    result: RESULT[ACTION, STATE],
    untried: UNTRIED[ACTION, STATE],
    unbacktracked: UNBACKTRACKED[STATE],
    previousState: Option[STATE], // s
    previousAction: Option[ACTION] // a
)

object OnlineDFSAgentState {

  type RESULT[ACTION, STATE]  = Map[(STATE, ACTION), STATE]
  type UNTRIED[ACTION, STATE] = Map[STATE, List[ACTION]]
  type UNBACKTRACKED[STATE]   = Map[STATE, List[STATE]]

  object Implicits {

    implicit class MapOps[K, V](m: Map[K, V]) {
      def put(k: K, v: V): Map[K, V] =
        m.updated(k, v)

      def computeIfAbsent(k: K, v: K => V): Map[K, V] = {
        if (m.contains(k)) {
          m
        } else {
          put(k, v(k))
        }
      }

      def transformValue(k: K, fv: Option[V] => V): Map[K, V] = {
        val oldValue = m.get(k)
        val newValue = fv(oldValue)
        m.updated(k, newValue)
      }

    }

    implicit class MapListOps[K, A](m: Map[K, List[A]]) {
      def safePop2(k: K): (Option[A], Map[K, List[A]]) = {
        val st = m.getOrElse(k, Nil)
        if (st.isEmpty) {
          (None, m)
        } else {
          val (popped, updatedStack) = (st.head, st.tail)
          (Some(popped), m.updated(k, updatedStack))
        }

      }
    }

  }
}

final class OnlineDFSAgent[PERCEPT, ACTION, STATE](identifyStateFor: IdentifyState[PERCEPT, STATE],
                                                   onlineProblem: OnlineSearchProblem[STATE, ACTION],
                                                   stop: ACTION)
    extends StatelessAgent[PERCEPT, ACTION, OnlineDFSAgentState[ACTION, STATE]] {

  import OnlineDFSAgentState.Implicits._

  type RESULT_TYPE        = RESULT[ACTION, STATE]
  type UNTRIED_TYPE       = UNTRIED[ACTION, STATE]
  type UNBACKTRACKED_TYPE = UNBACKTRACKED[STATE]

  override val agentFunction: AgentFunction = {
    case (percept, priorAgentState) =>
      val sPrime = identifyStateFor(percept)

      if (onlineProblem.isGoalState(sPrime)) {

        (stop, priorAgentState.copy(previousAction = Some(stop)))

      } else {

        val updatedUntried: UNTRIED_TYPE =
          priorAgentState.untried.computeIfAbsent(sPrime, _ => onlineProblem.actions(sPrime))

        val (updatedResult, updatedUnbacktracked): (RESULT_TYPE, UNBACKTRACKED_TYPE) =
          (priorAgentState.previousState, priorAgentState.previousAction) match {
            case (Some(_s), Some(_a)) if !priorAgentState.result.get((_s, _a)).contains(sPrime) =>
              (
                priorAgentState.result.put((_s, _a), sPrime),
                priorAgentState.unbacktracked.transformValue(sPrime, fv => fv.fold(List(_s))(st => _s :: st))
              )
            case _ =>
              (
                priorAgentState.result,
                priorAgentState.unbacktracked
              )
          }

        val updatedAgentState: OnlineDFSAgentState[ACTION, STATE] = {
          if (updatedUntried.get(sPrime).toList.flatten.isEmpty) {
            if (updatedUnbacktracked.get(sPrime).toList.flatten.isEmpty) {

              priorAgentState.copy(
                previousAction = Some(stop),
                previousState = Some(sPrime),
                untried = updatedUntried,
                result = updatedResult,
                unbacktracked = updatedUnbacktracked
              )

            } else {
              val (popped, updatedUnbacktracked2): (Option[STATE], UNBACKTRACKED_TYPE) =
                updatedUnbacktracked.safePop2(sPrime)

              val action: Option[ACTION] = updatedResult.toList.collectFirst {
                case ((ks, ka), vs) if ks == sPrime && popped.contains(vs) => ka
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
            val (popped, updatededUntried2): (Option[ACTION], UNTRIED_TYPE) =
              updatedUntried.safePop2(sPrime)

            priorAgentState.copy(
              previousAction = popped,
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
