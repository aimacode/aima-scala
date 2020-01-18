package aima.core.agent.basic

import aima.core.agent.StatelessAgent
import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.agent.basic.OnlineDFSAgentState.{RESULT, UNBACKTRACKED, UNTRIED}
import aima.core.fp.Eqv
import aima.core.fp.Eqv.Implicits._
import aima.core.search.api.OnlineSearchProblem

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
final class OnlineDFSAgent[PERCEPT, ACTION, STATE: Eqv](
    identifyStateFor: IdentifyState[PERCEPT, STATE],
    onlineProblem: OnlineSearchProblem[ACTION, STATE],
    stop: ACTION
) extends StatelessAgent[PERCEPT, ACTION, OnlineDFSAgentState[ACTION, STATE]] {

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
            case (Some(_s), Some(_a)) if !priorAgentState.result.get(_s).flatMap(_.get(_a)).contains(sPrime) =>
              val resultOrigActionToState: Map[ACTION, STATE] =
                priorAgentState.result.getOrElse(_s, Map.empty[ACTION, STATE])
              val updatedResultActionToState
                  : Map[ACTION, STATE] = resultOrigActionToState.put(_a, sPrime) // TODO: could be less verbose with lense
              (
                priorAgentState.result.put(_s, updatedResultActionToState),
                priorAgentState.unbacktracked.transformValue(sPrime, fv => fv.fold(List(_s))(st => _s :: st))
              )
            case _ =>
              (
                priorAgentState.result,
                priorAgentState.unbacktracked
              )
          }

        val updatedUntriedList: List[ACTION] = updatedUntried.get(sPrime).toList.flatten

        val updatedAgentState: OnlineDFSAgentState[ACTION, STATE] = updatedUntriedList match {
          case Nil =>
            val unbacktrackedList: List[STATE] = updatedUnbacktracked.get(sPrime).toList.flatten
            unbacktrackedList match {
              case Nil =>
                priorAgentState.copy(
                  previousAction = Some(stop),
                  previousState = Some(sPrime),
                  untried = updatedUntried,
                  result = updatedResult,
                  unbacktracked = updatedUnbacktracked
                )

              case popped :: remainingUnbacktracked =>
                val action: Option[ACTION] =
                  updatedResult.getOrElse(sPrime, Map.empty[ACTION, STATE]).toList.collectFirst {
                    case (action, state) if popped === state => action
                  }

                priorAgentState.copy(
                  previousAction = action,
                  previousState = Some(sPrime),
                  untried = updatedUntried,
                  result = updatedResult,
                  unbacktracked = updatedUnbacktracked.updated(sPrime, remainingUnbacktracked)
                )
            }

          case popped :: remainingUntried =>
            priorAgentState.copy(
              previousAction = Some(popped),
              previousState = Some(sPrime),
              untried = updatedUntried.updated(sPrime, remainingUntried),
              result = updatedResult,
              unbacktracked = updatedUnbacktracked
            )
        }

        (updatedAgentState.previousAction.getOrElse(stop), updatedAgentState)
      }
  }

}

final case class OnlineDFSAgentState[ACTION, STATE](
    result: RESULT[ACTION, STATE],
    untried: UNTRIED[ACTION, STATE],
    unbacktracked: UNBACKTRACKED[STATE],
    previousState: Option[STATE],  // s
    previousAction: Option[ACTION] // a
)

object OnlineDFSAgentState {

  def apply[ACTION, STATE] =
    new OnlineDFSAgentState[ACTION, STATE](
      result = Map.empty,
      untried = Map.empty,
      unbacktracked = Map.empty,
      previousState = None,
      previousAction = None
    )

  type RESULT[ACTION, STATE]  = Map[STATE, Map[ACTION, STATE]]
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

  }
}

object OnlineDFSAgent {
  type IdentifyState[PERCEPT, STATE] = PERCEPT => STATE
}
