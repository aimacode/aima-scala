package aima.core.agent.basic

import aima.core.agent.StatelessAgent
import aima.core.agent.basic.LRTAStarAgent.IdentifyState
import aima.core.agent.basic.LRTAStarAgentState.{COST_ESTIMATES, RESULT}
import aima.core.fp.Eqv
import aima.core.fp.Eqv.Implicits._
import aima.core.search.api.OnlineSearchProblem

/**
  *
  *
  * @author Shawn Garner
  */
final class LRTAStarAgent[PERCEPT, ACTION, STATE: Eqv](
    identifyStateFor: IdentifyState[PERCEPT, STATE],
    onlineProblem: OnlineSearchProblem[ACTION, STATE],
    h: STATE => Double,
    stop: ACTION,
    undefinedState: STATE
) extends StatelessAgent[PERCEPT, ACTION, LRTAStarAgentState[ACTION, STATE]] {

  import LRTAStarAgentState.Implicits._

  type RESULT_TYPE         = RESULT[ACTION, STATE]
  type COST_ESTIMATES_TYPE = COST_ESTIMATES[STATE]

  def lrtaCost(s: STATE, a: ACTION, sPrime: STATE, H: COST_ESTIMATES_TYPE): Double = {
    if (sPrime === undefinedState) {
      h(s)
    } else {
      onlineProblem.stepCost(s, a, sPrime) + H(sPrime) // TODO: unsafe to do apply but should actually be in there
    }
  }

  override val agentFunction: AgentFunction = {
    case (percept, priorAgentState) =>
      val sPrime = identifyStateFor(percept)

      if (onlineProblem.isGoalState(sPrime)) {

        (stop, priorAgentState.copy(previousAction = Some(stop)))

      } else {

        val updatedH: COST_ESTIMATES_TYPE =
          priorAgentState.H.computeIfAbsent(sPrime, _ => h(sPrime))

        val (updatedResult, updatedH2): (RESULT_TYPE, COST_ESTIMATES_TYPE) =
          (priorAgentState.previousState, priorAgentState.previousAction) match {
            case (Some(_s), Some(_a)) if !priorAgentState.result.get(_s).flatMap(_.get(_a)).contains(sPrime) =>
              val resultOrigActionToState: Map[ACTION, STATE] =
                priorAgentState.result.getOrElse(_s, Map.empty[ACTION, STATE])
              val updatedResultActionToState
                  : Map[ACTION, STATE] = resultOrigActionToState.put(_a, sPrime) // TODO: could be less verbose with lense

              val finalResult = priorAgentState.result.put(_s, updatedResultActionToState)
              val newH = updatedH.put(
                _s,
                onlineProblem
                  .actions(_s)
                  .map(b => lrtaCost(_s, b, priorAgentState.result(_s)(b), updatedH))
                  .min // TODO: should use get and flatMap Option and convert to list
              )

              (
                finalResult,
                newH
              )
            case _ =>
              (
                priorAgentState.result,
                updatedH
              )
          }

        val newAction: ACTION = onlineProblem
          .actions(sPrime)
          .minBy(b => lrtaCost(sPrime, b, updatedResult(sPrime)(b), updatedH2))

        val updatedAgentState = priorAgentState.copy(
          result = updatedResult,
          H = updatedH2,
          previousState = Some(sPrime),
          previousAction = Some(newAction)
        )

        (newAction, updatedAgentState)
      }
  }

}

final case class LRTAStarAgentState[ACTION, STATE](
    result: RESULT[ACTION, STATE],
    H: COST_ESTIMATES[STATE],
    previousState: Option[STATE],  // s
    previousAction: Option[ACTION] // a
)

object LRTAStarAgentState {

  def apply[ACTION, STATE] =
    new LRTAStarAgentState[ACTION, STATE](
      result = Map.empty,
      H = Map.empty,
      previousState = None,
      previousAction = None
    )

  type RESULT[ACTION, STATE] = Map[STATE, Map[ACTION, STATE]]
  type COST_ESTIMATES[STATE] = Map[STATE, Double]

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

object LRTAStarAgent {
  type IdentifyState[PERCEPT, STATE] = PERCEPT => STATE
}
