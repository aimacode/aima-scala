package aima.core.agent.basic

import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.agent.{Action, Agent, Percept}
import aima.core.search.State

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
class OnlineDFSAgent(identifyStateFor: IdentifyState, onlineProblem: OnlineSearchProblem, stop: Action) extends Agent {

  val result            = mutable.Map[(State, Action), State]()       // TODO: get rid of mutability
  val untried           = mutable.Map[State, mutable.Stack[Action]]() // TODO: get rid of mutability
  val unbacktracked     = mutable.Map[State, mutable.Stack[State]]()  // TODO: get rid of mutability
  var s: Option[State]  = None                                        // TODO: get rid of mutability
  var a: Option[Action] = None                                        // TODO: get rid of mutability

  override val agentFunction: AgentFunction = { percept: Percept =>
    val sPrime = identifyStateFor(percept)
    if (onlineProblem.isGoalState(sPrime)) {
      stop
    } else {
      untried.getOrElseUpdate(sPrime, mutable.Stack(onlineProblem.actions(sPrime): _*))

      (s, a) match {
        case (Some(_s), Some(_a)) if !result.get((_s, _a)).contains(sPrime) =>
          result.put((_s, _a), sPrime)
          unbacktracked.getOrElseUpdate(sPrime, mutable.Stack()).push(_s)
        case _ =>
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

trait OnlineSearchProblem {
  def actions(s: State): List[Action]
  def isGoalState(s: State): Boolean
  def stepCost(s: State, a: Action, sPrime: State): Double
}

object OnlineDFSAgent {
  type IdentifyState = Percept => State
}
