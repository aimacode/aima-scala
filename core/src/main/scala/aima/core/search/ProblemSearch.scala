package aima.core.search

import scala.annotation.tailrec

trait Problem[State, Action] {
  def initialState: State
  def isGoalState(state: State): Boolean
  def actions(state: State): List[Action]
  def result(state: State, action: Action): State
  def stepCost(state: State, action: Action, childPrime: State): Int
}

sealed trait SearchNode[State, Action] {
  def state: State
  def action: Action
  def parent: Option[SearchNode[State, Action]]
}

case class StateNode[State, Action](state: State, action: Action, parent: Option[StateNode[State, Action]])
    extends SearchNode[State, Action]
case class CostNode[State, Action](state: State, cost: Int, action: Action, parent: Option[CostNode[State, Action]])
    extends SearchNode[State, Action]
case class HeuristicsNode[State, Action](
    state: State,
    gValue: Double,
    hValue: Option[Double],
    fValue: Option[Double],
    action: Action,
    parent: Option[CostNode[State, Action]]
) extends SearchNode[State, Action]

/**
  * @author Shawn Garner
  */
trait ProblemSearch[State, Action, Node <: SearchNode[State, Action]] {

  def newChildNode(problem: Problem[State, Action], parent: Node, action: Action): Node

  def solution(node: Node): List[Action] = {
    @tailrec def solutionHelper(n: Node, actions: List[Action]): List[Action] = {
      n.parent match {
        case None         => actions
        case Some(parent) => solutionHelper(parent, n.action :: actions)
      }
    }

    solutionHelper(node, Nil)
  }
}
