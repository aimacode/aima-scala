package aima.core.search

import aima.core.agent.Action

import scala.annotation.tailrec
import scala.collection.immutable.Iterable

trait Problem {
  def initialState: State
  def isGoalState(state: State): Boolean
  def actions(state: State): List[Action]
  def result(state: State, action: Action): State
  def stepCost(state: State, action: Action, childPrime: State): Int
}

trait State

sealed trait SearchNode {
  def state: State
  def action: Action
  def parent: Option[SearchNode]
}

case class StateNode(state: State, action: Action, parent: Option[StateNode])          extends SearchNode
case class CostNode(state: State, cost: Int, action: Action, parent: Option[CostNode]) extends SearchNode
case class HeuristicsNode(state: State,
                          gValue: Double,
                          hValue: Option[Double],
                          fValue: Option[Double],
                          action: Action,
                          parent: Option[CostNode])
    extends SearchNode

/**
  * @author Shawn Garner
  */
trait ProblemSearch {

  type Node <: SearchNode

  def newChildNode(problem: Problem, parent: Node, action: Action): Node

  def solution(node: Node): List[Action] = {
    @tailrec def solutionHelper(n: SearchNode, actions: List[Action]): List[Action] = {
      n.parent match {
        case None         => actions
        case Some(parent) => solutionHelper(parent, n.action :: actions)
      }
    }

    solutionHelper(node, Nil)
  }
}

trait Frontier[Node <: SearchNode] {
  def replaceByState(childNode: Node): Frontier[Node]
  def getNode(state: State): Option[Node]
  def removeLeaf: Option[(Node, Frontier[Node])]
  def add(node: Node): Frontier[Node]
  def addAll(iterable: Iterable[Node]): Frontier[Node]
  def contains(state: State): Boolean
}

trait FrontierSearch {
  type Node <: SearchNode

  def newFrontier(state: State): Frontier[Node]
}
