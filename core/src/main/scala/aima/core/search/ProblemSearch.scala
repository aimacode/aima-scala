package aima.core.search

import aima.core.agent.Action

import scala.collection.immutable.Iterable

trait Problem {
  def initialState: State
  def isGoalState(state: State): Boolean
  def actions(state: State): List[Action]
  def result(state: State, action: Action): State
  def stepCost(state: State, action: Action, childPrime: State): Int
}

trait State

/**
  * @author Shawn Garner
  */
trait ProblemSearch {

  type Node

  def newChildNode(problem: Problem, parent: Node, action: Action): Node
  def solution(node: Node): List[Action]
}

trait FrontierSearch extends ProblemSearch {

  trait Frontier {
    def replaceByState(childNode: Node): Frontier
    def getNode(state: State): Option[Node]
    def removeLeaf: Option[(Node, Frontier)]
    def add(node: Node): Frontier
    def addAll(iterable: Iterable[Node]): Frontier
    def contains(state: State): Boolean
  }

  def newFrontier(state: State): Frontier
}
