package aima.core.search.uninformed

import aima.core.agent.Action

import scala.collection.immutable.Iterable

/**
  * @author Shawn Garner
  */
trait ProblemSearch {
  type State
  trait Problem {
    def initialState: State
    def isGoalState(state: State): Boolean
    def actions(state: State): List[Action]
  }

  type Node


  trait Frontier {
    def replaceByState(childNode: Node): Frontier
    def getNode(state: State): Option[Node]
    def removeLeaf: Option[(Node, Frontier)]
    def add(node: Node): Frontier
    def addAll(iterable: Iterable[Node]): Frontier
    def contains(state: State): Boolean
  }

  def search(problem: Problem): List[Action]

  def newChildNode(problem: Problem, node: Node, action: Action): Node
  def solution(node: Node): List[Action]

  def newFrontier(state: State): Frontier
}
