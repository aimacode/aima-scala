package aima.core.search.uninformed

import aima.core.agent.Action

import scala.collection.immutable.Queue

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
  trait Node {
    def state: State
  }

  def search(problem: Problem): List[Action]

  def newChildNode(problem: Problem, node: Node, action: Action): Node
  def solution(node: Node): List[Action]

  def newFrontier(state: State): Queue[Node]
}
