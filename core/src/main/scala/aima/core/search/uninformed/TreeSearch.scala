package aima.core.search.uninformed

import aima.core.agent.Action

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * @author Shawn Garner
  */
trait TreeSearch {
  type State
  trait Problem {
    def initialState: State
    def isGoalState(state: State): Boolean
    def actions(state: State): List[Action]
  }
  trait Node {
    def state: State
  }

  def search(problem: Problem): List[Action] = {
    val initialFrontier = newFrontier(problem.initialState)

    @tailrec def searchHelper(frontier: Queue[Node]): List[Action] = {
      frontier.dequeueOption match {
        case None => List.empty[Action]
        case Some((leaf, _)) if problem.isGoalState(leaf.state) => solution(leaf)
        case Some((leaf, modifiedFrontier)) =>
          searchHelper(modifiedFrontier.enqueue(problem.actions(leaf.state).map(action => newChildNode(problem, leaf, action))))
      }
    }

    searchHelper(initialFrontier)
  }

  def newChildNode(problem: Problem, node: Node, action: Action): Node
  def solution(node: Node): List[Action]

  def newFrontier(state: State): Queue[Node]
}


