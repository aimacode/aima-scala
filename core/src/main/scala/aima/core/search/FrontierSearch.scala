package aima.core.search

import scala.collection.immutable.Iterable

trait Frontier[State, Action, Node <: SearchNode[State, Action]] {
  def replaceByState(childNode: Node): Frontier[State, Action, Node]
  def getNode(state: State): Option[Node]
  def removeLeaf: Option[(Node, Frontier[State, Action, Node])]
  def add(node: Node): Frontier[State, Action, Node]
  def addAll(iterable: Iterable[Node]): Frontier[State, Action, Node]
  def contains(state: State): Boolean
}

trait FrontierSearch[State, Action, Node <: SearchNode[State, Action]] {
  def newFrontier(state: State): Frontier[State, Action, Node]
}
