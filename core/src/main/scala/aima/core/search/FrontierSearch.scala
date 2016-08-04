package aima.core.search

import scala.collection.immutable.Iterable

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
