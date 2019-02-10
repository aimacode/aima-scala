package aima.core.search.adversarial

import aima.core.agent.Action

final case class UtilityValue[T](value: Numeric[T]) extends AnyVal

/**
  * @author Aditya Lahiri
  */
trait Game[Player, State, Action] {
  def initialState: State
  def getPlayer(state: State): Player 
  def getActions(state: State): List[Action]
  def result(state: State, action: Action): State
  def getUtility(state: State, player: Player): UtilityValue[Double]
}