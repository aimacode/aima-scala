package aima.core.search.adversarial

import aima.core.agent.{Action, NoAction}

final case class UtilityValue(value: Double) extends AnyVal

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
trait Game[Player, State, Action] {
  def initialState: State
  def getPlayer(state: State): Player 
  def getActions(state: State): List[Action]
  def result(state: State, action: Action): State
  def isTerminalState(state: State): Boolean
  def getUtility(state: State, player: Player): UtilityValue
}