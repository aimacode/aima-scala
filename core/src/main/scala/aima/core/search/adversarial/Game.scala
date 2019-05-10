package aima.core.search.adversarial

final case class UtilityValue(value: Double) extends AnyVal

/**
  * @author Aditya Lahiri
  */
trait Game[Player, State, Action] {
  def initialState: State
  def getPlayer(state: State): Player
  def getActions(state: State): List[Action]
  def result(state: State, action: Action): State
  def getUtility(state: State, player: Player): UtilityValue
}
