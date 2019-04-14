package aima.core.search.adversarial

final case class UtilityValue(value: Double) extends AnyVal {
  def <(that: UtilityValue) = if (this.value < that.value) true else false
  def >(that: UtilityValue) = if (this.value > that.value) true else false

}

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
