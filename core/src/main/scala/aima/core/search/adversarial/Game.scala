package aima.core.search.adversarial

final case class UtilityValue(value: Double) extends AnyVal {
  def <(that: UtilityValue)  = if (this.value < that.value) true else false
  def >(that: UtilityValue)  = if (this.value > that.value) true else false
  def ==(that: UtilityValue) = if (this.value == that.value) true else false

}

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
trait Game[P <: Player, S <: State, A <: Action] {
  def initialState: S
  def getPlayer(state: S): P
  def getActions(state: S): List[A]
  def result(state: S, action: A): S
  def isTerminalState(state: S): Boolean
  def getUtility(state: S): UtilityValue
}

trait Player {}

trait State {}

trait Action {}
