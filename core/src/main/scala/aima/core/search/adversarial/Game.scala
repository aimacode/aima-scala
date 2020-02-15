package aima.core.search.adversarial

final case class UtilityValue(value: Double) extends AnyVal

object UtilityValue {
  implicit def utilityValueOrdering(implicit dOrdering: Ordering[Double]): Ordering[UtilityValue] =
    new Ordering[UtilityValue] {
      override def compare(x: UtilityValue, y: UtilityValue): Int = dOrdering.compare(x.value, y.value)
    }
  implicit class UtilityValueOps(value: UtilityValue) {
    def <(other: UtilityValue)(implicit o: Ordering[UtilityValue]): Boolean = {
      o.compare(value, other) < 0
    }

    def >(other: UtilityValue)(implicit o: Ordering[UtilityValue]): Boolean = {
      o.compare(value, other) > 0
    }

    def ==(other: UtilityValue)(implicit o: Ordering[UtilityValue]): Boolean = {
      o.compare(value, other) == 0
    }
  }
}

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
trait Game[PLAYER, STATE, ACTION] {
  def initialState: STATE
  def getPlayer(state: STATE): PLAYER
  def getActions(state: STATE): List[ACTION]
  def result(state: STATE, action: ACTION): STATE
  def isTerminalState(state: STATE): Boolean
  def getUtility(state: STATE): UtilityValue
}
