package aima.core.search.adversarial

import scala.annotation.tailrec

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
object MinimaxDecision {
  import scala.math.Ordering.Double.IeeeOrdering
  def minMaxDecision[PLAYER, STATE, ACTION](
      g: Game[PLAYER, STATE, ACTION],
      noAction: ACTION
  ): (STATE) => ACTION = { (state: STATE) =>
    @tailrec def maxMinValue(Actions: List[ACTION]): ACTION = Actions match {
      case Nil                   => noAction
      case singularAction :: Nil => singularAction
      case action1 :: action2 :: rest =>
        maxMinValue(
          (if (minValue(g.result(state, action1)) > minValue(g.result(state, action2))) action1
           else action2) :: rest
        )
    }

    @tailrec def minMaxValue(Actions: List[ACTION]): ACTION = Actions match {
      case Nil                   => noAction
      case singularAction :: Nil => singularAction
      case action1 :: action2 :: rest =>
        minMaxValue(
          (if (maxValue(g.result(state, action1)) < maxValue(g.result(state, action2))) action1
           else action2) :: rest
        )
    }

    def maxValue(state: STATE): UtilityValue = {
      if (g.isTerminalState(state))
        g.getUtility(state)
      else
        minValue(g.result(state, maxMinValue(g.getActions(state))))
    }

    def minValue(state: STATE): UtilityValue = {
      if (g.isTerminalState(state))
        g.getUtility(state)
      else
        maxValue(g.result(state, minMaxValue(g.getActions(state))))
    }

    maxMinValue(g.getActions(g.initialState))
  }
}
