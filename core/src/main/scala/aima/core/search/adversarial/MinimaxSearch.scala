package aima.core.search.adversarial

import scala.annotation.tailrec

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
object MinimaxDecision {
  def minMaxDecision[PLAYER, STATE, ACTION](
      g: Game[PLAYER, STATE, ACTION],
      noAction: ACTION
  ): (STATE, PLAYER) => ACTION = { (s: STATE, p: PLAYER) =>
    @tailrec def maxMinValue(Actions: List[ACTION], p: PLAYER): ACTION = Actions match {
      case Nil      => noAction
      case a :: Nil => a
      case a1 :: a2 :: rest =>
        maxMinValue((if (minValue(g.result(s, a1), p) > minValue(g.result(s, a2), p)) a1 else a2) :: rest, p)
    }

    @tailrec def minMaxValue(Actions: List[ACTION], p: PLAYER): ACTION = Actions match {
      case Nil      => noAction
      case a :: Nil => a
      case a1 :: a2 :: rest =>
        minMaxValue((if (maxValue(g.result(s, a1), p) < maxValue(g.result(s, a2), p)) a1 else a2) :: rest, p)
    }

    def maxValue(s: STATE, p: PLAYER): UtilityValue = {
      if (g.isTerminalState(s))
        g.getUtility(s, p)
      else
        minValue(g.result(s, maxMinValue(g.getActions(s), p)), p)
    }

    def minValue(s: STATE, p: PLAYER): UtilityValue = {
      if (g.isTerminalState(s))
        g.getUtility(s, p)
      else
        maxValue(g.result(s, minMaxValue(g.getActions(s), p)), p)
    }

    maxMinValue(g.getActions(s), p)
  }
}
