package aima.core.search.adversarial

import aima.core.search.adversarial.{Game, UtilityValue}
import aima.core.agent.{Action, NoAction}

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */

object MinimaxDecision {
    def minMaxDecision[S, A, P](g: Game[S][A][P]): S, P => A = {
        (s: S, p: P) => maxMinValue(g.getActions(s), p)
            
        def maxMinValue(actions: List[Action], p: P): Action = actions match {
            case Nil => NoAction
            case List(a: Action) => a
            case a1 :: a2 :: rest => maxMinValue( (if (g.minValue(a1, p) > g.minValue(a2, p)) a1 else a2) :: rest )
        }

        def minMaxValue(actions: List[Action], p: P): Action = actions match {
            case Nil => NoAction
            case List(a: Action) => a
            case a1 :: a2 :: rest => minMaxValue( (if (g.maxValue(a1, p) < g.maxValue(a2, p)) a1 else a2) :: rest )
        }

        def maxValue(s: S, p: P), : UtilityValue = {
            if (g.isTerminalState(s))
                g.getUtility(s, p)

            else
                maxMinValue(g.getActions(s), p)
        }

        def minValue(s: S, p: P): UtilityValue = {
            if (g.isTerminalState(s))
                g.getUtility(s, p)


            else
                minMaxValue(g.getActions(s), p)
        }
    }
}