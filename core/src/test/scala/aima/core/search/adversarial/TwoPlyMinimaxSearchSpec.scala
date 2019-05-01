package aima.core.search.adversarial

import aima.core.search.problems.{Player, State, Action, TwoPlyGame}
import org.specs2.mutable.Specification

/**
  * @author Aditya Lahiri
  */
class TwoPlyMinimaxSearchSpec extends Specification {
  "The two-ply game " should {
    "should return action to state 1 from state 0" in {
      MinimaxDecision
        .minMaxDecision[Player, State, Action](TwoPlyGame, Action(-1))(TwoPlyGame.initialState, TwoPlyGame.Players(1))
        .stateNumber must beEqualTo(1) // Action(-1) denotes noAction
    }
  }
}
