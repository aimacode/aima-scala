package aima.core.search.adversarial

import aima.core.search.problems.{TwoPlyPlayer, TwoPlyState, TwoPlyAction, TwoPlyGame}
import org.specs2.mutable.Specification

class MinimaxSearchSpec extends Specification {
  "Utility value of E" should {
    "be 3" in {
      TwoPlyGame.getUtility(TwoPlyState("E")) must beEqualTo(UtilityValue(3))
    }
  }

  "Utility value of I" should {
    "be 4" in {
      TwoPlyGame.getUtility(TwoPlyState("I")) must beEqualTo(UtilityValue(4))
    }
  }
  "Utility value of K" should {
    "be 14" in {
      TwoPlyGame.getUtility(TwoPlyState("K")) must beEqualTo(UtilityValue(14))
    }
  }

  "Initial State" should {
    "be A" in {
      TwoPlyGame.initialState must beEqualTo(TwoPlyState("A"))
    }
  }

  "First player in the game" should {
    "be MAX" in {
      TwoPlyGame.getPlayer(TwoPlyGame.initialState) must beEqualTo(TwoPlyPlayer("MAX"))
    }
  }

  "State to move to from intial state" should {
    "be B" in {
      MinimaxDecision.minMaxDecision[TwoPlyPlayer, TwoPlyState, TwoPlyAction](
        TwoPlyGame,
        TwoPlyAction("noAction")
      )(TwoPlyGame.initialState) must beEqualTo(TwoPlyAction("B"))
    }
  }
}
