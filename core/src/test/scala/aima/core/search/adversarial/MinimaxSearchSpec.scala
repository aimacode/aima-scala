package aima.core.search.adversarial

import aima.core.search.problems.TwoPlayerGame
import org.specs2.mutable.Specification

class MinimaxSearchSpec extends Specification {
  import TwoPlayerGame.{Action, State, Player, impl}
  import impl._
  "Utility value of E" should {
    "be 3" in {
      getUtility(State("E")) must beEqualTo(UtilityValue(3))
    }
  }

  "Utility value of I" should {
    "be 4" in {
      getUtility(State("I")) must beEqualTo(UtilityValue(4))
    }
  }
  "Utility value of K" should {
    "be 14" in {
      getUtility(State("K")) must beEqualTo(UtilityValue(14))
    }
  }

  "Initial State" should {
    "be A" in {
      initialState must beEqualTo(State("A"))
    }
  }

  "First player in the game" should {
    "be MAX" in {
      getPlayer(initialState) must beEqualTo(Player("MAX"))
    }
  }

  "State to move to from intial state" should {
    "be B" in {
      MinimaxDecision.minMaxDecision[Player, State, Action](
        impl,
        Action("noAction")
      )(initialState) must beEqualTo(Action("B"))
    }
  }
}
