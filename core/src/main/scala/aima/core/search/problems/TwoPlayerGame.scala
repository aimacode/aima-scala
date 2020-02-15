package aima.core.search.problems

import aima.core.search.adversarial._

/**
  * @author Aditya Lahiri
  * @author Shawn Garner
  */
object TwoPlayerGame {

  final case class Player(name: String)
  final case class State(name: String)
  final case class Action(name: String)

  sealed trait TwoPlayerGame extends Game[Player, State, Action]

  val impl: TwoPlayerGame = new TwoPlayerGame {
    val adjacencyMat = Map(
      "A" -> List(Action("B"), Action("C"), Action("D")),
      "B" -> List(Action("E"), Action("F"), Action("G")),
      "C" -> List(Action("H"), Action("I"), Action("J")),
      "D" -> List(Action("K"), Action("L"), Action("M"))
    )
    val TwoPlyStates = Map(
      "A" -> State("A"),
      "B" -> State("B"),
      "C" -> State("C"),
      "D" -> State("D"),
      "E" -> State("E"),
      "F" -> State("F"),
      "G" -> State("G"),
      "H" -> State("H"),
      "I" -> State("I"),
      "J" -> State("J"),
      "K" -> State("K"),
      "L" -> State("L"),
      "M" -> State("M")
    )

    @Override
    def initialState: State = TwoPlyStates("A")

    @Override
    def getPlayer(state: State): Player = state.name match {
      case "B" | "C" | "D" => Player("MIN")
      case _               => Player("MAX")
    }

    @Override
    def getActions(state: State): List[Action] = adjacencyMat(state.name)

    @Override
    def result(state: State, action: Action): State = TwoPlyStates(action.name)

    @Override
    def isTerminalState(state: State): Boolean = state.name match {
      case "A" | "B" | "C" | "D" => false
      case _                     => true

    }

    @Override
    def getUtility(state: State): UtilityValue = state.name match {
      case "E" => UtilityValue(3)
      case "F" => UtilityValue(12)
      case "G" => UtilityValue(8)
      case "H" => UtilityValue(2)
      case "I" => UtilityValue(4)
      case "J" => UtilityValue(6)
      case "K" => UtilityValue(14)
      case "L" => UtilityValue(5)
      case "M" => UtilityValue(2)
    }
  }
}
