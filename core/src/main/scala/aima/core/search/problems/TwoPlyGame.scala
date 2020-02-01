package aima.core.search.problems

import aima.core.search.adversarial._

/**
  * @author Aditya Lahiri
  */
case class TwoPlyPlayer(name: String) extends Player

case class TwoPlyState(name: String)  extends State
case class TwoPlyAction(name: String) extends Action

object TwoPlyGame extends Game[TwoPlyPlayer, TwoPlyState, TwoPlyAction] {
  val adjacencyMat = Map(
    "A" -> List(TwoPlyAction("B"), TwoPlyAction("C"), TwoPlyAction("D")),
    "B" -> List(TwoPlyAction("E"), TwoPlyAction("F"), TwoPlyAction("G")),
    "C" -> List(TwoPlyAction("H"), TwoPlyAction("I"), TwoPlyAction("J")),
    "D" -> List(TwoPlyAction("K"), TwoPlyAction("L"), TwoPlyAction("M"))
  )
  val TwoPlyStates = Map(
    "A" -> TwoPlyState("A"),
    "B" -> TwoPlyState("B"),
    "C" -> TwoPlyState("C"),
    "D" -> TwoPlyState("D"),
    "E" -> TwoPlyState("E"),
    "F" -> TwoPlyState("F"),
    "G" -> TwoPlyState("G"),
    "H" -> TwoPlyState("H"),
    "I" -> TwoPlyState("I"),
    "J" -> TwoPlyState("J"),
    "K" -> TwoPlyState("K"),
    "L" -> TwoPlyState("L"),
    "M" -> TwoPlyState("M")
  )

  @Override
  def initialState: TwoPlyState = TwoPlyStates("A")

  @Override
  def getPlayer(state: TwoPlyState): TwoPlyPlayer = state.name match {
    case "B" | "C" | "D" => TwoPlyPlayer("MIN")
    case _               => TwoPlyPlayer("MAX")
  }

  @Override
  def getActions(state: TwoPlyState): List[TwoPlyAction] = adjacencyMat(state.name)

  @Override
  def result(state: TwoPlyState, action: TwoPlyAction): TwoPlyState = TwoPlyStates(action.name)

  @Override
  def isTerminalState(state: TwoPlyState): Boolean = state.name match {
    case "A" | "B" | "C" | "D" => false
    case _                     => true

  }

  @Override
  def getUtility(state: TwoPlyState): UtilityValue = state.name match {
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
