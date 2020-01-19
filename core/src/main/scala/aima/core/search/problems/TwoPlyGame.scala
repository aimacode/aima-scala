package aima.core.search.problems

import aima.core.search.adversarial.{Game, UtilityValue}

/**
  * @author Aditya Lahiri
  */
sealed abstract class Player
object PlayerMax extends Player //PlayerMax aims to obtain maximum value
object PlayerMin extends Player

case class State(stateNumber: Int)  extends AnyVal
case class Action(stateNumber: Int) extends AnyVal

object TwoPlyGame extends Game[Player, State, Action] {
  val adjacencyMat = Map(
    0 -> List(Action(1), Action(2), Action(3)),
    1 -> List(Action(4), Action(5), Action(6)),
    2 -> List(Action(7), Action(8), Action(9)),
    3 -> List(Action(10), Action(11), Action(12))
  )
  val States = Map(
    0  -> State(0),
    1  -> State(1),
    2  -> State(2),
    3  -> State(3),
    4  -> State(4),
    5  -> State(5),
    6  -> State(6),
    7  -> State(7),
    8  -> State(8),
    9  -> State(9),
    10 -> State(10),
    11 -> State(11),
    12 -> State(12)
  )

  def initialState: State = States(0)

  def getPlayer(state: State): Player = state.stateNumber match {
    case 0 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 => PlayerMax
    case 1 | 2 | 3                                => PlayerMin
  }

  def getActions(state: State): List[Action] = adjacencyMat(state.stateNumber)

  def result(state: State, action: Action): State = States(action.stateNumber)

  def isTerminalState(state: State): Boolean = state.stateNumber match {
    case 0 | 1 | 2 | 3                        => false
    case 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 => true

  }

  def getUtility(state: State, player: Player): UtilityValue = state.stateNumber match {
    case 4  => UtilityValue(3)
    case 5  => UtilityValue(12)
    case 6  => UtilityValue(8)
    case 7  => UtilityValue(2)
    case 8  => UtilityValue(4)
    case 9  => UtilityValue(6)
    case 10 => UtilityValue(14)
    case 11 => UtilityValue(5)
    case 12 => UtilityValue(2)
  }
}
