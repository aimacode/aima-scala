package aima.core.agent.basic

import aima.core.agent.{Action, NoAction, Percept}
import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.search.State
import org.specs2.mutable.Specification

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
class OnlineDFSAgentSpec extends Specification {

  /*
 3    |   x G
   xxxx---x--
 2    x   x
   ---x---x--
 1  S |   |
    1   2   3

 x is impassable
 | or - is passable
 S is start
 G is goal

   */
  "maze Figure 4.19" should {
    import OnlineDFSAgentSpec.Maze._

    "find solution from example" >> {
      val goalState = MazeXY(3, 3)
      val agent = new OnlineDFSAgent(
        idStateFn,
        mazeProblem(goalState),
        NoAction
      )

      val initialState = MazeXY(1, 1)
      val actions      = determineActions(initialState, agent)
      actions must_== List(Up, Down, Right, Up, Up, Left, Right, Down, Down, Right, Up, Up)

    }

  }

}

object OnlineDFSAgentSpec {
  /*

 3  6 | 7 | 8
   ---|---|--
 2  3 | 4 | 5
   ---|---|--
 1  0 | 1 | 2
    1   2   3

 State is (x,y) eg (2,2) is middle
 Percept is number at state eg Percept 4 is State (2,2)

  **/
  object Maze {
    sealed trait MazeAction extends Action
    case object Up          extends MazeAction
    case object Right       extends MazeAction
    case object Left        extends MazeAction
    case object Down        extends MazeAction

    sealed trait MazeState                  extends State
    final case class MazeXY(x: Int, y: Int) extends MazeState

    sealed trait MazePercept                            extends Percept
    final case class MazePositionPercept(position: Int) extends MazePercept

    def determineActions(initialState: MazeState, agent: OnlineDFSAgent): List[Action] = {

      @tailrec def d(s: State, acc: List[Action]): List[Action] = {
        val p      = stateToPerceptFn(s)
        val action = agent.agentFunction(p)
        if (action == NoAction) {
          acc.reverse
        } else {
          val statePrime = nextState(s, action)

          d(statePrime, action :: acc)
        }

      }

      d(initialState, Nil)
    }

    val idStateFn: IdentifyState = {
      case MazePositionPercept(0) => MazeXY(1, 1)
      case MazePositionPercept(1) => MazeXY(2, 1)
      case MazePositionPercept(2) => MazeXY(3, 1)
      case MazePositionPercept(3) => MazeXY(1, 2)
      case MazePositionPercept(4) => MazeXY(2, 2)
      case MazePositionPercept(5) => MazeXY(3, 2)
      case MazePositionPercept(6) => MazeXY(1, 3)
      case MazePositionPercept(7) => MazeXY(2, 3)
      case MazePositionPercept(8) => MazeXY(3, 3)
    }

    def nextState(state: State, action: Action): State = (state, action) match {
      case (MazeXY(x, y), Up)    => MazeXY(x, y + 1)
      case (MazeXY(x, y), Down)  => MazeXY(x, y - 1)
      case (MazeXY(x, y), Right) => MazeXY(x + 1, y)
      case (MazeXY(x, y), Left)  => MazeXY(x - 1, y)
    }

    val stateToPerceptFn: State => Percept = {
      case MazeXY(1, 1) => MazePositionPercept(0)
      case MazeXY(2, 1) => MazePositionPercept(1)
      case MazeXY(3, 1) => MazePositionPercept(2)
      case MazeXY(1, 2) => MazePositionPercept(3)
      case MazeXY(2, 2) => MazePositionPercept(4)
      case MazeXY(3, 2) => MazePositionPercept(5)
      case MazeXY(1, 3) => MazePositionPercept(6)
      case MazeXY(2, 3) => MazePositionPercept(7)
      case MazeXY(3, 3) => MazePositionPercept(8)
    }

    def mazeProblem(goal: MazeXY) = new OnlineSearchProblem {
      override def actions(s: State): List[Action] = s match {
        case MazeXY(1, 1) => List(Up, Right)
        case MazeXY(2, 1) => List(Up, Right, Left)
        case MazeXY(3, 1) => List(Up, Left)
        case MazeXY(1, 2) => List(Down)
        case MazeXY(2, 2) => List(Up, Down)
        case MazeXY(3, 2) => List(Up, Down)
        case MazeXY(1, 3) => List(Right)
        case MazeXY(2, 3) => List(Left, Down)
        case MazeXY(3, 3) => List(Down)

      }
      override def isGoalState(s: State): Boolean = s match {
        case MazeXY(x, y) if x == goal.x && y == goal.y => true
        case _                                          => false
      }
      override def stepCost(s: State, a: Action, sPrime: State): Double = ???
    }

  }
}
