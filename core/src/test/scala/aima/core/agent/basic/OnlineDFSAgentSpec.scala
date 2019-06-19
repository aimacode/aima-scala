package aima.core.agent.basic

import aima.core.agent.basic.OnlineDFSAgent.IdentifyState
import aima.core.fp.Eqv
import aima.core.search.api.OnlineSearchProblem
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
class OnlineDFSAgentSpec extends Specification with ScalaCheck {

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
    import MazeState.Implicits.mazeStateEq

    "find action solution from example" >> {
      val goalState = MazeXYState(3, 3)
      val agent = new OnlineDFSAgent[MazePositionPercept, MazeAction, MazeXYState](
        idStateFn,
        mazeProblem(goalState),
        StopAction
      )

      val initialState = MazeXYState(1, 1)
      val actions      = determineActions(initialState, agent)
      actions must_== List(Up, Down, Right, Up, Up, Left, Right, Down, Down, Right, Up, Up)
    }

    "find moveTO action solution from example" >> {
      val goalState = MazeXYState(3, 3)
      val agent = new OnlineDFSAgent[MazePositionPercept, MazeAction, MazeXYState](
        idStateFn,
        mazeProblem(goalState),
        StopAction
      )

      val initialState = MazeXYState(1, 1)
      val states       = determineMoveToStates(initialState, agent)
      val goToActions = states.map {
        case MazeXYState(x, y) => s"Go($x,$y)"
      }
      val display = goToActions.mkString("", " ", " NoOp")
      display must_== "Go(1,2) Go(1,1) Go(2,1) Go(2,2) Go(2,3) Go(1,3) Go(2,3) Go(2,2) Go(2,1) Go(3,1) Go(3,2) Go(3,3) NoOp"
    }

    import aima.core.agent.basic.OnlineDFSAgentSpec.Maze.MazeState.Implicits.arbMazeState
    "find solutions for all start and goal states" >> prop { (initialState: MazeXYState, goalState: MazeXYState) =>
      val agent = new OnlineDFSAgent[MazePositionPercept, MazeAction, MazeXYState](
        idStateFn,
        mazeProblem(goalState),
        StopAction
      )

      val states = determineMoveToStates(initialState, agent)

      states must beLike {
        case Nil => ok // already on goal
        case ls  => ls must contain[MazeXYState](goalState)
      }
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
    sealed trait MazeAction
    case object Up         extends MazeAction
    case object Right      extends MazeAction
    case object Left       extends MazeAction
    case object Down       extends MazeAction
    case object StopAction extends MazeAction

    final case class MazeXYState(x: Int, y: Int)

    object MazeState {
      object Implicits {
        implicit val arbMazeState: Arbitrary[MazeXYState] = Arbitrary {
          for {
            x <- Gen.oneOf(1, 2, 3)
            y <- Gen.oneOf(1, 2, 3)
          } yield MazeXYState(x, y)
        }

        implicit val mazeStateEq: Eqv[MazeXYState] = new Eqv[MazeXYState] {
          override def eqv(a1: MazeXYState, a2: MazeXYState): Boolean =
            a1.x == a2.x && a1.y == a2.y

        }
      }
    }

    final case class MazePositionPercept(position: Int)

    def determineActions(
        initialState: MazeXYState,
        agent: OnlineDFSAgent[MazePositionPercept, MazeAction, MazeXYState]
    ): List[MazeAction] = {

      @tailrec
      def d(
          s: MazeXYState,
          currentAgentState: OnlineDFSAgentState[MazeAction, MazeXYState],
          acc: List[MazeAction]
      ): List[MazeAction] = {
        val p                           = stateToPerceptFn(s)
        val (action, updatedAgentState) = agent.agentFunction(p, currentAgentState)
        if (action == StopAction) {
          acc.reverse
        } else {
          val statePrime = nextState(s, action)

          d(statePrime, updatedAgentState, action :: acc)
        }

      }

      d(initialState, OnlineDFSAgentState[MazeAction, MazeXYState], Nil)
    }
    def determineMoveToStates(
        initialState: MazeXYState,
        agent: OnlineDFSAgent[MazePositionPercept, MazeAction, MazeXYState]
    ): List[MazeXYState] = {

      @tailrec
      def d(
          s: MazeXYState,
          currentAgentState: OnlineDFSAgentState[MazeAction, MazeXYState],
          acc: List[MazeXYState]
      ): List[MazeXYState] = {
        val p                           = stateToPerceptFn(s)
        val (action, updatedAgentState) = agent.agentFunction(p, currentAgentState)
        if (action == StopAction) {
          acc.reverse
        } else {
          val statePrime = nextState(s, action)

          d(statePrime, updatedAgentState, statePrime :: acc)
        }

      }

      d(initialState, OnlineDFSAgentState[MazeAction, MazeXYState], Nil)
    }

    val idStateFn: IdentifyState[MazePositionPercept, MazeXYState] = {
      case MazePositionPercept(0) => MazeXYState(1, 1)
      case MazePositionPercept(1) => MazeXYState(2, 1)
      case MazePositionPercept(2) => MazeXYState(3, 1)
      case MazePositionPercept(3) => MazeXYState(1, 2)
      case MazePositionPercept(4) => MazeXYState(2, 2)
      case MazePositionPercept(5) => MazeXYState(3, 2)
      case MazePositionPercept(6) => MazeXYState(1, 3)
      case MazePositionPercept(7) => MazeXYState(2, 3)
      case MazePositionPercept(8) => MazeXYState(3, 3)
      case _                      => MazeXYState(1, 1) // should not be called
    }

    def nextState(state: MazeXYState, action: MazeAction): MazeXYState = action match {
      case Up         => MazeXYState(state.x, state.y + 1)
      case Down       => MazeXYState(state.x, state.y - 1)
      case Right      => MazeXYState(state.x + 1, state.y)
      case Left       => MazeXYState(state.x - 1, state.y)
      case StopAction => state
    }

    val stateToPerceptFn: MazeXYState => MazePositionPercept = {
      case MazeXYState(1, 1) => MazePositionPercept(0)
      case MazeXYState(2, 1) => MazePositionPercept(1)
      case MazeXYState(3, 1) => MazePositionPercept(2)
      case MazeXYState(1, 2) => MazePositionPercept(3)
      case MazeXYState(2, 2) => MazePositionPercept(4)
      case MazeXYState(3, 2) => MazePositionPercept(5)
      case MazeXYState(1, 3) => MazePositionPercept(6)
      case MazeXYState(2, 3) => MazePositionPercept(7)
      case MazeXYState(3, 3) => MazePositionPercept(8)
      case _                 => MazePositionPercept(0) // should not be called
    }

    def mazeProblem(goal: MazeXYState) = new OnlineSearchProblem[MazeAction, MazeXYState] {
      override def actions(s: MazeXYState): List[MazeAction] = s match {
        case MazeXYState(1, 1) => List(Up, Right)
        case MazeXYState(2, 1) => List(Up, Right, Left)
        case MazeXYState(3, 1) => List(Up, Left)
        case MazeXYState(1, 2) => List(Down)
        case MazeXYState(2, 2) => List(Up, Down)
        case MazeXYState(3, 2) => List(Up, Down)
        case MazeXYState(1, 3) => List(Right)
        case MazeXYState(2, 3) => List(Left, Down)
        case MazeXYState(3, 3) => List(Down)
        case _                 => List()
      }
      override def isGoalState(s: MazeXYState): Boolean = s == goal

      override def stepCost(s: MazeXYState, a: MazeAction, sPrime: MazeXYState): Double =
        ??? // not used
    }

  }
}
