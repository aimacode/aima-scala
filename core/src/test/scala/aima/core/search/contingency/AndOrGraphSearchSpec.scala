package aima.core.search.contingency

import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

object AndOrGraphSearchSpec {
  sealed trait Action
  case object MoveLeft  extends Action
  case object MoveRight extends Action
  case object Suck      extends Action

  def showAction(action: Action): String = action match {
    case MoveLeft  => "Left"
    case MoveRight => "Right"
    case Suck      => "Suck"
  }

  val allActions = List(MoveLeft, Suck, MoveRight)

  sealed trait Location
  case object LocationA extends Location
  case object LocationB extends Location

  sealed trait Status
  case object Dirty extends Status
  case object Clean extends Status

  def statusShow(s: Status): String = s match {
    case Dirty => "*"
    case Clean => " "
  }

  final case class VacuumWorldState(vacuumLocation: Location, a: Status, b: Status)

  def stateShow(s: VacuumWorldState): String = s.vacuumLocation match {
    case LocationA => s"[${statusShow(s.a)}_/][${statusShow(s.b)} ]"
    case LocationB => s"[${statusShow(s.a)} ][${statusShow(s.b)}_/]"
  }
}

/**
  * @author Shawn Garner
  */
class AndOrGraphSearchSpec extends Specification {
  import AndOrGraphSearchSpec._

  "AndOrGraphSearch" should {
    "handle state State 1 [*_/][* ]" in new context {
      val cp = andOrGraphSearch(problem)
      val result: MatchResult[Any] = cp match {
        case cp: ConditionalPlan =>
          ConditionalPlan
            .show[VacuumWorldState, Action](cp, stateShow, showAction) must_== "[Suck, if State = [ _/][*  ] then [Right, Suck] else []]"
        case f => ko(f.toString)
      }
      /*
      [error]    '[Suck, if State = [ _/][* ] then [Right, Suck, if State = [  ][ _/] then [] else []] else []]'
[error]
[error]     is not equal to
[error]
[error]    '[Suck, if State = [ _/][*  ] then [Right, Suck] else []]'
       */
      result
    }
  }

  trait context extends Scope with AndOrGraphSearch[Action, VacuumWorldState] {
    val problem = new NondeterministicProblem[Action, VacuumWorldState] {
      override def initialState(): VacuumWorldState =
        VacuumWorldState(
          LocationA,
          Dirty,
          Dirty
        )

      override def actions(s: VacuumWorldState): List[Action] = allActions

      override def results(s: VacuumWorldState, a: Action): List[VacuumWorldState] = (s, a) match {
        case (_, MoveRight) => List(s.copy(vacuumLocation = LocationB))
        case (_, MoveLeft)  => List(s.copy(vacuumLocation = LocationA))
        case (VacuumWorldState(LocationA, Clean, _), Suck) =>
          List(s, s.copy(a = Dirty)) // if current location is clean suck can sometimes deposit dirt
        case (VacuumWorldState(LocationB, _, Clean), Suck) =>
          List(s, s.copy(b = Dirty)) // if current location is clean suck can sometimes deposit dirt
        case (VacuumWorldState(LocationA, Dirty, _), Suck) =>
          List(s.copy(a = Clean), s.copy(a = Clean, b = Clean)) // if current location is dirty sometimes also cleans up adjacent location
        case (VacuumWorldState(LocationB, _, Dirty), Suck) =>
          List(s.copy(b = Clean), s.copy(a = Clean, b = Clean)) // if current location is dirty sometimes also cleans up adjacent location
      }

      override def isGoalState(s: VacuumWorldState): Boolean = s match {
        case VacuumWorldState(_, Clean, Clean) => true
        case _                                 => false
      }

      override def stepCost(s: VacuumWorldState, a: Action, childPrime: VacuumWorldState): Double = ??? // Not used
    }
  }

}
