package aima.core.search.contingency

import aima.core.fp.Show
import aima.core.search.contingency.AndOrGraphSearchSpec.{Action, VacuumWorldState}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.reflect.ClassTag

object AndOrGraphSearchSpec {
  sealed trait Action
  case object MoveLeft  extends Action
  case object MoveRight extends Action
  case object Suck      extends Action

  object Action {
    object Implicits {
      implicit val showAction: Show[Action] = new Show[Action] {
        override def show(action: Action): String = action match {
          case MoveLeft  => "Left"
          case MoveRight => "Right"
          case Suck      => "Suck"
        }
      }
    }
  }

  val allActions = List(MoveLeft, Suck, MoveRight)

  sealed trait Location
  case object LocationA extends Location
  case object LocationB extends Location

  sealed trait Status
  case object Dirty extends Status
  case object Clean extends Status

  object Status {
    object Implicits {
      implicit val statusShow: Show[Status] = new Show[Status] {
        override def show(s: Status): String = s match {
          case Dirty => "*"
          case Clean => " "
        }
      }
    }
  }

  final case class VacuumWorldState(vacuumLocation: Location, a: Status, b: Status)

  object VacuumWorldState {
    object Implicits {
      implicit val arbVacuumWorldState = Arbitrary {
        for {
          location <- Gen.oneOf(LocationA, LocationB)
          aStatus  <- Gen.oneOf(Dirty, Clean)
          bStatus  <- Gen.oneOf(Dirty, Clean)
        } yield VacuumWorldState(location, aStatus, bStatus)
      }

      import Show.Implicits._
      implicit def vacuumWorldStateShow(implicit statusShow: Show[Status]): Show[VacuumWorldState] =
        new Show[VacuumWorldState] {
          override def show(s: VacuumWorldState): String =
            s.vacuumLocation match {
              case LocationA => s"[${s.a.show}_/][${s.b.show}  ]"
              case LocationB => s"[${s.a.show}  ][${s.b.show}_/]"
            }
        }
    }
  }

  def problem(initial: VacuumWorldState) = new NondeterministicProblem[Action, VacuumWorldState] {
    override def initialState(): VacuumWorldState = initial

    override def actions(s: VacuumWorldState): List[Action] = allActions

    override def results(s: VacuumWorldState, a: Action): List[VacuumWorldState] = (s, a) match {
      case (_, MoveRight) => List(s.copy(vacuumLocation = LocationB))
      case (_, MoveLeft)  => List(s.copy(vacuumLocation = LocationA))

      case (VacuumWorldState(LocationA, Clean, _), Suck) =>
        List(s, s.copy(a = Dirty)) // if current location is clean suck can sometimes deposit dirt
      case (VacuumWorldState(LocationB, _, Clean), Suck) =>
        List(s, s.copy(b = Dirty)) // if current location is clean suck can sometimes deposit dirt

      case (VacuumWorldState(LocationA, Dirty, Dirty), Suck) =>
        List(s.copy(a = Clean), s.copy(a = Clean, b = Clean)) // if current location is dirty sometimes also cleans up adjacent location
      case (VacuumWorldState(LocationB, Dirty, Dirty), Suck) =>
        List(s.copy(b = Clean), s.copy(a = Clean, b = Clean)) // if current location is dirty sometimes also cleans up adjacent location

      case (VacuumWorldState(LocationA, Dirty, _), Suck) =>
        List(s.copy(a = Clean))
      case (VacuumWorldState(LocationB, _, Dirty), Suck) =>
        List(s.copy(b = Clean))

    }

    override def isGoalState(s: VacuumWorldState): Boolean = s match {
      case VacuumWorldState(_, Clean, Clean) => true
      case _                                 => false
    }

    override def stepCost(s: VacuumWorldState, a: Action, childPrime: VacuumWorldState): Double =
      ??? // Not used
  }

  import scala.reflect.classTag
  val aCTag: ClassTag[Action]           = classTag[Action]
  val sCTag: ClassTag[VacuumWorldState] = classTag[VacuumWorldState]
}

/**
  * @author Shawn Garner
  */
class AndOrGraphSearchSpec extends Specification with AndOrGraphSearch[Action, VacuumWorldState] with ScalaCheck {
  import AndOrGraphSearchSpec._

  import Action.Implicits.showAction
  import Status.Implicits.statusShow
  import VacuumWorldState.Implicits.vacuumWorldStateShow
  implicit def sCP: Show[ConditionalPlan] = ConditionalPlan.Implicits.showConditionalPlan[VacuumWorldState, Action]
  import Show.Implicits._

  "AndOrGraphSearch" should {
    "handle State 1 [*_/][* ]" in {
      val initial = VacuumWorldState(LocationA, Dirty, Dirty)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Suck, if State = [ _/][*  ] then [Right, Suck] else []]"
        case f => ko(f.toString)
      }
    }

    "handle State 2 [* ][*_/]" in {
      val initial = VacuumWorldState(LocationB, Dirty, Dirty)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Left, Suck, if State = [ _/][*  ] then [Right, Suck] else []]"
        case f => ko(f.toString)
      }
    }

    "handle State 3 [*_/][ ]" in {
      val initial = VacuumWorldState(LocationA, Dirty, Clean)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Suck]"
        case f => ko(f.toString)
      }
    }

    "handle State 4 [* ][ _/]" in {
      val initial = VacuumWorldState(LocationB, Dirty, Clean)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Left, Suck]"
        case f => ko(f.toString)
      }
    }

    "handle State 5 [ _/][* ]" in {
      val initial = VacuumWorldState(LocationA, Clean, Dirty)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Right, Suck]"
        case f => ko(f.toString)
      }
    }

    "handle State 6 [ ][*_/]" in {
      val initial = VacuumWorldState(LocationB, Clean, Dirty)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[Suck]"
        case f => ko(f.toString)
      }
    }

    "handle State 7 [ _/][ ]" in {
      val initial = VacuumWorldState(LocationA, Clean, Clean)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[]"
        case f => ko(f.toString)
      }
    }

    "handle  State 8 [ ][ _/]" in {
      val initial = VacuumWorldState(LocationB, Clean, Clean)
      val prob    = problem(initial)
      val cp      = andOrGraphSearch(prob)
      cp match {
        case cp: ConditionalPlan =>
          cp.show must_== "[]"
        case f => ko(f.toString)
      }
    }

    import VacuumWorldState.Implicits.arbVacuumWorldState
    "find solutions for all initial states" >> prop { initial: VacuumWorldState =>
      val prob = problem(initial)
      val cp   = andOrGraphSearch(prob)
      cp match {
        case _: ConditionalPlan => ok
        case f                  => ko(f.toString)
      }
    }
  }
  override implicit val aCT: ClassTag[Action]           = aCTag
  override implicit val sCT: ClassTag[VacuumWorldState] = sCTag
}
