package aima.core.environment.vacuum

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

import scala.util.Random

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgentSpec extends Specification {

  "first level dirty sucks" in new context {
    invokeAgent(List(NoPercept, DirtyPercept)) must_== List(NoAction, Suck)
  }

  "first level A and Clean moves Right" in new context {
    invokeAgent(List(LocationAPercept, CleanPercept)) must_== List(NoAction, RightMoveAction)
  }

  "first level B and Clean moves Right" in new context {
    invokeAgent(List(LocationBPercept, CleanPercept)) must_== List(NoAction, LeftMoveAction)
  }

  "second level dirty sucks" in new context {
    val givenPercepts   = List.fill(2)(NoPercept) ++ List(NoPercept, DirtyPercept)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, Suck)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "second level A and Clean moves Right" in new context {
    val givenPercepts   = List.fill(2)(NoPercept) ++ List(LocationAPercept, CleanPercept)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, RightMoveAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "second level B and Clean moves Right" in new context {
    val givenPercepts   = List.fill(2)(NoPercept) ++ List(LocationBPercept, CleanPercept)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, LeftMoveAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level dirty sucks" in new context {
    val givenPercepts   = List.fill(6)(NoPercept) ++ List(NoPercept, DirtyPercept)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, Suck)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level A and Clean moves Right" in new context {
    val givenPercepts   = List.fill(6)(NoPercept) ++ List(LocationAPercept, CleanPercept)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, RightMoveAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level B and Clean moves Right" in new context {
    val givenPercepts   = List.fill(6)(NoPercept) ++ List(LocationBPercept, CleanPercept)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, LeftMoveAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "twenty dirty percepts is undefined because out of table definition range" in new context {
    val givenPercepts   = List.fill(20)(NoPercept)
    val expectedActions = List.fill(20)(NoAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "table driven agent must persist all percepts" in new context {
    val rnd = new Random()
    val randomPerceptStream: LazyList[VacuumPercept] = LazyList.continually {
      val selector = rnd.nextInt(3)
      if (selector == 0)
        LocationPercept.randomValue
      else if (selector == 1)
        DirtPercept.randomValue
      else
        NoPercept
    }

    val givenPercepts = randomPerceptStream.take(100).toList

    invokeAgent(givenPercepts)

    agent.percepts.toList must_== givenPercepts
  }

  trait context extends Scope {
    val agent = new TableDrivenVacuumAgentProgram
    def invokeAgent(percepts: List[VacuumPercept]): List[VacuumAction] = {
      percepts.map(agent.agentFunction)
    }
  }
}
