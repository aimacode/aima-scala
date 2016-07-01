package aima.core.environment.vacuum

import aima.core.agent.{NoAction, NoPercept, Percept, Action}
import aima.core.environment.vacuum.DirtStatusPercepts.{Clean, Dirty}
import aima.core.environment.vacuum.LocationPercepts.{B, A}
import aima.core.environment.vacuum.MoveActions.{Left, Right}
import aima.core.environment.vacuum.SuckerActions.Suck
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgentSpec extends Specification {

  "first level dirty sucks" in new context {
    invokeAgent(List(NoPercept, Dirty)) must_== List(NoAction, Suck)
  }

  "first level A and Clean moves Right" in new context {
    invokeAgent(List(A, Clean)) must_== List(NoAction, Right)
  }

  "first level B and Clean moves Right" in new context {
    invokeAgent(List(B, Clean)) must_== List(NoAction, Left)
  }


  "second level dirty sucks" in new context {
    val givenPercepts = List.fill(2)(NoPercept) ++ List(NoPercept, Dirty)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, Suck)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "second level A and Clean moves Right" in new context {
    val givenPercepts = List.fill(2)(NoPercept) ++ List(A, Clean)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, Right)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "second level B and Clean moves Right" in new context {
    val givenPercepts = List.fill(2)(NoPercept) ++ List(B, Clean)
    val expectedActions = List.fill(2)(NoAction) ++ List(NoAction, Left)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level dirty sucks" in new context {
    val givenPercepts = List.fill(6)(NoPercept) ++ List(NoPercept, Dirty)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, Suck)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level A and Clean moves Right" in new context {
    val givenPercepts = List.fill(6)(NoPercept) ++ List(A, Clean)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, Right)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "fourth level B and Clean moves Right" in new context {
    val givenPercepts = List.fill(6)(NoPercept) ++ List(B, Clean)
    val expectedActions = List.fill(6)(NoAction) ++ List(NoAction, Left)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  "twenty dirty percepts is undefined because out of table definition range" in new context {
    val givenPercepts = List.fill(20)(NoPercept)
    val expectedActions = List.fill(20)(NoAction)
    invokeAgent(givenPercepts) must_== expectedActions
  }

  trait context extends Scope {
    val agent = new TableDrivenVacuumAgent
    def invokeAgent(percepts: List[Percept]): List[Action] = {
      percepts.map(agent.agentFunction)
    }
  }
}
