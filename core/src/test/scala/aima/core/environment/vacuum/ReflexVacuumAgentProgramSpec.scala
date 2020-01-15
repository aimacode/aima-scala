package aima.core.environment.vacuum

import aima.core.agent.{Actuator, Agent, AgentProgram, Environment, Sensor}
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.annotation.tailrec

/**
  * @author Shawn Garner
  */
class ReflexVacuumAgentProgramSpec extends Specification with ScalaCheck {

  implicit val arbVacuumEnvironment = Arbitrary(Vacuum())

  "should eventually clean environment" in prop { env: Vacuum =>
    val agent = new Agent[Vacuum, VacuumPercept, VacuumAction] {
      val agentProgram = new SimpleReflexVacuumAgentProgram
      val actuators = List[Actuator[Vacuum, VacuumAction]](
        new SuckerActuator(this),
        new MoveActuator(this)
      )
      lazy val sensors = List[Sensor[Vacuum, VacuumPercept]](
        new DirtSensor(this, NoPercept),
        new AgentLocationSensor(this, NoPercept)
      )
    }

    @tailrec def eventuallyClean(currentEnv: Vacuum): Boolean = {
      currentEnv match {
        case ve: Vacuum if ve.isClean() => true
        case _                          => eventuallyClean(agent.run(currentEnv))
      }
    }

    eventuallyClean(env.addAgent(agent)) must beTrue
  }

}
