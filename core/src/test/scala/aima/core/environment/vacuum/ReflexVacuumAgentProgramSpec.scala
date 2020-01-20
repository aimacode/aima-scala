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

  implicit val arbVacuumEnvironment = Arbitrary(VacuumEnvironment())

  "should eventually clean environment" in prop { env: VacuumEnvironment =>
    val agent = new Agent[VacuumEnvironment, VacuumPercept, VacuumAction] {
      val agentProgram = new SimpleReflexVacuumAgentProgram
      val actuators    = List[Actuator[VacuumEnvironment, VacuumAction]](new SuckerActuator(this), new MoveActuator(this))
      lazy val sensors = List[Sensor[VacuumEnvironment, VacuumPercept]](
        new DirtSensor(this, NoPercept),
        new AgentLocationSensor(this, NoPercept)
      )
    }

    @tailrec def eventuallyClean(currentEnv: VacuumEnvironment): Boolean = {
      currentEnv match {
        case ve: VacuumEnvironment if ve.isClean() => true
        case _                          => eventuallyClean(agent.run(currentEnv)._1)
      }
    }

    eventuallyClean(env.addAgent(agent)) must beTrue
  }

}
