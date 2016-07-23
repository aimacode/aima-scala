package aima.core.environment.vacuum

import aima.core.agent.{Environment, Sensor, Actuator, AgentProgram}
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
    val agentProgram = new AgentProgram {
      lazy val agent = new ReflexVacuumAgent
      lazy val actuators = Seq[Actuator](new SuckerActuator(agent), new MoveActuator(agent))
      lazy val sensors = Seq[Sensor](new DirtSensor(agent), new AgentLocationSensor(agent))
    }

    @tailrec def eventuallyClean(currentEnv: Environment): Boolean = {
      currentEnv match {
        case ve: VacuumEnvironment if ve.isClean() => true
        case _ => eventuallyClean(agentProgram.run(currentEnv))
      }
    }

    eventuallyClean(env.addAgent(agentProgram.agent)) must beTrue
  }


}
