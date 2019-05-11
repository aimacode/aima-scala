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
    val agentProgram = new AgentProgram[VacuumAction, VacuumPercept] {
      val agent     = new SimpleReflexVacuumAgent
      val actuators = List[Actuator[VacuumAction, VacuumPercept]](new SuckerActuator(agent), new MoveActuator(agent))
      lazy val sensors = List[Sensor[VacuumAction, VacuumPercept]](
        new DirtSensor(agent, NoPercept),
        new AgentLocationSensor(agent, NoPercept)
      )
    }

    @tailrec def eventuallyClean(currentEnv: Environment[VacuumAction, VacuumPercept]): Boolean = {
      currentEnv match {
        case ve: VacuumEnvironment if ve.isClean() => true
        case _                                     => eventuallyClean(agentProgram.run(currentEnv))
      }
    }

    eventuallyClean(env.addAgent(agentProgram.agent)) must beTrue
  }

}
