package aima.core.environment.vacuum

import aima.core.agent._

import scala.util.Random
import VacuumEnvironment._

/**
  * @author Shawn Garner
  */
case class VacuumEnvironment(
                              statusLocations: Vector[Percept] = defaultStatusLocations,
                              agentLocations: Vector[Option[Agent]] = defaultAgentLocations) extends Environment { self =>

  def addAgent(agent: Agent): Environment = {
    if(agentLocations.count(_.isDefined) == 0)
      VacuumEnvironment(statusLocations, randomAgentPlacement(agentLocations, agent))
    else
      self
  }

  def removeAgent(agent: Agent): Environment = {
    VacuumEnvironment(statusLocations, removeAgentLocation(agentLocations, agent))

  }

  def actuate(actuator: Actuator, action: Action): Environment = (action, actuator) match {
    case (NoAction, _) => self
    case (SuckerActions.Suck, sa: SuckerActuator) => ???
    case (MoveActions.Left, ma: MoveActuator) => ???
    case (MoveActions.Right, ma: MoveActuator) => ???
  }
  def perceive(sensor: Sensor): Percept = sensor match {
    case ls: AgentLocationSensor =>
      val index = agentLocations.indexOf(Some(ls.agent))
      if(index == 0)
        LocationPercepts.A
      else if (index == 1)
        LocationPercepts.B
      else
        NoPercept

    case ds: DirtSensor =>
      val index = agentLocations.indexOf(Some(ds.agent))
      if(index == 0) {
        statusLocations(index)
      } else if (index == 1)
        statusLocations(index)
      else
        NoPercept

  }
}

object LocationPercepts extends Enumeration {
  val A = new Val(0, "A") with Percept
  val B = new Val(1, "B") with Percept
}

object DirtStatus extends Enumeration {
  val Clean = new Val(0, "Clean") with Percept
  val Dirty = new Val(1, "Dirty") with Percept
}

object MoveActions extends Enumeration {
  val Left = new Val(0, "Left") with Action
  val Right = new Val(1, "Right") with Action
}

object SuckerActions extends Enumeration {
  val Suck = new Val(0, "Suck") with Action
}

class SuckerActuator(val agent: Agent) extends Actuator { self =>
  lazy val rand = new Random()
  def act(action: Action, environment: Environment): Environment = {
    if(rand.nextBoolean())
      environment
    else
      environment.actuate(self, action)
  }
}
class MoveActuator(val agent: Agent) extends Actuator { self =>
  lazy val rand = new Random()
  def act(action: Action, environment: Environment): Environment = {
    if(rand.nextBoolean())
      environment
    else
      environment.actuate(self, action)
  }
}


object VacuumEnvironment {
  lazy val statusRand = new Random()

  def defaultStatusLocations: Vector[Percept] = Vector.fill(2)(randomStatus())
  def defaultAgentLocations: Vector[Option[Agent]] = Vector.fill(2)(None)

  lazy val agentRand = new Random()
  def randomAgentPlacement(agentLocations: Vector[Option[Agent]], agent: Agent): Vector[Option[Agent]] = {
    val index = if(agentRand.nextBoolean()) 1 else 0
    agentLocations.updated(index, Some(agent))
  }

  def removeAgentLocation(agentLocations: Vector[Option[Agent]], agent: Agent): Vector[Option[Agent]] = {
    agentLocations.map {
      case Some(a) if a == agent => None
      case x => x
    }
  }

  def randomStatus(): Percept = {
    if(statusRand.nextBoolean()) DirtStatus.Dirty else DirtStatus.Clean
  }


}

class AgentLocationSensor(val agent: Agent) extends Sensor { self =>
  lazy val rand = new Random()
  def perceive(environment: Environment): Percept = {
    if(rand.nextBoolean()) NoPercept else environment.perceive(self)
  }
}

class DirtSensor(val agent: Agent) extends Sensor { self =>
  lazy val rand = new Random()
  def perceive(environment: Environment): Percept = {
    if(rand.nextBoolean()) NoPercept else environment.perceive(self)
  }
}
