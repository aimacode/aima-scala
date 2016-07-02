package aima.core.environment.vacuum

import aima.core.agent._

import scala.util.Random
import VacuumEnvironment._

/**
  * @author Shawn Garner
  */
case class VacuumEnvironment(
                              statusLocations: StatusLocations = defaultStatusLocations,
                              agentLocations: AgentLocations = defaultAgentLocations) extends Environment { self =>

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
    case (SuckerActions.Suck, sa: SuckerActuator) => VacuumEnvironment(updateStatus(agentLocations, actuator.agent, statusLocations, SuckerActions.Suck))
    case (MoveActions.Left, ma: MoveActuator) => VacuumEnvironment(statusLocations, moveAgent(agentLocations, actuator.agent, MoveActions.Left))
    case (MoveActions.Right, ma: MoveActuator) => VacuumEnvironment(statusLocations, moveAgent(agentLocations, actuator.agent, MoveActions.Right))
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
  val A = new Val(nextId, "A") with Percept
  val B = new Val(nextId, "B") with Percept
}

object DirtStatusPercepts extends Enumeration {
  val Clean = new Val(nextId, "Clean") with Percept
  val Dirty = new Val(nextId, "Dirty") with Percept
}

object MoveActions extends Enumeration {
  val Left = new Val(nextId, "Left") with Action
  val Right = new Val(nextId, "Right") with Action
}

object SuckerActions extends Enumeration {
  val Suck = new Val(nextId, "Suck") with Action
}

class SuckerActuator(val agent: Agent) extends Actuator { self =>
  lazy val rand = new Random()//actuators do not always work
  def act(action: Action, environment: Environment): Environment = {
    if(rand.nextBoolean())
      environment
    else
      environment.actuate(self, action)
  }
}
class MoveActuator(val agent: Agent) extends Actuator { self =>
  lazy val rand = new Random()//actuators do not always work
  def act(action: Action, environment: Environment): Environment = {
    if(rand.nextBoolean())
      environment
    else
      environment.actuate(self, action)
  }
}


object VacuumEnvironment {
  type AgentLocations = Vector[Option[Agent]]
  type StatusLocations = Vector[Percept]

  lazy val statusRand = new Random()
  def defaultStatusLocations: StatusLocations = Vector.fill(2)(randomStatus())
  def defaultAgentLocations: AgentLocations = Vector.fill(2)(None)

  lazy val agentRand = new Random()
  def randomAgentPlacement(agentLocations: AgentLocations, agent: Agent): AgentLocations = {
    val index = if(agentRand.nextBoolean()) 1 else 0
    agentLocations.updated(index, Some(agent))
  }

  def removeAgentLocation(agentLocations: AgentLocations, agent: Agent): AgentLocations = {
    agentLocations.map {
      case Some(a) if a == agent => None
      case x => x
    }
  }

  def moveAgent(agentLocations: AgentLocations, agent: Agent, moveAction: MoveActions.Value): AgentLocations = {
    val updatedLocations = removeAgentLocation(agentLocations, agent)
    val index = moveAction match {
      case MoveActions.Left => 0
      case MoveActions.Right => 1
    }
    updatedLocations.updated(index, Some(agent))
  }

  def updateStatus(agentLocations: AgentLocations, agent: Agent, statusLocations: StatusLocations, suckAction: SuckerActions.Value): StatusLocations = {
    val index = agentLocations.indexOf(Some(agent))
    val newStatus = suckAction match {
      case SuckerActions.Suck => DirtStatusPercepts.Clean
    }
    statusLocations.updated(index, newStatus)
  }

  def randomStatus(): Percept = {
    if(statusRand.nextBoolean()) DirtStatusPercepts.Dirty else DirtStatusPercepts.Clean
  }


}

class AgentLocationSensor(val agent: Agent) extends Sensor { self =>
  lazy val rand = new Random()//sensors do not always work
  def perceive(environment: Environment): Percept = {
    if(rand.nextBoolean()) NoPercept else environment.perceive(self)
  }
}

class DirtSensor(val agent: Agent) extends Sensor { self =>
  lazy val rand = new Random()//sensors do not always work
  def perceive(environment: Environment): Percept = {
    if(rand.nextBoolean()) NoPercept else environment.perceive(self)
  }
}
