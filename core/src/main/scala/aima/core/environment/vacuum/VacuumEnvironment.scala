package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.environment.vacuum.DirtStatusPercepts.{Dirty, Clean}
import aima.core.environment.vacuum.SuckerActions.Suck
import aima.core.util.{DefaultRandomness, Randomness}

/**
  * @author Shawn Garner
  */
case class VacuumEnvironment(map: VacuumMap = VacuumMap()) extends Environment { self =>

  def addAgent(agent: Agent): Environment = {
    VacuumEnvironment(map.addAgent(agent))
  }

  def removeAgent(agent: Agent): Environment = {
    VacuumEnvironment(map.removeAgent(agent))
  }

  def actuate(actuator: Actuator, action: Action): Environment =
    (action, actuator) match {
      case (Suck, sa: SuckerActuator) =>
        VacuumEnvironment(map.updateStatus(sa.agent, Clean))
      case (moveAction: MoveActions.Value, actuator: MoveActuator) =>
        VacuumEnvironment(map.moveAgent(actuator.agent, moveAction))
      case (NoAction, _) => self
    }

  def perceive(sensor: Sensor): Percept = sensor match {
    case ls: AgentLocationSensor =>
      map.getAgentLocation(ls.agent).getOrElse(NoPercept)

    case ds: DirtSensor =>
      map.getDirtStatus(ds.agent).getOrElse(NoPercept)
  }
}

case class VacuumMapNode(dirtStatus: DirtStatusPercepts.Value = DirtStatusPercepts.randomValue, maybeAgent: Option[Agent] = None)

case class VacuumMap(nodes : Vector[VacuumMapNode] = Vector.fill(2)(VacuumMapNode())) extends DefaultRandomness { self =>
  def getDirtStatus(agent: Agent): Option[Percept] = {
    val maybeNodeWithIndex = nodes.zipWithIndex.find {
      case (VacuumMapNode(_, Some(a)), _) if agent == a => true
      case _ => false
    }
    maybeNodeWithIndex.map { p =>
      p._1.dirtStatus match {//shouldn't need to do this if it was a trait instead Enumeration
        case Clean => Clean
        case Dirty => Dirty
      }
    }
  }

  def getAgentLocation(agent: Agent): Option[Percept] = {
    val maybeNodeWithIndex = nodes.zipWithIndex.find {
      case (VacuumMapNode(_, Some(a)), _) if agent == a => true
      case _ => false
    }
    maybeNodeWithIndex.map(p => indexToLocationPercept(p._2))
  }

  private[this] def indexToLocationPercept(index: Int): Percept = {
    if(index == 0) {
      LocationPercepts.A
    } else {
      LocationPercepts.B
    }
  }
  private[this] def directionToMapIndex(direction: MoveActions.Value):Int = direction match {
    case MoveActions.Left => 0
    case MoveActions.Right => 1
  }

  def moveAgent(agent: Agent, direction: MoveActions.Value): VacuumMap = {
    removeAgent(agent).updateByIndex(directionToMapIndex(direction))(vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent)))
  }

  private[this] def updateByAgent(target: Agent)(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val updatedNodes = nodes.map {
      case n @ VacuumMapNode(_, Some(a)) if a == target => f(n)
      case x => x
    }
    VacuumMap(updatedNodes)
  }

  def updateStatus(agent: Agent, dirtStatusPercepts: DirtStatusPercepts.Value): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(dirtStatus = dirtStatusPercepts))

  def removeAgent(agent: Agent): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = None))

  private def updateByIndex(index: Int)(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val node = nodes.apply(index)
    val updatedNodes = nodes.updated(index, f(node))
    VacuumMap(updatedNodes)
  }

  def addAgent(agent: Agent): VacuumMap = {
    if (nodes.count(_.maybeAgent.isDefined) == 0) {
      val selection = rand.nextInt(nodes.size)
      updateByIndex(selection)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent)))
    } else {
      self
    }
  }
}










