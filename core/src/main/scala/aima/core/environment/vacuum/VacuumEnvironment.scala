package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.util.DefaultRandomness

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
        VacuumEnvironment(map.updateStatus(sa.agent, CleanPercept))
      case (moveAction: MoveAction, actuator: MoveActuator) =>
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

case class VacuumMapNode(dirtStatus: DirtPercept = DirtPercept.randomValue, maybeAgent: Option[Agent] = None)

case class VacuumMap(nodes : Vector[VacuumMapNode] = Vector.fill(2)(VacuumMapNode())) extends DefaultRandomness { self =>
  def getDirtStatus(agent: Agent): Option[Percept] = {
    val maybeNodeWithIndex = nodes.zipWithIndex.find {
      case (VacuumMapNode(_, Some(a)), _) if agent == a => true
      case _ => false
    }
    maybeNodeWithIndex.map ( p => p._1.dirtStatus )
  }

  def getAgentLocation(agent: Agent): Option[Percept] = {
    val maybeNodeWithIndex = nodes.zipWithIndex.find {
      case (VacuumMapNode(_, Some(a)), _) if agent == a => true
      case _ => false
    }
    maybeNodeWithIndex.map(p => indexToLocationPercept(p._2))
  }

  private[this] def indexToLocationPercept(index: Int): Percept = {
    if(index == 0) { LocationAPercept} else { LocationBPercept }
  }
  private[this] def directionToMapIndex(direction: MoveAction):Int = direction match {
    case LeftMoveAction => 0
    case RightMoveAction => 1
  }

  def moveAgent(agent: Agent, direction: MoveAction): VacuumMap = {
    removeAgent(agent).updateByIndex(directionToMapIndex(direction))(vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent)))
  }

  private[this] def updateByAgent(target: Agent)(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val updatedNodes = nodes.map {
      case n @ VacuumMapNode(_, Some(a)) if a == target => f(n)
      case x => x
    }
    VacuumMap(updatedNodes)
  }

  def updateStatus(agent: Agent, dirtStatusPercepts: DirtPercept): VacuumMap =
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










