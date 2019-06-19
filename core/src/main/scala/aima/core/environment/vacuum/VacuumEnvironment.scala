package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
case class VacuumEnvironment(map: VacuumMap = VacuumMap()) extends Environment[VacuumAction, VacuumPercept] { self =>

  def addAgent(agent: Agent[VacuumAction, VacuumPercept]): Environment[VacuumAction, VacuumPercept] = {
    VacuumEnvironment(map.addAgent(agent))
  }

  def removeAgent(agent: Agent[VacuumAction, VacuumPercept]): Environment[VacuumAction, VacuumPercept] = {
    VacuumEnvironment(map.removeAgent(agent))
  }

  def actuate(
      actuator: Actuator[VacuumAction, VacuumPercept],
      action: VacuumAction
  ): Environment[VacuumAction, VacuumPercept] =
    (action, actuator) match {
      case (Suck, sa: SuckerActuator[VacuumAction, VacuumPercept]) =>
        VacuumEnvironment(map.updateStatus(sa.agent, CleanPercept))
      case (moveAction: MoveAction, actuator: MoveActuator[VacuumAction, VacuumPercept]) =>
        VacuumEnvironment(map.moveAgent(actuator.agent, moveAction))
      case _ => self
    }

  def perceive(sensor: Sensor[VacuumAction, VacuumPercept]): VacuumPercept = sensor match {
    case ls: AgentLocationSensor[VacuumAction, VacuumPercept] =>
      map.getAgentLocation(ls.agent).getOrElse(NoPercept)

    case ds: DirtSensor[VacuumAction, VacuumPercept] =>
      map.getDirtStatus(ds.agent).getOrElse(NoPercept)
  }

  def isClean(): Boolean = {
    map.isClean()
  }
}

case class VacuumMapNode(
    dirtStatus: DirtPercept = DirtPercept.randomValue,
    maybeAgent: Option[Agent[VacuumAction, VacuumPercept]] = None
)

case class VacuumMap(nodes: Vector[VacuumMapNode] = Vector.fill(2)(VacuumMapNode())) extends DefaultRandomness {
  self =>

  def isClean(): Boolean = {
    nodes.forall { node =>
      node.dirtStatus match {
        case CleanPercept => true
        case _            => false
      }
    }
  }

  def getDirtStatus(agent: Agent[VacuumAction, VacuumPercept]): Option[VacuumPercept] =
    nodes.collectFirst {
      case VacuumMapNode(dirtStatus, Some(a)) if agent == a => dirtStatus
    }

  def getAgentLocation(agent: Agent[VacuumAction, VacuumPercept]): Option[VacuumPercept] = {
    val maybeIndex = nodes.zipWithIndex.collectFirst {
      case (VacuumMapNode(_, Some(a)), index) if agent == a => index
    }
    maybeIndex.map(index => indexToLocationPercept(index))
  }

  private[this] def indexToLocationPercept(index: Int): VacuumPercept = {
    if (index == 0) {
      LocationAPercept
    } else {
      LocationBPercept
    }
  }
  private[this] def directionToMapIndex(direction: MoveAction): Int =
    direction match {
      case LeftMoveAction  => 0
      case RightMoveAction => 1
    }

  def moveAgent(agent: Agent[VacuumAction, VacuumPercept], direction: MoveAction): VacuumMap = {
    removeAgent(agent).updateByIndex(directionToMapIndex(direction))(
      vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent))
    )
  }

  private[this] def updateByAgent(
      target: Agent[VacuumAction, VacuumPercept]
  )(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val updatedNodes = nodes.map {
      case n @ VacuumMapNode(_, Some(a)) if a == target => f(n)
      case x                                            => x
    }
    VacuumMap(updatedNodes)
  }

  def updateStatus(agent: Agent[VacuumAction, VacuumPercept], dirtStatusPercepts: DirtPercept): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(dirtStatus = dirtStatusPercepts))

  def removeAgent(agent: Agent[VacuumAction, VacuumPercept]): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = None))

  private def updateByIndex(index: Int)(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val node         = nodes.apply(index)
    val updatedNodes = nodes.updated(index, f(node))
    VacuumMap(updatedNodes)
  }

  def addAgent(agent: Agent[VacuumAction, VacuumPercept]): VacuumMap = {
    if (nodes.count(_.maybeAgent.isDefined) == 0) {
      val selection = rand.nextInt(nodes.size)
      updateByIndex(selection)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent)))
    } else {
      self
    }
  }
}
