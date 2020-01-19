package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.random.DefaultRandomness

/**
  * @author Shawn Garner
  */
case class Vacuum(map: VacuumMap = VacuumMap()) extends Environment[Vacuum, VacuumPercept, VacuumAction] {

  def addAgent(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): Vacuum = {
    Vacuum(map.addAgent(agent))
  }

  def removeAgent(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): Vacuum = {
    Vacuum(map.removeAgent(agent))
  }

  def isClean(): Boolean = {
    map.isClean()
  }
}

case class VacuumMapNode(
    dirtStatus: DirtPercept = DirtPercept.randomValue,
    maybeAgent: Option[Agent[Vacuum, VacuumPercept, VacuumAction]] = None
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

  def getAgentNode(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): Option[VacuumMapNode] = nodes.collectFirst {
    case VacuumMapNode(dirtStatus, Some(a)) if agent == a =>
      VacuumMapNode(dirtStatus, Some(a))
  }
  def getDirtStatus(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): Option[VacuumPercept] =
    getAgentNode(agent).map(_.dirtStatus)

  def getAgentLocation(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): Option[VacuumPercept] = {
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

  def moveAgent(agent: Agent[Vacuum, VacuumPercept, VacuumAction], direction: MoveAction): VacuumMap = {
    removeAgent(agent).updateByIndex(directionToMapIndex(direction))(vacuumMapNode =>
      vacuumMapNode.copy(maybeAgent = Some(agent))
    )
  }

  private[this] def updateByAgent(
      target: Agent[Vacuum, VacuumPercept, VacuumAction]
  )(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val updatedNodes = nodes.map {
      case n @ VacuumMapNode(_, Some(a)) if a == target => f(n)
      case x                                            => x
    }
    VacuumMap(updatedNodes)
  }

  def updateStatus(agent: Agent[Vacuum, VacuumPercept, VacuumAction], dirtStatusPercepts: DirtPercept): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(dirtStatus = dirtStatusPercepts))

  def removeAgent(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): VacuumMap =
    updateByAgent(agent)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = None))

  private def updateByIndex(index: Int)(f: VacuumMapNode => VacuumMapNode): VacuumMap = {
    val node         = nodes.apply(index)
    val updatedNodes = nodes.updated(index, f(node))
    VacuumMap(updatedNodes)
  }

  def addAgent(agent: Agent[Vacuum, VacuumPercept, VacuumAction]): VacuumMap = {
    if (nodes.count(_.maybeAgent.isDefined) == 0) {
      val selection = rand.nextInt(nodes.size)
      updateByIndex(selection)(vacuumMapNode => vacuumMapNode.copy(maybeAgent = Some(agent)))
    } else {
      self
    }
  }
}
