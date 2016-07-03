package aima.core.environment.vacuum

import aima.core.agent.Action
import aima.core.util.{DefaultRandomness, EnumerationRandomness}

/**
  * @author Shawn Garner
  */
object MoveActions extends Enumeration with EnumerationRandomness with DefaultRandomness {
  val Left = new Val(nextId, "Left") with Action
  val Right = new Val(nextId, "Right") with Action
}

object SuckerActions extends Enumeration with EnumerationRandomness with DefaultRandomness{
  val Suck = new Val(nextId, "Suck") with Action
}
