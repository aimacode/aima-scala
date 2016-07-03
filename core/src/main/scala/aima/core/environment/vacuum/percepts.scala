package aima.core.environment.vacuum

import aima.core.agent.Percept
import aima.core.util.{EnumerationRandomness, DefaultRandomness}

/**
  * @author Shawn Garner
  */
object LocationPercepts extends Enumeration with EnumerationRandomness with DefaultRandomness {
  val A = new Val(nextId, "A") with Percept
  val B = new Val(nextId, "B") with Percept
}

object DirtStatusPercepts extends Enumeration with EnumerationRandomness with DefaultRandomness {
  val Clean = new Val(nextId, "Clean") with Percept
  val Dirty = new Val(nextId, "Dirty") with Percept
}
