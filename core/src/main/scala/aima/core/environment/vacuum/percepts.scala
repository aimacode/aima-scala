package aima.core.environment.vacuum

import aima.core.random.{DefaultRandomness, SetRandomness}

/**
  * @author Shawn Garner
  */
sealed trait VacuumPercept

sealed trait LocationPercept extends VacuumPercept
case object LocationAPercept extends LocationPercept
case object LocationBPercept extends LocationPercept

object LocationPercept extends SetRandomness[LocationPercept] with DefaultRandomness {
  lazy val valueSet: Set[LocationPercept] =
    Set(LocationAPercept, LocationBPercept)
}

sealed trait DirtPercept extends VacuumPercept
case object CleanPercept extends DirtPercept
case object DirtyPercept extends DirtPercept

object DirtPercept extends SetRandomness[DirtPercept] with DefaultRandomness {
  lazy val valueSet: Set[DirtPercept] = Set(CleanPercept, DirtyPercept)
}

case object NoPercept extends VacuumPercept
