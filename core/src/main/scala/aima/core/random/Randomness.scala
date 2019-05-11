package aima.core.random

import scala.util.Random

/**
  * @author Shawn Garner
  */
trait Randomness {
  def rand: Random
}

trait DefaultRandomness extends Randomness {
  val rand = new Random()
}

trait EnumerationRandomness extends Randomness { self: Enumeration =>

  def randomValue: self.Value = {
    val selection = rand.nextInt(self.maxId)
    apply(selection)
  }
}

trait SetRandomness[T] extends Randomness {
  def valueSet: Set[T]

  def randomValue: T = {
    val selection = rand.nextInt(valueSet.size)
    val valueList = valueSet.toList
    valueList(selection)
  }
}
