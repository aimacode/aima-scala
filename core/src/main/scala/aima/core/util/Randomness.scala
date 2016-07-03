package aima.core.util

import scala.util.Random

/**
  * @author Shawn Garner
  */
trait Randomness {
  def rand: Random
}

trait DefaultRandomness extends Randomness {
  lazy val rand = new Random()
}

trait EnumerationRandomness extends Randomness {
  self : Enumeration =>

  def randomValue: self.Value = {
    val selection = rand.nextInt(self.maxId)
    apply(selection)
  }
}