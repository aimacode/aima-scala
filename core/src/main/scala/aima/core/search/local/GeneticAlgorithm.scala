package aima.core.search.local

import aima.core.search.local.set.NonEmptySet

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

final case class Fitness(value: Double)     extends AnyVal
final case class Probability(value: Double) extends AnyVal // Need to make sure between 0.0 an 1.0

package set {
  final class NonEmptySet[A] private[set] (val value: Set[A])

  object NonEmptySet {
    def apply[A](set: Set[A]): Either[String, NonEmptySet[A]] = {
      if (set.isEmpty) {
        Left("Set can not be empty")
      } else {
        Right(new NonEmptySet[A](set))
      }
    }
  }
}

trait Deadline {
  def isOverDeadline: Boolean
}

object Deadline {
  def start(timeLimit: FiniteDuration) = new Deadline {
    val start = System.currentTimeMillis()
    override def isOverDeadline: Boolean = {
      val current = System.currentTimeMillis()
      (current - start) > timeLimit.toMillis
    }
  }
}

object Util {
  def normalize(probDist: List[Double]): List[Double] = {
    val total = probDist.sum
    if (total != 0.0d) {
      probDist.map(d => d / total)
    } else {
      List.fill(probDist.length)(0.0d)
    }
  }
}

/**
  * function GENETIC-ALGORITHM(population, FITNESS-FN) returns an individual
  *   inputs: population, a set of individuals
  *           FITNESS-FN, a function that measures the fitness of an individual
  *
  *   repeat
  *     new_population &lt;- empty set
  *     repeat
  *       x &lt;- RANDOM-SELECTION(population, FITNESS-FN)
  *       y &lt;- RANDOM-SELECTION(population, FITNESS-FN)
  *       child &lt;- REPRODUCE(x, y)
  *       if (small random probability) then child &lt;- MUTATE(child)
  *       add child to new_population
  *     until SIZE(new_population) = SIZE(population)
  *     population &lt;- new_population
  *   until some individual is fit enough, or enough time has elapsed
  *   return the best individual in population, according to FITNESS-FN
  * --------------------------------------------------------------------------------
  * function REPRODUCE(x, y) returns an individual
  *   inputs: x, y, parent individuals
  *
  *   n &lt;- LENGTH(x); c &lt;- random number from 1 to n
  *   return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n))
  * </pre>
  *
  * Figure ?? A genetic algorithm. The algorithm is the same as the one
  * diagrammed in Figure 4.6, with one variation: in this more popular version,
  * each mating of two parents produces only one offspring, not two.
  *
  * @author Shawn Garner
  */
trait GeneticAlgorithm[Individual] {

  type FitnessFunction      = Individual => Fitness
  type FitEnough            = Individual => Boolean
  type ReproductionFunction = (Individual, Individual, Random) => List[Individual]
  type MutationFunction     = (Individual, Random) => Individual

  def geneticAlgorithm(initialPopulation: NonEmptySet[Individual], fitnessFunction: FitnessFunction)(
      fitEnough: FitEnough,
      timeLimit: FiniteDuration,
      reproduce: ReproductionFunction,
      mutationProbability: Probability,
      mutate: MutationFunction
  ): Individual = {
    val random   = new Random()
    val deadline = Deadline.start(timeLimit)

    @tailrec def recurse(currentPopulation: Set[Individual], newPopulation: Set[Individual]): Individual = {
      val children: List[Individual] = (for {
        x <- randomSelection(currentPopulation, fitnessFunction)(random)
        y <- randomSelection(currentPopulation, fitnessFunction)(random)
      } yield reproduce(x, y, random)).toList.flatten

      val mutated = {
        children.map { c =>
          if (isSmallRandomProbabilityOfMutation(mutationProbability, random)) {
            mutate(c, random)
          } else {
            c
          }
        }
      }

      val updatedNewPop = newPopulation ++ mutated

      if (updatedNewPop.size < currentPopulation.size) {
        recurse(currentPopulation, updatedNewPop)
      } else {
        val selected = selectBestIndividualIfReady(updatedNewPop, fitnessFunction)(fitEnough, deadline, random)
        selected match {
          case Some(ind) => ind
          case None      => recurse(updatedNewPop, Set.empty[Individual])
        }

      }
    }

    recurse(initialPopulation.value, Set.empty[Individual])

  }

  def selectBestIndividualIfReady(population: Set[Individual], fitnessFunction: FitnessFunction)(
      fitEnough: FitEnough,
      deadline: Deadline,
      random: Random
  ): Option[Individual] = {
    if (deadline.isOverDeadline || population.exists(fitEnough)) {

      @tailrec def findBest(pop: List[Individual], best: Option[(Individual, Fitness)]): Option[Individual] =
        (pop, best) match {
          case (Nil, b) => b.map(_._1)
          case (current :: rest, None) =>
            val currentValue = fitnessFunction(current)
            findBest(rest, Some((current, currentValue)))
          case (current :: rest, Some(b)) =>
            val currentValue = fitnessFunction(current)
            if (currentValue.value > b._2.value) {
              findBest(rest, Some((current, currentValue)))
            } else if (currentValue.value == b._2.value) {
              if (random.nextBoolean()) {
                findBest(rest, Some((current, currentValue)))
              } else {
                findBest(rest, best)
              }
            } else {
              findBest(rest, best)
            }
        }

      findBest(population.toList, None)
    } else {
      None
    }

  }

  def isSmallRandomProbabilityOfMutation(mutationProbability: Probability, random: Random): Boolean =
    random.nextDouble <= mutationProbability.value

  def randomSelection(population: Set[Individual], fitnessFunction: FitnessFunction)(
      random: Random
  ): Option[Individual] = {
    val populationList    = population.toList
    val populationFitness = populationList.map(fitnessFunction)
    val fValues           = Util.normalize(populationFitness.map(_.value))
    val probability       = random.nextDouble()

    val popWithFValues = populationList zip fValues
    @tailrec def selectByProbability(l: List[(Individual, Double)], totalSoFar: Double): Option[Individual] = l match {
      case Nil          => None
      case first :: Nil => Some(first._1) // if we are at end of list or only one element must select it
      case first :: rest =>
        val newTotal = totalSoFar + first._2
        if (probability <= newTotal) { // seems weird
          Some(first._1)
        } else {
          selectByProbability(rest, newTotal)
        }

    }

    selectByProbability(popWithFValues, 0.0d)
  }

}

object GeneticAlgorithm {
  object StringIndividual {

    // function REPRODUCE(x, y) returns an individual
    def reproduce1(x: String, y: String, random: Random): List[String] = {
      // n <- LENGTH(x);
      val n = x.length
      // c <- random number from 1 to n
      val c = random.nextInt(n)
      // return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n))
      List(
        x.substring(0, c) + y.substring(c, n)
      )
    }

    // function REPRODUCE(x, y) returns a pair of individual
    def reproduce2(x: String, y: String, random: Random): List[String] = {
      // n <- LENGTH(x);
      val n = x.length
      // c <- random number from 1 to n
      val c = random.nextInt(n)
      // return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n)) and APPEND(SUBSTRING(y, 1, c), SUBSTRING(x, c+1, n))
      List(
        x.substring(0, c) + y.substring(c, n),
        y.substring(0, c) + x.substring(c, n)
      )
    }

    val alphabet: List[Char] = (('a' to 'z') ++ ('A' to 'Z')).toList

    def mutate(child: String, random: Random): String = {
      val replacement: Char = alphabet(random.nextInt(alphabet.size))
      child.updated(random.nextInt(child.length), replacement)
    }
  }
}
