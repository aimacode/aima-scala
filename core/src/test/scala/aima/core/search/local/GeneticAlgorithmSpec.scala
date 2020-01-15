package aima.core.search.local

import aima.core.search.local.set.NonEmptySet
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.concurrent.duration._

/**
  * @author Shawn Garner
  */
class GeneticAlgorithmSpec
    extends Specification
    with GeneticAlgorithm[String]
    with ScalaCheck {

  import GeneticAlgorithm.StringIndividual._

  implicit val arbString: Arbitrary[String] = Arbitrary(
    Gen.listOfN(20, Gen.alphaChar).map(_.mkString)
  )

  implicit def arbSet[A: Arbitrary]: Arbitrary[NonEmptySet[A]] = Arbitrary {
    Gen.nonEmptyListOf(Arbitrary.arbitrary[A]).map(_.toSet).flatMap { set =>
      NonEmptySet.apply(set) match {
        case Right(nes) => Gen.const(nes)
        case Left(_)    => Gen.fail[NonEmptySet[A]]
      }
    }
  }

  "must find strings of at least 50% vowels" >> prop {
    population: NonEmptySet[String] =>
      def vowelFitness(individual: String): Fitness = {
        val u = individual.foldLeft(0.0d) {
          case (
              acc,
              'a' | 'e' | 'i' | 'o' | 'u' | 'y' | 'A' | 'E' | 'I' | 'O' | 'U' |
              'Y'
              ) =>
            acc + 1.0d
          case (acc, _) => acc
        }
        Fitness(u)
      }

      def fitEnough(individual: String): Boolean = {
        val length = individual.length
        if (length != 0) {
          vowelFitness(individual).value / length >= 0.50d
        } else {
          false
        }
      }

      val fitIndividual =
        geneticAlgorithm(population, vowelFitness)(
          fitEnough,
          2.minutes,
          reproduce2,
          Probability(0.05),
          mutate
        )
      val fitness = vowelFitness(fitIndividual).value / fitIndividual.length
      fitness aka fitIndividual must be greaterThanOrEqualTo 0.50d
  }

}
