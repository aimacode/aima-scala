package aima.core.environment.map2d

import aima.core.search.CostNode
import Ordering.Double.TotalOrdering

/**
  * @author Shawn Garner
  */
object Map2DFunctionFactory {
  import aima.core.fp.Eqv
  import Eqv.Implicits._

  def actions(map2D: Map2D): InState => List[Map2DAction] =
    inState => map2D.locationsLinkedTo(inState.location).map(Go)

  def stepCost(map2D: Map2D): (InState, Map2DAction, InState) => Double =
    (s, _, sPrime) => map2D.distance(s.location, sPrime.location).map(_.value).getOrElse(Double.MaxValue)

  val result: (InState, Map2DAction) => InState =
    (s, action) =>
      action match {
        case Go(goTo) => InState(goTo)
        case NoOp     => s
      }

  def goalTestPredicate(goalLocations: String*): InState => Boolean =
    inState => goalLocations.exists(location => location === inState.location)

  object StraightLineDistanceHeuristic {

    def apply(map2D: Map2D, goals: String*): CostNode[InState, Map2DAction] => Double =
      node => {

        def h(state: InState): Double = {
          val distances: List[Double] = goals.toList.flatMap { goal =>
            val distance: Option[Double] = for {
              currentPosition <- map2D.position(state.location)
              goalPosition    <- map2D.position(goal)
            } yield distanceOf(currentPosition, goalPosition)

            distance.toList
          }

          distances match {
            case Nil => Double.MaxValue
            case _   => distances.min
          }

        }

        def distanceOf(p1: Point2D, p2: Point2D): Double = {
          math.sqrt(
            (p1.x - p2.x) * (p1.x - p2.x)
              + (p1.y - p2.y) * (p1.y - p2.y)
          )
        }

        h(node.state)

      }

  }

}
