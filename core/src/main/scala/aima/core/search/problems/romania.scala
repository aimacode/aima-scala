package aima.core.search.problems

import aima.core.search.{CostNode, Problem, StateNode}

import scala.reflect.{ClassTag, classTag}

/**
  * @author Shawn Garner
  */
object Romania {
  final case class City(name: String) extends AnyVal
  case class Road(from: City, to: City, cost: Int)

  val Arad          = City("Arad")
  val Zerind        = City("Zerind")
  val Oradea        = City("Oradea")
  val Sibiu         = City("Sibiu")
  val Timisoara     = City("Timisoara")
  val Lugoj         = City("Lugoj")
  val Mehadia       = City("Mehadia")
  val Drobeta       = City("Drobeta")
  val RimnicuVilcea = City("Rimnicu Vilcea")
  val Craiova       = City("Craiova")
  val Fagaras       = City("Fagaras")
  val Pitesti       = City("Pitesti")
  val Bucharest     = City("Bucharest")
  val Giurgiu       = City("Giurgiu")
  val Urziceni      = City("Urziceni")
  val Neamt         = City("Neamt")
  val Iasi          = City("Iasi")
  val Vaslui        = City("Vaslui")
  val Hirsova       = City("Hirsova")
  val Eforie        = City("Eforie")

  val roadsFromCity: Map[City, List[Road]] = List(
    Road(Arad, Zerind, 75),
    Road(Arad, Timisoara, 118),
    Road(Arad, Sibiu, 140), //Arad Edges
    Road(Zerind, Arad, 75),
    Road(Zerind, Oradea, 71), //Zerind Edges
    Road(Oradea, Zerind, 71),
    Road(Oradea, Sibiu, 151), //Oradea Edges
    Road(Sibiu, Arad, 140),
    Road(Sibiu, Oradea, 151),
    Road(Sibiu, Fagaras, 99),
    Road(Sibiu, RimnicuVilcea, 80), //Sibiu Edges
    Road(Timisoara, Arad, 118),
    Road(Timisoara, Lugoj, 111), //Timisoara Edges
    Road(Lugoj, Timisoara, 111),
    Road(Lugoj, Mehadia, 70), //Lugoj Edges
    Road(Mehadia, Lugoj, 70),
    Road(Mehadia, Drobeta, 75), //Mehadia Edges
    Road(Drobeta, Mehadia, 75),
    Road(Drobeta, Craiova, 120), //Drobeta Edges
    Road(RimnicuVilcea, Sibiu, 80),
    Road(RimnicuVilcea, Craiova, 146),
    Road(RimnicuVilcea, Pitesti, 97), //Rimnicu Vilcea Edges
    Road(Craiova, Drobeta, 120),
    Road(Craiova, RimnicuVilcea, 146),
    Road(Craiova, Pitesti, 138), //Craiova Edges
    Road(Fagaras, Sibiu, 99),
    Road(Fagaras, Bucharest, 211), //Fagaras Edges
    Road(Pitesti, Craiova, 138),
    Road(Pitesti, RimnicuVilcea, 97),
    Road(Pitesti, Bucharest, 101), //Pitesti Edges
    Road(Bucharest, Pitesti, 101),
    Road(Bucharest, Fagaras, 211),
    Road(Bucharest, Urziceni, 85),
    Road(Bucharest, Giurgiu, 90), //Bucharest Edges
    Road(Giurgiu, Bucharest, 90), //Giurgiu Edges
    Road(Urziceni, Bucharest, 85),
    Road(Urziceni, Vaslui, 142),
    Road(Urziceni, Hirsova, 98), //Urziceni Edges
    Road(Neamt, Iasi, 87),       //Neamt Edges
    Road(Iasi, Neamt, 87),
    Road(Iasi, Vaslui, 92), //Iasi Edges
    Road(Vaslui, Iasi, 92),
    Road(Vaslui, Urziceni, 142), //Vaslui Edges
    Road(Hirsova, Urziceni, 98),
    Road(Hirsova, Eforie, 86), //Hirsova Edges
    Road(Eforie, Hirsova, 86)  //Eforie Edges
  ).foldLeft(Map.empty[City, List[Road]]) { (acc, road) =>
    val from        = road.from
    val listForEdge = acc.getOrElse(from, List.empty[Road])
    acc.updated(from, road :: listForEdge)
  }

  sealed trait RomaniaState
  final case class In(city: City) extends RomaniaState

  sealed trait RomaniaAction
  final case class GoTo(city: City) extends RomaniaAction
  case object NoAction              extends RomaniaAction

  val sCTag: ClassTag[RomaniaState]  = classTag[RomaniaState]
  val aCTag: ClassTag[RomaniaAction] = classTag[RomaniaAction]

  val snCTag: ClassTag[StateNode[RomaniaState, RomaniaAction]] =
    classTag[StateNode[RomaniaState, RomaniaAction]]

  val cnCTag: ClassTag[CostNode[RomaniaState, RomaniaAction]] =
    classTag[CostNode[RomaniaState, RomaniaAction]]

  class RomaniaRoadProblem(
      val initialState: RomaniaState,
      val goalState: RomaniaState
  ) extends Problem[RomaniaState, RomaniaAction] {
    def result(
        currentState: RomaniaState,
        action: RomaniaAction
    ): RomaniaState = action match {
      case GoTo(city) => In(city)
      case NoAction   => currentState
    }

    def actions(state: RomaniaState): List[RomaniaAction] = state match {
      case In(city) => roadsFromCity(city).map(road => GoTo(road.to))
    }

    def isGoalState(state: RomaniaState): Boolean = (state, goalState) match {
      case (In(city), In(goal)) => city == goal
      case _                    => false
    }

    def stepCost(
        state: RomaniaState,
        action: RomaniaAction,
        statePrime: RomaniaState
    ): Int =
      (state, statePrime) match {
        case (In(city), In(cityPrime)) =>
          val maybeCost = roadsFromCity(city) collectFirst {
            case Road(c1, c2, cost) if c1 == city && c2 == cityPrime => cost
          }
          maybeCost.getOrElse(Int.MaxValue)
      }
  }

}
