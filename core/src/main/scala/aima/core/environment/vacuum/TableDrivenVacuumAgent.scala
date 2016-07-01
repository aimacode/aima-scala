package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.environment.vacuum.DirtStatusPercepts.{Dirty, Clean}
import aima.core.environment.vacuum.LocationPercepts.{B, A}
import aima.core.environment.vacuum.SuckerActions.Suck

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgent extends TableDrivenAgent {
  override def lookupTable: LookupTable = {
    case List(_, Dirty) => Suck
    case List(A, Clean) => MoveActions.Right
    case List(B, Clean) => MoveActions.Left
    case List(_, _, _, Dirty) => Suck
    case List(_, _, A, Clean) => MoveActions.Right
    case List(_, _, B, Clean) => MoveActions.Left
    case List(_, _, _, _, _, Dirty) => Suck
    case List(_, _, _, _, A, Clean) => MoveActions.Right
    case List(_, _, _, _, B, Clean) => MoveActions.Left
    case List(_, _, _, _, _, _, _, Dirty) => Suck
    case List(_, _, _, _, _, _, A, Clean) => MoveActions.Right
    case List(_, _, _, _, _, _, B, Clean) => MoveActions.Left
    case _ => NoAction
  }


  override def lookup(lookupTable: LookupTable, percepts: List[Percept]): Action = {
      //Since the Partial function assumes an order and pairs of location and status we need to compensate for NoPercepts
      val locationPercepts = percepts.collect {
        case lp: LocationPercepts.Value => lp
      }
      val lastLocation = locationPercepts.lastOption.getOrElse(NoPercept)

      val statusPercepts = percepts.collect {
        case sp: DirtStatusPercepts.Value => sp
      }

      val lastStatus = statusPercepts.lastOption.getOrElse(NoPercept)

      val groupedPercepts = locationPercepts.zipAll(statusPercepts,lastLocation, lastStatus).flatMap {p =>
        List(p._1, p._2)
      }
      super.lookup(lookupTable, groupedPercepts)
  }
}
