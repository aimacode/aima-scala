package aima.core.environment.vacuum

import aima.core.agent._
import aima.core.environment.vacuum.DirtStatusPercepts.{Dirty, Clean}
import aima.core.environment.vacuum.LocationPercepts.{B, A}
import aima.core.environment.vacuum.MoveActions._
import aima.core.environment.vacuum.SuckerActions.Suck

/**
  * @author Shawn Garner
  */
class TableDrivenVacuumAgent extends TableDrivenAgent {
  override def lookupTable: LookupTable = {
    case List(_, Dirty) => Suck
    case List(A, Clean) => Right
    case List(B, Clean) => Left
    case List(_, _, _, Dirty) => Suck
    case List(_, _, A, Clean) => Right
    case List(_, _, B, Clean) => Left
    case List(_, _, _, _, _, Dirty) => Suck
    case List(_, _, _, _, A, Clean) => Right
    case List(_, _, _, _, B, Clean) => Left
    case List(_, _, _, _, _, _, _, Dirty) => Suck
    case List(_, _, _, _, _, _, A, Clean) => Right
    case List(_, _, _, _, _, _, B, Clean) => Left
    case _ => NoAction
  }
}
