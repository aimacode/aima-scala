package aima.core.environment.map2d

/**
  * @author Shawn Garner
  */
sealed trait Map2DAction
case object NoAction                     extends Map2DAction
final case class GoAction(gotTo: String) extends Map2DAction
