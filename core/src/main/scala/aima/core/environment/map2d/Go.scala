package aima.core.environment.map2d

/**
  * @author Shawn Garner
  */
sealed trait Map2DAction
case object NoOp                   extends Map2DAction
final case class Go(gotTo: String) extends Map2DAction
