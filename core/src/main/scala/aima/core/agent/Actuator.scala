package aima.core.agent

/**
  * @author Shawn Garner
  * @author Damien Favre
  */
trait Actuator[ENVIRONMENT, ACTION] {
  def act(action: ACTION, e: ENVIRONMENT): Option[ENVIRONMENT]
}
