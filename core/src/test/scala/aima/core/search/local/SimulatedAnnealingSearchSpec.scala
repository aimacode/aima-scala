package aima.core.search.local

import aima.core.search.local.time.TimeStep
import org.specs2.mutable.Specification

import scala.util.{Success, Try}

/**
  * @author Shawn Garner
  */
class SimulatedAnnealingSearchSpec extends Specification {

  "BasicSchedule" >> {
    import aima.core.search.local.SimulatedAnnealingSearch.BasicSchedule.schedule
    import aima.core.search.local.time.TimeStep.Implicits._

    def increment(ts: TimeStep, times: Int): Try[TimeStep] = times match {
      case 0 => Success(ts)
      case _ => ts.step.flatMap(increment(_, times - 1))
    }

    "lower limit check" in {
      schedule(TimeStep.start) must beCloseTo(19.1d within 3.significantFigures)
    }

    "upper limit check" in {
      increment(TimeStep.start, 98).map(schedule(_)) must beSuccessfulTry(
        beCloseTo(0.232d within 3.significantFigures))
    }

    "over limit check" in {
      increment(TimeStep.start, 99).map(schedule(_)) must beSuccessfulTry(beCloseTo(0.00d within 3.significantFigures))
    }
  }

}
