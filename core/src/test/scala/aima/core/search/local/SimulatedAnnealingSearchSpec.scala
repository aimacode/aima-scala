package aima.core.search.local

import org.specs2.mutable.Specification
import aima.core.search.local.SimulatedAnnealingSearch._

/**
  * @author Shawn Garner
  */
class SimulatedAnnealingSearchSpec extends Specification {

  "BasicSchedule" >> {
    import BasicSchedule.schedule

    "lower limit check" in {
      schedule(1) must beCloseTo(19.1d within 3.significantFigures)
    }

    "upper limit check" in {
      schedule(99) must beCloseTo(0.232d within 3.significantFigures)
    }

    "over limit check" in {
      schedule(100) must beCloseTo(0.00d within 3.significantFigures)
    }

    "below time step 1 - zero check" in {
      schedule(0) must beCloseTo(20.0d within 3.significantFigures)
    }

    "negative time step check" in {
      schedule(-1) must beCloseTo(20.9d within 3.significantFigures)
    }

  }
}
