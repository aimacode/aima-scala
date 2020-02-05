package aima.core.environment.vacuum

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class ModelBasedReflexVacuumAgentSpec extends Specification {

  "should suck if location A" in new context {
    agent.agentFunction.apply(LocationAPercept) must beLike {
      case Suck => ok
    }
  }

  "should move if location A is clean" in new context {
    agent.agentFunction.apply(LocationAPercept)
    agent.agentFunction.apply(CleanPercept) must beLike {
      case RightMoveAction => ok
    }
  }

  "should assume dirty after moving to location B" in new context {
    agent.agentFunction.apply(LocationAPercept)
    agent.agentFunction.apply(CleanPercept)
    agent.agentFunction.apply(NoPercept) must beLike {
      case Suck => ok
    }
  }

  "should suck if location B" in new context {
    agent.agentFunction.apply(LocationBPercept) must beLike {
      case Suck => ok
    }
  }

  "should move if location B is clean" in new context {
    agent.agentFunction.apply(LocationBPercept)
    agent.agentFunction.apply(CleanPercept) must beLike {
      case LeftMoveAction => ok
    }
  }

  "should assume dirty after moving to location A" in new context {
    agent.agentFunction.apply(LocationBPercept)
    agent.agentFunction.apply(CleanPercept)
    agent.agentFunction.apply(NoPercept) must beLike {
      case Suck => ok
    }
  }

  "should suck if dirty" in new context {
    agent.agentFunction.apply(DirtyPercept) must beLike {
      case Suck => ok
    }
  }

  "should do nothing if low on power" in new context {
    val resultStream = LazyList.continually(agent.agentFunction.apply(DirtyPercept))
    resultStream.take(100).last must beLike {
      case NoAction => ok
    }
  }

  trait context extends Scope {
    val agent = new ModelBasedReflexVacuumAgentProgram
  }
}
