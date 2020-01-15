package aima.core.environment.vacuum

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
  * @author Shawn Garner
  */
class SimpleReflexVacuumAgentSpec extends Specification {

  "should move right if location A" in new context {
    agent.agentFunction.apply(LocationAPercept) must beLike {
      case RightMoveAction => ok
    }
  }

  "should move left if location B" in new context {
    agent.agentFunction.apply(LocationBPercept) must beLike {
      case LeftMoveAction => ok
    }
  }

  "should suck if dirty" in new context {
    agent.agentFunction.apply(DirtyPercept) must beLike {
      case Suck => ok
    }
  }

  trait context extends Scope {
    val agent = new SimpleReflexVacuumAgentProgram

  }
}
