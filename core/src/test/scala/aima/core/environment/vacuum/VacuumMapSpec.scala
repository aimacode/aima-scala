package aima.core.environment.vacuum

import aima.core.agent.{Actuator, Agent, AgentProgram, Sensor}
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
  * @author Shawn Garner
  */
class VacuumMapSpec extends Specification with ScalaCheck {

  "must be clean if all nodes clean" in {
    val map = VacuumMap(Vector(cleanNode, cleanNode))

    map.isClean() must beTrue
  }

  "must be dirty if left node is dirty" in {
    val map = VacuumMap(Vector(dirtyNode, cleanNode))

    map.isClean() must beFalse
  }

  "must be dirty if right node is dirty" in {
    val map = VacuumMap(Vector(cleanNode, dirtyNode))

    map.isClean() must beFalse
  }

  "must be dirty if right node is dirty" in {
    val map = VacuumMap(Vector(cleanNode, dirtyNode))

    map.isClean() must beFalse
  }

  "must be dirty if both nodes are dirty" in {
    val map = VacuumMap(Vector(dirtyNode, dirtyNode))

    map.isClean() must beFalse
  }

  implicit lazy val arbVacuumMap = Arbitrary(VacuumMap())

  "dirt status of agent not on map must be none" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.getDirtStatus(agent) must beNone
  }

  "dirt status of agent must be dirty if all dirty" in {
    val agent = noopAgent
    val map   = VacuumMap(Vector(dirtyNode, dirtyNode)).addAgent(agent)

    map.getDirtStatus(agent) must beSome[VacuumPercept](DirtyPercept)
  }

  "dirt status of agent must be clean if all clean" in {
    val agent = noopAgent
    val map   = VacuumMap(Vector(cleanNode, cleanNode)).addAgent(agent)

    map.getDirtStatus(agent) must beSome[VacuumPercept](CleanPercept)
  }

  "agent location of agent not on map must be none" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.getAgentLocation(agent) must beNone
  }

  "agent location must be A if in first spot" in {
    val agent = noopAgent
    val map   = VacuumMap(Vector(agentNode(agent), cleanNode)).addAgent(agent)

    map.getAgentLocation(agent) must beSome[VacuumPercept](LocationAPercept)
  }

  "agent location must be B if in second spot" in {
    val agent = noopAgent
    val map   = VacuumMap(Vector(cleanNode, agentNode(agent))).addAgent(agent)

    map.getAgentLocation(agent) must beSome[VacuumPercept](LocationBPercept)
  }

  "moving right will put in spot B" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).moveAgent(agent, RightMoveAction).getAgentLocation(agent) must beSome[VacuumPercept](
      LocationBPercept
    )
  }

  "moving left will put in spot A" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).moveAgent(agent, LeftMoveAction).getAgentLocation(agent) must beSome[VacuumPercept](
      LocationAPercept
    )
  }

  "updating dirt status of dirty will return dirt percept of dirty" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).updateStatus(agent, DirtyPercept).getDirtStatus(agent) must beSome[VacuumPercept](DirtyPercept)
  }

  "updating dirt status of clean will return clean percept of dirty" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).updateStatus(agent, CleanPercept).getDirtStatus(agent) must
      beSome[VacuumPercept](CleanPercept)
  }

  "removing agent must have no location percept" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).removeAgent(agent).getAgentLocation(agent) must beNone
  }

  "removing agent must have no status percept" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).removeAgent(agent).getDirtStatus(agent) must beNone
  }

  "adding an agent and then removing them should be the original input, eg f'(f(x)) = x" in prop { map: VacuumMap =>
    val agent = noopAgent
    map.addAgent(agent).removeAgent(agent) must_== map
  }

  def noopAgentProgram = new AgentProgram[VacuumPercept, VacuumAction] {
    def agentFunction: AgentFunction = { _ =>
      NoAction
    }
  }

  def noopAgent: Agent[Vacuum, VacuumPercept, VacuumAction] =
    new Agent[Vacuum, VacuumPercept, VacuumAction] {
      override def actuators: List[Actuator[Vacuum, VacuumAction]] = List.empty
      override def sensors: List[Sensor[Vacuum, VacuumPercept]]    = List.empty
      override def agentProgram: AgentProgram[VacuumPercept, VacuumAction] =
        noopAgentProgram

    }

  def agentNode(agent: Agent[Vacuum, VacuumPercept, VacuumAction] = noopAgent) =
    VacuumMapNode(maybeAgent = Some(agent))
  def dirtyNode = VacuumMapNode(DirtyPercept, None)
  def cleanNode = VacuumMapNode(CleanPercept, None)
}
