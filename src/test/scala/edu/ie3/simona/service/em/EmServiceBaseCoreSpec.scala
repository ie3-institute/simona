/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.model.em.{EmSetPoint, FlexOptionRequest}
import edu.ie3.simona.api.ontology.em.ProvideEmData
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  IssueFlexControl,
  IssueNoControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.flex.{
  FlexibilityMessage,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.util.quantities.QuantityUtils.asKiloWatt
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.{Kilowatts, Power, Watts}

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsJava

class EmServiceBaseCoreSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with ConfigTestData {

  "An EmServiceBaseCore" should {
    // logger used by some methods
    given Logger = LoggerFactory.getLogger(classOf[EmServiceBaseCore])
    given Power = Watts(1e-3)

    "handle registration of parentless em agent correctly" in {
      val emptyCore = EmServiceBaseCore()

      val emAgent = TestProbe[EmAgent.Message]("emAgent").ref
      val emUuid = UUID.randomUUID()

      val updatedCore = emptyCore.handleRegistration(
        EmServiceRegistration(emAgent, emUuid, None, None)
      )

      updatedCore.uuidToAgent shouldBe Map(emUuid -> emAgent)
      updatedCore.flexOptions shouldBe ReceiveDataMap.empty
      updatedCore.completions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.uuidToInferior shouldBe Map.empty
      updatedCore.sendOptionsToExt shouldBe false
      updatedCore.canHandleSetPoints shouldBe false
      updatedCore.setPointOption shouldBe None
    }

    "handle registration of em agent with parent correctly" in {
      val emptyCore = EmServiceBaseCore()

      val emAgent = TestProbe[EmAgent.Message]("emAgent").ref
      val emUuid = UUID.randomUUID()

      val parentEmAgent = TestProbe[EmAgent.Message]("parentEmAgent").ref
      val parentEmUuid = UUID.randomUUID()

      val updatedCore = emptyCore.handleRegistration(
        EmServiceRegistration(
          emAgent,
          emUuid,
          Some(parentEmAgent),
          Some(parentEmUuid),
        )
      )

      updatedCore.uuidToAgent shouldBe Map(emUuid -> emAgent)
      updatedCore.flexOptions shouldBe ReceiveDataMap.empty
      updatedCore.completions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.uuidToInferior shouldBe Map(parentEmUuid -> Set(emUuid))
      updatedCore.sendOptionsToExt shouldBe false
      updatedCore.canHandleSetPoints shouldBe false
      updatedCore.setPointOption shouldBe None
    }

    "handle flex option request correctly" in {
      val emAgent = TestProbe[EmAgent.Message]("emAgent")
      val emUuid = UUID.randomUUID()

      val core = EmServiceBaseCore().handleRegistration(
        EmServiceRegistration(emAgent.ref, emUuid, None, None)
      )

      val flexRequests = new ProvideEmData(
        0L,
        Map(emUuid -> new FlexOptionRequest(emUuid, true)).asJava,
        Map.empty.asJava,
        Map.empty.asJava,
      )

      val (updatedCore, msgToExt) = core.handleExtMessage(0L, flexRequests)

      // the agent should receive a flex option request for disaggregated
      emAgent.expectMessage(FlexActivation(0L, true))

      // we should have no message for the external simulation
      msgToExt shouldBe None

      // check updated state of the core
      updatedCore.uuidToAgent shouldBe Map(emUuid -> emAgent.ref)
      updatedCore.flexOptions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.completions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.uuidToInferior shouldBe Map.empty
      updatedCore.sendOptionsToExt shouldBe true // since we received a flex option request
      updatedCore.canHandleSetPoints shouldBe false
      updatedCore.setPointOption shouldBe None
    }

    "handle em set point provision" in {
      val emAgent = TestProbe[EmAgent.Message]("emAgent")
      val emUuid = UUID.randomUUID()

      val core = EmServiceBaseCore().handleRegistration(
        EmServiceRegistration(emAgent.ref, emUuid, None, None)
      )

      val setPoints = Map(emUuid -> new EmSetPoint(emUuid, 5.asKiloWatt))

      val setPointData = new ProvideEmData(
        0L,
        Map.empty.asJava,
        Map.empty.asJava,
        setPoints.asJava,
      )

      val (updatedCore, msgToExt) = core.handleExtMessage(0L, setPointData)

      // the agent should receive a flex option request
      // since the agent cannot handle the set point currently
      emAgent.expectMessage(FlexActivation(0L))

      // we should have no message for the external simulation
      msgToExt shouldBe None

      // check updated state of the core
      updatedCore.uuidToAgent shouldBe Map(emUuid -> emAgent.ref)
      updatedCore.flexOptions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.completions shouldBe ReceiveDataMap(Set(emUuid))
      updatedCore.uuidToInferior shouldBe Map.empty
      updatedCore.sendOptionsToExt shouldBe false // since we didn't receive a flex option request
      updatedCore.canHandleSetPoints shouldBe false
      updatedCore.setPointOption shouldBe Some(
        setPoints
      ) // save the set point data until we can handle it

      // handle flex options
      val (coreAfterFlexOptionProvision, msgToExt2) =
        updatedCore.handleFlexResponse(
          0L,
          ProvideFlexOptions(
            emUuid,
            PowerLimitFlexOptions(zeroKW, zeroKW, zeroKW),
          ),
          Left(emUuid),
        )

      // the agent should receive the set point
      emAgent.expectMessageType[IssueFlexControl] match {
        case FlexibilityMessage.IssuePowerControl(tick, setPower) =>
          tick shouldBe 0L
          setPower should approximate(Kilowatts(5))
      }

      // we should have no message for the external simulation, since we have not received a request
      msgToExt2 shouldBe None

      // check updated state of the core
      coreAfterFlexOptionProvision.uuidToAgent shouldBe Map(
        emUuid -> emAgent.ref
      )
      coreAfterFlexOptionProvision.flexOptions shouldBe ReceiveDataMap.empty // empty, since we received all flex options
      coreAfterFlexOptionProvision.completions shouldBe ReceiveDataMap(
        Set(emUuid)
      )
      coreAfterFlexOptionProvision.uuidToInferior shouldBe Map.empty
      coreAfterFlexOptionProvision.sendOptionsToExt shouldBe false // since we didn't receive a flex option request
      coreAfterFlexOptionProvision.canHandleSetPoints shouldBe true // since all agents have provided flex options
      coreAfterFlexOptionProvision.setPointOption shouldBe None // empty, since we handled the data
    }

    "handle flex requests correctly" in {
      val emAgent = TestProbe[EmAgent.Message]("emAgent")
      val emUuid = UUID.randomUUID()
      val core = EmServiceBaseCore().handleRegistration(
        EmServiceRegistration(emAgent.ref, emUuid)
      )

      val msg = IssueNoControl(0L)
      val (updatedCore, msgToExt) = core.handleFlexRequest(msg, emAgent.ref)

      // since we don't update the core
      updatedCore shouldBe core.copy(completions =
        core.completions.addExpectedKey(emUuid)
      )

      // we should have no message for the external simulation
      msgToExt shouldBe None

      // the core simply sends the message to the receiver
      emAgent.expectMessage(msg)
    }

  }

}
