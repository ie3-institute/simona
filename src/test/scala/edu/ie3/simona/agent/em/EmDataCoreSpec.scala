/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.simona.agent.em.EmDataCore.Inactive
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexRequest
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef

class EmDataCoreSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with EmInputTestData {

  "An EmDataCore" should {
    def getCore(
        pvRef: ActorRef[FlexRequest],
        storageRef: ActorRef[FlexRequest],
    ): Inactive = EmDataCore
      .create(using defaultSimulationStart)
      .addControlledAsset(pvRef, pvInput.getUuid)
      .handleSchedule(pvInput.getUuid, 0L)
      ._2
      .addControlledAsset(storageRef, storageInput.getUuid)
      .handleSchedule(storageInput.getUuid, 2L)
      ._2

    "be activated with a tick correctly" in {
      val pvRef: ActorRef[FlexRequest] = TestProbe("pv").ref
      val storageRef: ActorRef[FlexRequest] = TestProbe("storage").ref

      // activate at tick 0
      val activated =
        getCore(pvRef, storageRef).activate(0L).takeNewFlexRequests()._1
      activated shouldBe Set(pvRef)
    }

    "be activated with a new tick correctly" in {
      val pvRef: ActorRef[FlexRequest] = TestProbe("pv").ref
      val storageRef: ActorRef[FlexRequest] = TestProbe("storage").ref

      // activate at tick 1
      val activated =
        getCore(pvRef, storageRef).activateWithTick(1L).takeNewFlexRequests()._1
      activated shouldBe Set(pvRef)

      // activate at tick 2
      val activated2 =
        getCore(pvRef, storageRef).activateWithTick(2L).takeNewFlexRequests()._1
      activated2 shouldBe Set(pvRef, storageRef)
    }
  }

}
