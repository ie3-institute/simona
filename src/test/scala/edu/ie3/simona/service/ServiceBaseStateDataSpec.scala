/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.service.ServiceStateData.ServiceActivationBaseStateData
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.scalatestplus.mockito.MockitoSugar

class ServiceBaseStateDataSpec
    extends AgentSpec(
      ActorSystem(
        "ServiceBaseStateDataSpec",
        ConfigFactory
          .parseString("""
                   |akka.loggers = ["akka.testkit.TestEventListener"]
                   |akka.loglevel="OFF"
          """.stripMargin)
      )
    )
    with MockitoSugar {

  "State data for services" should {
    "convert an undefined optional tick to None on attempt to convert it to a trigger message" in {
      ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
        None,
        self
      ) shouldBe None
    }

    "convert an given tick to correct sequence of scheduler messages" in {
      ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
        Some(5L),
        self
      ) shouldBe Some(
        Seq(
          ScheduleTriggerMessage(
            ActivityStartTrigger(5L),
            self
          )
        )
      )
    }
  }
}
