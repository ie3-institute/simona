/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.ev

import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.ontology.EvDataMessageFromExt
import edu.ie3.simona.model.participant2.evcs.EvModelWrapper
import edu.ie3.simona.service.ServiceStateData.ServiceBaseStateData
import edu.ie3.simona.util.ReceiveDataMap
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

final case class ExtEvStateData(
    extEvData: ExtEvDataConnection,
    uuidToActorRef: Map[UUID, ActorRef[ParticipantAgent.Request]] = Map.empty,
    extEvMessage: Option[EvDataMessageFromExt] = None,
    freeLots: ReceiveDataMap[UUID, Int] = ReceiveDataMap.empty,
    departingEvResponses: ReceiveDataMap[UUID, Seq[EvModelWrapper]] =
      ReceiveDataMap.empty,
) extends ServiceBaseStateData
