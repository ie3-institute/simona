/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.api.data.results.ontology.ResultDataMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

object ExtResultDataProvider {

  trait Request

  final case class RequestDataMessageAdapter(
      sender: ActorRef[ActorRef[ResultDataMessageFromExt]]
  ) extends Request

  final case class RequestScheduleActivationAdapter(
      sender: ActorRef[ActorRef[ScheduleServiceActivation]]
  ) extends Request

  final case class Create(
      initializeStateData: InitExtResultData,
      unlockKey: ScheduleKey,
  ) extends Request

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  def apply(
      scheduler: ActorRef[SchedulerMessage]
  ): Behavior[Request] = ???

  // -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  final case class InitExtResultData(
      extResultData: ExtResultDataConnection,
      powerFlowResolution: Long,
  )
}
