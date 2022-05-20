package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{ProvisionMessage, ServiceRegistrationMessage}

import java.util.UUID

sealed trait OpfMessage

object OpfMessage {

  final case class RegisterForOpfDataMessage(
                                                    generator: UUID
                                                    ) extends OpfMessage with ServiceRegistrationMessage

  trait ExtOpfData extends PrimaryData

  final case class ProvideSetpointMessage(
                                           override val tick: Long,
                                           override val data: ExtOpfData,
                                           override val nextDataTick: Option[Long] = None
                                         ) extends OpfMessage with ProvisionMessage[ExtOpfData]

  final case class ActivePowerRequest(
                                     tick: Long
                                     )

  final case class SetpointData (
                                 setpoints: SetPoints   // zu implementieren in API (vgl. public static final class EvcsMovements in Datei EvMovementsMessage.java)
                                 ) extends ExtOpfData

  trait OpfResponseMessage extends OpfMessage

  final case class ActivePowerResponse(
                                      generator: UUID,
                                      activePower: Double // hier fehlt noch der richtige Datentyp, da an dieser stelle nicht die Setpoints aus Matpower gemeint sind, sondern die Leistung, die jeder SPA an den GA übergibt.
                                      ) extends OpfResponseMessage

}
