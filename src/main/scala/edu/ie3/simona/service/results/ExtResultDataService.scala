package edu.ie3.simona.service.results

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.api.data.results.ExtResultsData
import edu.ie3.simona.api.data.results.ontology.{ProvideResultEntities, RequestResultEntities, ResultDataMessageFromExt}
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.services.ResultMessage.ResultResponseMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.DataMessage
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.{ExtDataSupport, SimonaService}
import edu.ie3.simona.service.results.ExtResultDataService.{ExtResultsStateData, InitExtResultsData}
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.{Failure, Success, Try}

object ExtResultDataService {
  def props(scheduler: ActorRef): Props =
    Props(
      new ExtResultDataService(scheduler: ActorRef)
    )

  final case class ExtResultsStateData(
                                   extResultsData: ExtResultsData,
                                   uuidToActorRef: Map[UUID, ActorRef] = Map.empty[UUID, ActorRef],
                                   extResultsMessage: Option[ResultDataMessageFromExt] = None
                                 ) extends ServiceBaseStateData

  final case class InitExtResultsData(
                                  extResultsData: ExtResultsData
                                ) extends InitializeServiceStateData
}


class ExtResultDataService(override val scheduler: ActorRef)
  extends SimonaService[ExtResultsStateData](scheduler)
    with ExtDataSupport[ExtResultsStateData] {

  override def init(
                     initServiceData: InitializeServiceStateData
                   ): Try[(ExtResultsStateData, Option[Long])] = {
    initServiceData match {
      case InitExtResultsData(extResultsData) =>
        val resultInitializedStateData = ExtResultsStateData(extResultsData)
        Success(resultInitializedStateData, None)

      case invalidData =>
        Failure(
          new InitializationException(
            s"Provided init data '${invalidData.getClass.getSimpleName}' for EV service are invalid!"
          )
        )
    }
  }

  override protected def handleRegistrationRequest(
                                                    registrationMessage: ServiceRegistrationMessage
                                                  )(implicit serviceStateData: ExtResultsStateData):
  Try[ExtResultsStateData] =
        Failure(
          ServiceException(
            "For this service is no registration possible!"
          )
        )

  override protected def announceInformation(
                                              tick: Long
                                            )(
    implicit serviceStateData: ExtResultsStateData,
    ctx: ActorContext): (ExtResultsStateData, Option[Long]) = {

    serviceStateData.extResultsMessage.getOrElse(
      throw ServiceException(
        "ExtEvDataService was triggered without ExtEvMessage available"
      )
    ) match {
      case _: RequestResultEntities =>
        requestResults(tick)
    }
    (null, None) // No Annoucement
  }

  /** Handle a message from outside the simulation
   *
   * @param extMsg
   * the external incoming message
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the updated state data
   */
  override protected def handleDataMessage(
                                            extMsg: DataMessageFromExt
                                          )(
    implicit serviceStateData: ExtResultsStateData
  ): ExtResultsStateData = {
    serviceStateData
  }

  /** Handle a message from inside SIMONA sent to external
   *
   * @param extResponseMsg
   * the external incoming message
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the updated state data
   */
  override protected def handleDataResponseMessage(
                                                    extResponseMsg: DataMessage
                                                  )(
    implicit serviceStateData: ExtResultsStateData): ExtResultsStateData = {
    extResponseMsg match {
      case ResultResponseMessage(results) =>
        serviceStateData.extResultsData.queueExtResponseMsg(
          new ProvideResultEntities(results.toList.asJava))
        serviceStateData.copy()
    }
  }

  private def requestResults(
                            tick: Long
                            )(implicit serviceStateData: ExtResultsStateData): (ExtResultsStateData, Option[Long]) = {

    (serviceStateData.copy(), None)
  }






}