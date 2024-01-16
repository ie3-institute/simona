package edu.ie3.simona.service.primary

import edu.ie3.simona.api.data.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.services.{DataMessage, ServiceMessage}
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}
import edu.ie3.simona.service.primary.ExtPrimaryServiceWorker.ExtPrimaryDataStateData
import org.apache.pekko.actor.{ActorContext, ActorRef, Props}

import scala.util.Try

object ExtPrimaryDataService {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtPrimaryDataService(scheduler: ActorRef)
    )


}
final case class ExtPrimaryDataService(
                                        override val scheduler: ActorRef
                                      )
  extends SimonaService[ExtPrimaryDataStateData](scheduler)
  with ExtDataSupport[ExtPrimaryDataStateData] {

  /** Initialize the concrete service implementation using the provided
   * initialization data. This method should perform all heavyweight tasks
   * before the actor becomes ready. The return values are a) the state data of
   * the initialized service and b) optional triggers that should be send to
   * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
   * message that is send in response to the trigger that is send to start the
   * initialization process
   *
   * @param initServiceData
   * the data that should be used for initialization
   * @return
   * the state data of this service and optional tick that should be included
   * in the completion message
   */
  override def init(initServiceData: ServiceStateData.InitializeServiceStateData): Try[(ExtPrimaryDataStateData, Option[Long])] = ???

  /** Handle a request to register for information from this service
   *
   * @param registrationMessage
   * registration message to handle
   * @param serviceStateData
   * current state data of the actor
   * @return
   * the service stata data that should be used in the next state (normally
   * with updated values)
   */
  override protected def handleRegistrationRequest(
                                                    registrationMessage: ServiceMessage.ServiceRegistrationMessage
                                                  )(implicit serviceStateData: ExtPrimaryDataStateData): Try[ExtPrimaryDataStateData] = ???

  /** Send out the information to all registered recipients
   *
   * @param tick
   * current tick data should be announced for
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the service stata data that should be used in the next state (normally
   * with updated values) together with the completion message that is send
   * in response to the trigger that was sent to start this announcement
   */
  override protected def announceInformation(tick: Long)(implicit serviceStateData: ExtPrimaryDataStateData, ctx: ActorContext): (ExtPrimaryDataStateData, Option[Long]) = ???

  /** Handle a message from outside the simulation
   *
   * @param extMsg
   * the external incoming message
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the updated state data
   */
  override protected def handleDataMessage(extMsg: DataMessageFromExt)(implicit serviceStateData: ExtPrimaryDataStateData): ExtPrimaryDataStateData = ???

  /** Handle a message from inside SIMONA sent to external
   *
   * @param extResponseMsg
   * the external incoming message
   * @param serviceStateData
   * the current state data of this service
   * @return
   * the updated state data
   */
  override protected def handleDataResponseMessage(extResponseMsg: DataMessage)(implicit serviceStateData: ExtPrimaryDataStateData): ExtPrimaryDataStateData = ???


}

