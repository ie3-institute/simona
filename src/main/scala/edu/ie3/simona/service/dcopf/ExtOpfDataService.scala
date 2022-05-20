import ExtOpfDataService.{ExtOpfStateData, InitExtOpfData}
import akka.actor.{ActorRef, Props}
import akka.protobufv3.internal.ServiceException
import edu.ie3.simona.api.data.ontology.ExtDataMessage
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.messages.services.{EvMessage, ServiceMessage}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.OpfMessage._
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.service.ServiceStateData.{InitializeServiceStateData, ServiceBaseStateData}
import edu.ie3.simona.service.primary.PrimaryServiceWorker
import edu.ie3.simona.service.primary.PrimaryServiceWorker.PrimaryServiceInitializedStateData
import edu.ie3.simona.service.{ExtDataSupport, ServiceStateData, SimonaService}

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.{Failure, Success, Try}

object ExtOpfDataService {

  def props(scheduler:ActorRef): Props =
    Props(
      new ExtOpfDataService(scheduler: ActorRef)
    )

  final case class ExtOpfStateData(
                                    extOpfData: ExtOpfData, // muss im API Projekt implementiert werden
                                    uuidToActorRef: Map[UUID, ActorRef] = Map.empty[UUID, ActorRef],
                                    extOpfMessage: Option[ExtDataExchangeMessage] = None, // muss im API Projekt implementiert werden
                                    setPoints: Map[UUID, Double] = Map.empty,
                                    activePowerResponses: Map[UUID, Option[Double]] = Map.empty
  ) extends ServiceBaseStateData

  final case class InitExtOpfData(
                                   extOpfData: ExtOpfData, // muss im API Projekt implementiert werden
  ) extends InitializeServiceStateData
}

class ExtOpfDataService(override val scheduler: ActorRef)
  extends SimonaService[ExtOpfStateData](scheduler)
  with ExtDataSupport[ExtOpfStateData] {

  override def init(
                   initServiceData: ServiceStateData.InitializeServiceStateData
                   ): Try[
    (
        ExtOpfStateData,
        Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]

      )
  ] = initServiceData match {
    case InitExtOpfData(extOpfData) =>
      val extOpfInitializedStateData = ExtOpfStateData(extOpfData)
      initPrimaryServiceWorker()

      Success(extOpfInitializedStateData, None)

    case invalidData =>
      Failure(new InitializationException(s"Provided init data '${invalidData.getClass.getSimpleName}' for External OPF Data Service is invalid!"))

  }

  private def initPrimaryServiceWorker(
                                      initServiceData: ServiceStateData.InitializeServiceStateData
                                   ): Try[(
      PrimaryServiceInitializedStateData[V],
      Option[Seq[SchedulerMessage.ScheduleTriggerMessage]])
  ] = {
    (initServiceData match { // oder direkt im PSW implementieren und dann hier nur generell aufrufen?
      case PrimaryServiceWorker.InterfaceInitPrimaryServiceStateData
    })


  }

  /**
    * Handle a request to register for information from this service
    *
    * @param registrationMessage
    *   registration message to handle
    * @param serviceStateData
    *   current state data of the actor
    *  @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values)
    */
  override def handleRegistrationRequest(registrationMessage: ServiceRegistrationMessage
                                        )(implicit serviceStateData: ExtOpfStateData
  ): Try[ExtOpfStateData] =
    registrationMessage match {
      case RegisterForOpfDataMessage(generators) =>         // muss als eigener Scala trait ExtExchangeMessage.scala geschrieben werden (vgl. EvMessage.scala)
        Success(handleRegistrationRequest(sender(), generators))
      case invalidMessage =>
        Failure(InvalidRegistrationRequestException(
          "Cannot register an agent for External OPF Data Service with registration "
            + s"request message '${invalidMessage.getClass.getSimpleName}'"))
    }

  /** Try to register the sending agent with its uuid (and location type value?) for External OPF Data updates
    *
    * @param agentToBeRegistered
    *   the agent that wants to be registeres
    * @param generators
    *   the system participant that should receive the setpoints
    * @param serviceStateData
    *   the current servoce state data of this service
    * @return
    *   an updated state data of this service that contains registration information if the registration has been carried out successfully
    */
  private def handleRegistrationRequest(
                                       agentToBeRegistered: ActorRef,
                                       generators: UUID
                                       )(implicit serviceStateData: ExtOpfStateData
  ):ExtOpfStateData = {
    log.debug("Received External OPF Data Service registration from {} for [generators: {}]", agentToBeRegistered.path.name, generators
    )

    serviceStateData.uuidToActorRef.get(generators) match {
      case None => // Actor is not registered yet
        agentToBeRegistered ! RegistrationSuccessfulMessage(None)

        serviceStateData.copy(
          uuidToActorRef = serviceStateData.uuidToActorRef + (generators -> agentToBeRegistered)
        )
      case Some(_) => // Actor is already registered, do nothing
        log.warning("Sending actor {} is already registered", agentToBeRegistered)

      serviceStateData
    }

  }

  /**Send out information to all registered recipients DEPENDING ON THE MAPPED SETPOINTS???
    *
    * @param tick
    *   current tick data should be announced for
    * @param serviceStateData
    *   the current state data of this service
    *  @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is send
    *   in response to the trigger that was sent to start this announcement
    */
  override protected def announceInformation(
                                              tick: Long
                                            )(implicit serviceStateData: ExtOpfStateData
  ): (ExtOpfStateData,
    Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
    ) = {
    serviceStateData.extOpfMessage.getOrElse(
      throw ServiceException(
        "ExtOpfDataActor was triggered without available ExtOpfMessage"
      )
    ) match {
      case _: RequestActivePower =>                // muss im API Projekt implementiert werden
        requestCurrentPower(tick)
      case setpointsMessage: SetpointsMessage =>    // muss im API Projekt implementiert werden
        sendSetpoints(tick, setpointsMessage.getSetpoints)
    }

  }

  /** Request active power of each generator for the current tick.
    * Additionally: when receiving the active power send from the generators, compare it with the sent values for the setpoints.
    * Send back an "OK" message if the values match. Otherwise, something went wrong!
    *
    * @param tick
    * @param serviceStateData
    * @return
    */
  def requestCurrentPower(tick: Long)(implicit serviceStateData: ExtOpfStateData
  ): (ExtOpfStateData, Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]) = {
    serviceStateData.uuidToActorRef.foreach {
      case (_, generatorActor) =>
        generatorActor ! ActivePowerRequest(tick)  // muss im API Projekt implementiert werden
    }

    val activePower: Map[UUID, Option[Double]] =
      serviceStateData.uuidToActorRef.map {
        case (generators, _) =>
          generators -> None
      }

    if (activePower.isEmpty)
      serviceStateData.extOpfData.queueExtResponseMsg(new ProvideActivePower())   // muss in extExchangeData.java implementiert werden

    (serviceStateData.copy(
      extOpfData = None,
      activePower = activePower),
      None)
  }


  /** Send out the active power setpoints to the affiliated generators
    * TBD: Where should the differentiation between the generators be taken out?
    *   pos. 1: announceInformation calls sendSetpoints separately for each generator
    *   pos. 2: sendSetpoints uses filtered position Data to send ! ProvideSetpointMessage to each generator separately
    *
    * @param tick
    * @param setpoints
    * @param serviceStateData
    * @return
    */
  def sendSetpoints(
                     tick: Long,
                     setpoints: java.util.Map[UUID, Double]
                   )(implicit serviceStateData: ExtOpfStateData
  ): (ExtOpfStateData, Option[Seq[ScheduleTriggerMessage]]) = {
    val filteredPositionData =  // maybe different nameing
      setpoints.asScala.flatMap {
        case (generator, setpoint) =>
      serviceStateData.uuidToActorRef.get(generator) match {
        case Some(generatorActor) =>
          Some(generator, generatorActor, setpoint)
        case None =>
          log.warning("A corresponding actor ref for UUID {} could not be found", generator)
          None
      }}

    val scheduleTriggerMsg = filteredPositionData.map { case (_, generatorActor, setpoint) =>
    generatorActor ! ProvideSetpointMessage(
      tick,
      SetpointData(setpoints) // muss noch implementiert werden (extends ExtExchangeData)
    )

    //schedule activation of participant
    ScheduleTriggerMessage(
      ActivityStartTrigger(tick),
      generatorActor)
    }

    (serviceStateData.copy(
      extOpfMessage = None
    ), Option.when(scheduleTriggerMsg.nonEmpty)(scheduleTriggerMsg.toSeq)
    )
  }

  /** Beschreibung einfügen
    *
    * @param extMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    *  @return
    *   the updated state data
    */
  override protected def handleDataMessage(
                                            extMsg: ExtDataMessage
                                          )(implicit serviceStateData: ExtOpfStateData
  ): ExtOpfStateData =
    extMsg match {
      case extOpfMessage: ExtOpfMessage => //muss separat in API implementiert werden
        serviceStateData.copy(
          extOpfMessage = Some(extOpfMessage)
        )
    }

  /**Beschreibung einfügen
    *
    * @param extResponseMsg
    * @param serviceStateData
    * @return
    */
  override protected def handleDataResponseMessage(
                                                    extResponseMsg: OpfResponseMessage
                                                  )(implicit serviceStateData: ExtOpfStateData
  ): ExtOpfStateData = {
    extResponseMsg match {
      case ActivePowerResponse(generator, activePower) =>
        val updatedResponse = serviceStateData.activePowerResponses
    }

    serviceStateData.copy()
  }

}

// class Attribute sind meist unterschiedlich für jedes Object (z.B. Name)
// Object Attribute sind meist für alle identisch
// Object entspricht statischer Klasse in Java

// kann ich auch einfach ein Objekt vom Txp Primary Service Worker hier in der ExtOpdDataService Klasse erstellen?
// oder noch eine Klasse erstellen, die von PrimaryServiceWorker erbt?

// neuen case in PSP???