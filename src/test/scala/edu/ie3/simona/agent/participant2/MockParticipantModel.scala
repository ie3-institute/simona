/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant2.MockParticipantModel._
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.energy.{Kilowatts, Power}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power => QuantPower}

class MockParticipantModel(
    override val uuid: UUID = UUID.fromString("0-0-0-0-1"),
    override val id: String = "MockParticipant 1",
    override val sRated: ApparentPower = Kilovoltamperes(10),
    override val cosPhiRated: Double = 0.9,
    override val qControl: QControl = CosPhiFixed(0.9),
    mockActivationTicks: Map[Long, Long] = Map.empty,
    mockChangeAtNext: Set[Long] = Set.empty,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      MockState,
    ] {

  override val initialState: ModelInput => MockState = { input =>
    val maybeAdditionalPower = input.receivedData.collectFirst {
      case data: MockSecondaryData =>
        data.additionalP
    }

    MockState(
      maybeAdditionalPower,
      input.currentTick,
    )
  }

  override def determineState(
      lastState: MockState,
      operatingPoint: ActivePowerOperatingPoint,
      input: ModelInput,
  ): MockState = initialState(input)

  override def determineOperatingPoint(
      state: MockState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    (
      ActivePowerOperatingPoint(
        Kilowatts(6) + state.additionalP.getOrElse(zeroKW)
      ),
      mockActivationTicks.get(state.tick),
    )
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: MockState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: PrimaryData.ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      MockResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = {
    MockResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )
  }

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    throw new NotImplementedError() // Not tested

  override def calcFlexOptions(
      state: MockState
  ): FlexibilityMessage.ProvideFlexOptions = {
    val additionalP = state.additionalP.getOrElse(zeroKW)
    ProvideMinMaxFlexOptions(
      uuid,
      Kilowatts(1) + additionalP,
      Kilowatts(-1) + additionalP,
      Kilowatts(3) + additionalP,
    )
  }

  override def handlePowerControl(
      state: MockState,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (ActivePowerOperatingPoint, OperationChangeIndicator) =
    (
      ActivePowerOperatingPoint(setPower),
      OperationChangeIndicator(
        changesAtNextActivation = mockChangeAtNext.contains(state.tick),
        changesAtTick = mockActivationTicks.get(state.tick),
      ),
    )

  override def handleRequest(
      state: MockState,
      ctx: ActorContext[ParticipantAgent.Request],
      msg: ParticipantRequest,
  ): MockState = {
    msg match {
      case MockRequestMessage(_, replyTo) =>
        replyTo ! MockResponseMessage
    }

    state
  }

}

object MockParticipantModel {

  /** Simple [[ModelState]] to test its usage in operation point calculations
    *
    * @param additionalP
    *   Power value that is added to the power or flex options power for testing
    *   purposes
    */
  final case class MockState(
      additionalP: Option[Power],
      override val tick: Long,
  ) extends ModelState

  final case class MockResult(
      time: ZonedDateTime,
      inputModel: UUID,
      p: ComparableQuantity[QuantPower],
      q: ComparableQuantity[QuantPower],
  ) extends SystemParticipantResult(time, inputModel, p, q)

  final case class MockRequestMessage(
      override val tick: Long,
      replyTo: ActorRef[MockResponseMessage.type],
  ) extends ParticipantRequest

  case object MockResponseMessage

  final case class MockSecondaryData(additionalP: Power) extends SecondaryData

}
