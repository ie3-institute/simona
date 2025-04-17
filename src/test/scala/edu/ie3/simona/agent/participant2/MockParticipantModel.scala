/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant2.MockParticipantModel._
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel._
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.QuantityUtils._
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.{Energy, Kilowatts, Power}
import squants.time.Seconds
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power => QuantPower}

/** Mock [[ParticipantModel]] to test various functionality of
  * [[edu.ie3.simona.model.participant2.ParticipantModelShell]] and
  * [[ParticipantAgent]].
  *
  * @param mockActivationTicks
  *   Map where a current tick maps to the next activation tick
  * @param mockChangeAtNext
  *   Set of current ticks when an activation at the next tick is desired (used
  *   with flexibility)
  */
class MockParticipantModel(
    override val uuid: UUID = MockParticipantModel.uuid,
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

  override def determineState(
      lastState: MockState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): MockState = {
    val energySinceLastState =
      operatingPoint.activePower * Seconds(tick - lastState.tick)

    lastState.copy(
      tick = tick,
      countedEnergy = lastState.countedEnergy + energySinceLastState,
    )
  }

  override def handleInput(
      state: MockState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): MockState =
    receivedData
      .collectFirst { case data: MockSecondaryData =>
        data
      }
      .map(newData =>
        state.copy(
          additionalP = Some(newData.additionalP)
        )
      )
      .getOrElse(state)

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

  override def determineFlexOptions(
      state: MockState
  ): FlexOptions = {
    val additionalP = state.additionalP.getOrElse(zeroKW)
    MinMaxFlexOptions(
      Kilowatts(1) + additionalP,
      Kilowatts(-1) + additionalP,
      Kilowatts(3) + additionalP,
    )
  }

  override def determineOperatingPoint(
      state: MockState,
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
        replyTo ! MockResponseMessage(state.countedEnergy)
    }

    state
  }

}

object MockParticipantModel {

  val uuid: UUID = UUID.fromString("0-0-0-0-1")

  /** Simple [[ModelState]] to test its usage in operation point calculations.
    * Produced and consumed energy is counted in order to test the handling of
    * states.
    *
    * @param additionalP
    *   Power value that is added to the power or flex options power for testing
    *   purposes
    * @param countedEnergy
    *   The counted produced and consumed energy since beginning of the
    *   simulation
    */
  final case class MockState(
      additionalP: Option[Power],
      countedEnergy: Energy,
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
      replyTo: ActorRef[MockResponseMessage],
  ) extends ParticipantRequest

  /** Mock response message that also enables testing of state handling
    *
    * @param countedEnergy
    *   The counted energy per current state
    */
  final case class MockResponseMessage(
      countedEnergy: Energy
  )

  final case class MockSecondaryData(additionalP: Power) extends SecondaryData

  final case class Factory(
      mockActivationTicks: Map[Long, Long] = Map.empty,
      mockChangeAtNext: Set[Long] = Set.empty,
  ) extends ParticipantModelFactory[MockState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): MockState =
      MockState(
        None,
        zeroKWh,
        tick,
      )

    override def create(): MockParticipantModel =
      new MockParticipantModel(
        mockActivationTicks = mockActivationTicks,
        mockChangeAtNext = mockChangeAtNext,
      )

  }

}
