/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.{ApparentPower, ReactivePower}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.UUID

abstract class ParticipantModel[
    OP <: OperatingPoint,
    S <: ModelState,
    OR <: OperationRelevantData,
] extends ParticipantFlexibility[OP, S, OR] {

  val uuid: UUID
  val sRated: ApparentPower
  val cosPhiRated: Double
  val qControl: QControl

  val initialState: Long => S

  protected val pRated: Power = sRated.toActivePower(cosPhiRated)

  /** Get a partial function, that transfers the current active into reactive
    * power based on the participants properties and the given nodal voltage
    *
    * @return
    *   A [[PartialFunction]] from [[Power]] and voltage ([[Dimensionless]]) to
    *   [[ReactivePower]]
    */
  def activeToReactivePowerFunc: Dimensionless => Power => ReactivePower =
    nodalVoltage =>
      qControl.activeToReactivePowerFunc(
        sRated,
        cosPhiRated,
        nodalVoltage,
      )

  /** With the given current state and the given operation-relevant data,
    * determines the operating point that is currently valid until the next
    * operating point is determined. Also, optionally returns a tick at which
    * the state will change unless the operating point changes due to external
    * influences beforehand.
    *
    * This method should be able to handle calls at arbitrary points in
    * simulation time (i.e. ticks), which are situated after the tick of the
    * last state.
    *
    * This method is only called if the participant is '''not''' em-controlled.
    * If the participant '''is''' em-controlled,
    * [[ParticipantFlexibility.handlePowerControl()]] determines the operating
    * point instead.
    *
    * @param state
    *   the current state
    * @param relevantData
    *   the relevant data for the current tick
    * @return
    *   the operating point and optionally a next activation tick
    */
  def determineOperatingPoint(state: S, relevantData: OR): (OP, Option[Long])

  def zeroPowerOperatingPoint: OP

  /** Determines the current state given the last state and the operating point
    * that has been valid from the last state up until now.
    *
    * @param lastState
    *   the last state
    * @param operatingPoint
    *   the operating point valid from the simulation time of the last state up
    *   until now
    * @param currentTick
    *   the current tick
    * @return
    *   the current state
    */
  def determineState(lastState: S, operatingPoint: OP, currentTick: Long): S

  /** @param state
    *   the current state
    * @param lastOperatingPoint
    *   the last operating point before the current one, i.e. the one valid up
    *   until the last state, if applicable
    * @param currentOperatingPoint
    *   the operating point valid from the simulation time of the last state up
    *   until now
    * @param complexPower
    *   the total complex power derived from the current operating point
    * @param dateTime
    *   the current simulation date and time
    * @return
    */
  def createResults(
      state: S,
      lastOperatingPoint: Option[OP],
      currentOperatingPoint: OP,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult]

  def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult

  /** Handling requests that are not part of the standard participant protocol
    *
    * @param state
    *   The current state
    * @param ctx
    *   The actor context that can be used to send replies
    * @param msg
    *   The received request
    * @return
    *   An updated state, or the same state provided as parameter
    */
  def handleRequest(
      state: S,
      ctx: ActorContext[ParticipantAgent.Request],
      msg: ParticipantRequest,
  ): S =
    throw new NotImplementedError(s"Method not implemented by $getClass")

  /** @return
    *   All secondary services required by the model for creating operation
    *   relevant data [[OR]]
    */
  def getRequiredSecondaryServices: Iterable[ServiceType]

  /** @param receivedData
    *   The received primary or secondary data
    * @param nodalVoltage
    *   The voltage at the node that we're connected to
    * @param tick
    *   The current tick
    * @param simulationTime
    *   The current simulation time (matches the tick)
    * @return
    *   The operation relevant date for the current point in simulation time
    * @throws edu.ie3.simona.exceptions.CriticalFailureException
    *   if unexpected type of data was provided
    */
  def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): OR
}

object ParticipantModel {

  trait OperationRelevantData

  /** Passed to model calculation classes for each participant when no secondary
    * data is required
    */
  case object FixedRelevantData extends OperationRelevantData

  /** OperationRelevantData that just transports the current datetime and tick
    * @param tick
    *   The current tick
    * @param dateTime
    *   The current datetime, corresponding to the current tick
    */
  final case class DateTimeData(tick: Long, dateTime: ZonedDateTime)
      extends OperationRelevantData

  trait OperatingPoint {

    val activePower: Power

    /** Reactive power can be overridden by the model itself. If this is None,
      * the active-to-reactive-power function is used.
      */
    val reactivePower: Option[ReactivePower]
  }

  final case class ActivePowerOperatingPoint(override val activePower: Power)
      extends OperatingPoint {
    override val reactivePower: Option[ReactivePower] = None
  }

  object ActivePowerOperatingPoint {
    def zero: ActivePowerOperatingPoint = ActivePowerOperatingPoint(zeroKW)
  }

  trait ModelState {
    val tick: Long
  }

  final case class FixedState(override val tick: Long) extends ModelState

  trait ParticipantFixedState[
      OP <: OperatingPoint,
      OR <: OperationRelevantData,
  ] {
    this: ParticipantModel[OP, FixedState, OR] =>

    override val initialState: Long => FixedState = tick => FixedState(tick)

    override def determineState(
        lastState: FixedState,
        operatingPoint: OP,
        currentTick: Long,
    ): FixedState = FixedState(currentTick)

  }

  /** Indicates when either flex options (when em-controlled) or the operating
    * point are going to change (when not em-controlled).
    *
    * A change of flex options or operating point might occur due to various
    * reasons, including expected data arrival, internal expected model changes
    * and operating interval limits.
    *
    * @param changesAtNextActivation
    *   Indicates whether flex options change at the very next tick that EM is
    *   activated, due to e.g. storage limits being reached. Not applicable for
    *   not-em-controlled models.
    * @param changesAtTick
    *   The next tick at which a change of flex options or the operating point
    *   is expected.
    */
  final case class OperationChangeIndicator(
      changesAtNextActivation: Boolean = false,
      changesAtTick: Option[Long] = None,
  ) {

    /** Combines two [[OperationChangeIndicator]]s by aggregating
      * changesAtNextActivation via OR function and picking the earlier (or any)
      * of both changesAtTick values.
      *
      * @param otherIndicator
      *   The other [[OperationChangeIndicator]] to combine with this one
      * @return
      *   An aggregated [[OperationChangeIndicator]]
      */
    def |(
        otherIndicator: OperationChangeIndicator
    ): OperationChangeIndicator = {
      OperationChangeIndicator(
        changesAtNextActivation || otherIndicator.changesAtNextActivation,
        Seq(changesAtTick, otherIndicator.changesAtTick).flatten.minOption,
      )
    }
  }

}
