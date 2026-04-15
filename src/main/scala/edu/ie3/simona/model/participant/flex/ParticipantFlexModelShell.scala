/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  FlexOptionsExtra,
  FlexType,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.util.TickUtil.TickLong
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Try}

/** A shell allowing interactions with the [[ParticipantFlexModel]] that it
  * holds.
  *
  * @param modelUuid
  *   The UUID of the participant model.
  * @param flexModel
  *   The flexibility model that determines flex options.
  * @param flexType
  *   The flex type of the flexibility model.
  * @param dataTimeType
  *   The data time type of the flex options to create.
  * @param flexOptions
  *   The most recent flex options plus flex type, if they have been calculated
  *   already.
  * @param simulationStart
  *   The date and time at which simulation started.
  * @param identifier
  *   A unique identifier for the model held by this model shell, including the
  *   type, UUID and id of the model, for the purpose of log or exception
  *   messaging.
  * @tparam S
  *   The type of state used by the [[ParticipantFlexModel]].
  */
final case class ParticipantFlexModelShell[
    OP <: OperatingPoint,
    S <: ModelState,
](
    modelUuid: UUID,
    flexModel: ParticipantFlexModel[OP, S],
    flexType: FlexType,
    dataTimeType: DataTimeType,
    flexOptions: Option[FlexOptions] = None,
)(using simulationStart: ZonedDateTime, identifier: String) {

  private lazy val flexOptionsExtra: FlexOptionsExtra[?] =
    FlexOptionsExtra(flexType)

  /** Returns the current flex options, if present, or throws a
    * [[CriticalFailureException]]. Only call this if you are certain the flex
    * options have been set.
    *
    * @return
    *   The flex options.
    */
  def getFlexOptions: FlexOptions =
    flexOptions
      .getOrElse(
        throw new CriticalFailureException(
          s"$identifier: Flex options have not been calculated!"
        )
      )

  /** Determines flex options results for the current flex options, which have
    * to have been calculated before.
    *
    * @param tick
    *   The current tick.
    * @return
    *   The flex options results.
    */
  def determineResult(tick: Long): FlexOptionsResult =
    flexOptionsExtra.createResult(
      flexOptionsExtra.castFlexOptions(getFlexOptions),
      modelUuid,
      tick.toDateTime,
    )

  /** Updates the flex options on basis of the current state.
    *
    * @param state
    *   The current state.
    * @return
    *   An updated [[ParticipantFlexModelShell]].
    */
  def updateFlexOptions(
      state: S,
      inOperation: Boolean,
  ): ParticipantFlexModelShell[OP, S] = {
    val updatedFlexOptions =
      if inOperation then flexModel.determineFlexOptions(state, dataTimeType)
      else
        // Out of operation, there's no way to operate besides 0 kW
        flexOptionsExtra.zero(state.tick)

    copy(flexOptions = Some(updatedFlexOptions))
  }

  def determineNextActivation(
      state: S,
      operatingPoint: OP,
      setPower: Power,
  ): OperationChangeIndicator =
    flexModel.determineNextActivation(
      state,
      operatingPoint,
      setPower,
      dataTimeType,
    )

  /** Determines and returns the set point power, determined by the flex control
    * message.
    *
    * @param flexControl
    *   The flex control message.
    * @return
    *   The active power set point.
    */
  def determineFlexPower(flexControl: IssueFlexControl): Power =
    Try(
      flexOptionsExtra.determineFlexPower(
        flexOptionsExtra.castFlexOptions(getFlexOptions),
        flexControl,
      )
    )
      .recoverWith(exception =>
        Failure(
          new CriticalFailureException(
            s"$identifier: Determining flex power failed",
            exception,
          )
        )
      )
      .get

}
