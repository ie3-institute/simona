/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.em.EmTools
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelChangeIndicator,
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions,
}
import org.apache.pekko.actor.typed.scaladsl.ActorContext

/** Takes care of:
  *   - activating/deactivating model
  *   - holding id information
  *   - storing:
  *     - states (only current needed)
  *       - operating points (only current needed)
  *       - operation relevant data (only current needed)
  *       - flex options? (only current needed)
  *       - results? also needs to handle power request from grid
  */
final case class ParticipantModelShell[
    OP <: OperatingPoint,
    S <: ModelState,
    OR <: OperationRelevantData,
](
    model: ParticipantModel[OP, S, OR] with ParticipantFlexibility[OP, S, OR],
    state: S,
    relevantData: OR,
    operatingPoint: OP,
    flexOptions: ProvideFlexOptions,
    modelChange: ModelChangeIndicator,
) {

  def updateRelevantData(
      receivedData: Seq[SecondaryData],
      tick: Long,
  ): ParticipantModelShell[OP, S, OR] = {
    val updatedRelevantData = model.createRelevantData(receivedData, tick)

    copy(relevantData = updatedRelevantData)
  }

  def updateOperatingPoint(
      currentTick: Long
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(currentTick)

    val (newOperatingPoint, maybeNextTick) =
      model.determineOperatingPoint(state, relevantData)

    val activePower = newOperatingPoint.activePower

    // todo where store the reactive power?
    val reactivePower = ???

    // todo store results here as well? Or separate module for avg power calculation?
    val results = model.createResults(
      state,
      newOperatingPoint,
      ApparentPower(activePower, reactivePower),
      ???,
    )

    copy(
      state = currentState,
      operatingPoint = newOperatingPoint,
      modelChange = ModelChangeIndicator(changesAtTick = maybeNextTick),
    )
  }

  def updateFlexOptions(currentTick: Long): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(currentTick)
    val flexOptions = model.calcFlexOptions(currentState, relevantData)

    copy(state = currentState, flexOptions = flexOptions)
  }

  def updateOperatingPoint(
      flexControl: IssueFlexControl
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(flexControl.tick)

    val setPointActivePower = EmTools.determineFlexPower(
      flexOptions,
      flexControl,
    )

    val (newOperatingPoint, modelChange) =
      model.handlePowerControl(flexOptions, setPointActivePower)

    copy(
      state = currentState,
      operatingPoint = newOperatingPoint,
      modelChange = modelChange,
    )
  }

  def handleRequest(
      ctx: ActorContext[ParticipantAgent.Request],
      request: ParticipantRequest,
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(request.tick)
    val updatedState = model.handleRequest(currentState, ctx, request)

    copy(state = updatedState)
  }

  private def determineCurrentState(currentTick: Long): S = {
    if (state.tick < currentTick)
      model.determineState(state, operatingPoint, currentTick)
    else
      state
  }

}
