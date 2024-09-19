/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2
import edu.ie3.simona.agent.em.FlexCorrespondenceStore.WithTime
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import org.apache.pekko.actor.typed.javadsl.ActorContext

/** Takes care of:
  *   - activating/deactivating model
  *   - holding id information
  *     - storing:
  *       - states (only current needed)
  *         - operating points (only current needed)
  *         - operation relevant data (only current needed)
  *         - flex options? (only current needed)
  *         - results? also needs to handle power request from grid
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
) {

  def determineRelevantData(receivedData: Seq[SecondaryData], tick: Long) = {
    model.createRelevantData(receivedData, tick)
  }

  private def determineCurrentState(currentTick: Long): S = {
    if (state.tick < currentTick)
      model.determineState(state, operatingPoint, currentTick)
    else
      state
  }

  def determineOperatingPoint(
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

    copy(state = currentState, operatingPoint = newOperatingPoint)
  }

  def calcFlexOptions(): ParticipantModelShell[OP, S, OR] = {
    val flexOptions = model.calcFlexOptions(state, relevantData)

    copy(flexOptions = flexOptions)
  }

  def handleFlexControl(): ParticipantModelShell[OP, S, OR] = {
    // todo pretty similar to determineOperatingPoint

    this
  }

  def handleRequest(
      ctx: ActorContext[ParticipantAgent.Request],
      msg: ParticipantRequest,
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(msg.tick)
    val updatedState = model.handleRequest(currentState, ctx, msg)

    copy(state = updatedState)
  }

}
