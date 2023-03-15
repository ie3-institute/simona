/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.fixedfeedin

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.result.system.{
  FixedFeedInResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.FixedFeedInModel
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOVARHOUR,
  KILOWATTHOUR,
  MEGAVAR,
  MEGAWATT,
  PU
}
import edu.ie3.util.scala.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy, Power}
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success}

protected trait FixedFeedInAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      FixedRelevantData.type,
      ParticipantStateData[ApparentPower],
      FixedFeedInInput,
      FixedFeedInRuntimeConfig,
      FixedFeedInModel
    ] {
  this: FixedFeedInAgent =>
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]
  override val alternativeResult: ApparentPower = ZERO_POWER

  /** Determines the needed base state data in dependence of the foreseen
    * simulation mode of the agent.
    *
    * @param inputModel
    *   Input model definition
    * @param modelConfig
    *   Configuration of the model
    * @param services
    *   Optional collection of services to register with
    * @param simulationStartDate
    *   Real world time date time, when the simulation starts
    * @param simulationEndDate
    *   Real world time date time, when the simulation ends
    * @param resolution
    *   Agents regular time bin it wants to be triggered e.g one hour
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @return
    *   A child of [[ParticipantModelBaseStateData]] that reflects the behaviour
    *   based on the data source definition
    */
  override def determineModelBaseStateData(
      inputModel: FixedFeedInInput,
      modelConfig: FixedFeedInRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): ParticipantModelBaseStateData[
    ApparentPower,
    FixedRelevantData.type,
    FixedFeedInModel
  ] = {
    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

    /* Go and collect all ticks, in which new data will be available. Also register for
     * services, where needed. */
    val lastTickInSimulation = simulationEndDate.toTick(simulationStartDate)
    val dataTicks =
      /* As participant agents always return their last known operation point on request, it is sufficient
       * to let a fixed fixed in model determine it's operation point on:
       *  1) The first tick of the simulation
       *  2) The tick, it turns on (in time dependent operation)
       *  3) The tick, it turns off (in time dependent operation)
       * Coinciding ticks are summarized and the last tick is removed, as the change in operation status
       * doesn't affect anything then */
      SortedSet[Long](
        SimonaConstants.FIRST_TICK_IN_SIMULATION,
        model.operationInterval.start,
        model.operationInterval.end
      ).filterNot(_ == lastTickInSimulation)

    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      dataTicks,
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution,
        inputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(resolution, 2),
      ValueStore(resolution),
      ValueStore(resolution)
    )
  }

  override def buildModel(
      inputModel: FixedFeedInInput,
      modelConfig: FixedFeedInRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): FixedFeedInModel = FixedFeedInModel(
    inputModel,
    modelConfig,
    simulationStartDate,
    simulationEndDate
  )

  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[
        ApparentPower,
        FixedRelevantData.type,
        FixedFeedInModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower = (
      currentTick: Long,
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        FixedRelevantData.type,
        FixedFeedInModel
      ],
      voltage: ComparableQuantity[Dimensionless]
  ) =>
    baseStateData.model match {
      case fixedModel: FixedFeedInModel =>
        fixedModel.calculatePower(currentTick, voltage, FixedRelevantData)
      case unsupportedModel =>
        throw new InconsistentStateException(
          s"The model $unsupportedModel is not supported!"
        )
    }

  /** Calculate the power output of the participant utilising secondary data.
    * However, it might appear, that not the complete set of secondary data is
    * available for the given tick. This might especially be true, if the actor
    * has been additionally activated. This method thereby has to try and fill
    * up missing data with the last known data, as this is still supposed to be
    * valid. The secondary data therefore is put to the calculation relevant
    * data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p>
    *
    * @param collectionStateData
    *   State data with collected, comprehensive secondary data.
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPower],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    throw new InvalidRequestException(
      "Request to calculate power with secondary data cannot be processed in a fixed feed in agent."
    )

  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged result
    */
  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ] = None
  ): ApparentPower =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log
    )

  /** Determines the correct result.
    *
    * @param uuid
    *   Unique identifier of the physical model
    * @param dateTime
    *   Real world date of the result
    * @param result
    *   The primary data to build a result model for
    * @return
    *   The equivalent event
    */
  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower
  ): SystemParticipantResult =
    new FixedFeedInResult(
      dateTime,
      uuid,
      result.p,
      result.q
    )
}
