/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.{
  LoadResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.load.FixedLoadModel.FixedLoadRelevantData
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.ProfileRelevantData
import edu.ie3.simona.model.participant.load.profile.{
  LoadProfileStore,
  ProfileLoadModel
}
import edu.ie3.simona.model.participant.load.random.RandomLoadModel.RandomRelevantData
import edu.ie3.simona.model.participant.load.random.{
  RandomLoadModel,
  RandomLoadParamStore
}
import edu.ie3.simona.model.participant.load.{
  FixedLoadModel,
  LoadModel,
  LoadReference
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}

protected trait LoadAgentFundamentals[LD <: LoadRelevantData, LM <: LoadModel[
  LD
]] extends ParticipantAgentFundamentals[
      ApparentPower,
      LD,
      ParticipantStateData[ApparentPower],
      LoadInput,
      LoadRuntimeConfig,
      LM
    ] {
  this: LoadAgent[LD, LM] =>
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
    *   Config of the output behaviour for simulation results
    * @return
    *   A child of [[ParticipantModelBaseStateData]] that reflects the behaviour
    *   based on the data source definition
    */
  override def determineModelBaseStateData(
      inputModel: LoadInput,
      modelConfig: LoadRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): ParticipantModelBaseStateData[ApparentPower, LD, LM] = {
    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

    /* Go and collect all ticks, in which activation is needed in addition to the activations made by incoming data.
     * Also register for services, where needed. */
    val lastTickInSimulation = simulationEndDate.toTick(simulationStartDate)
    val additionalActivationTicks = model match {
      /* If no secondary data is needed (implicitly by fixed load model), add activation ticks for the simple model */
      case fixedLoadModel: FixedLoadModel =>
        /* As participant agents always return their last known operation point on request, it is sufficient
         * to let a fixed load model determine it's operation point on:
         *  1) The first tick of the simulation
         *  2) The tick, it turns on (in time dependent operation)
         *  3) The tick, it turns off (in time dependent operation)
         * Coinciding ticks are summarized and the last tick is removed, as the change in operation status
         * doesn't affect anything then */
        SortedSet[Long](
          SimonaConstants.FIRST_TICK_IN_SIMULATION,
          fixedLoadModel.operationInterval.start,
          fixedLoadModel.operationInterval.end
        ).filterNot(_ == lastTickInSimulation)
      case profileLoadModel: ProfileLoadModel =>
        activationTicksInOperationTime(
          simulationStartDate,
          LoadProfileStore.resolution.getSeconds,
          profileLoadModel.operationInterval.start,
          profileLoadModel.operationInterval.end
        )
      case randomLoadModel: RandomLoadModel =>
        activationTicksInOperationTime(
          simulationStartDate,
          RandomLoadParamStore.resolution.getSeconds,
          randomLoadModel.operationInterval.start,
          randomLoadModel.operationInterval.end
        )
      case _ =>
        SortedSet.empty[Long]
    }

    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      additionalActivationTicks,
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
      inputModel: LoadInput,
      modelConfig: LoadRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): LM = {
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )
    val reference = LoadReference(inputModel, modelConfig)
    buildModel(inputModel, operationInterval, reference)
  }

  protected def buildModel(
      inputModel: LoadInput,
      operationInterval: OperationInterval,
      reference: LoadReference
  ): LM

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
    throw new InconsistentStateException(
      s"Load model is not able to calculate power with secondary data."
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
    new LoadResult(
      dateTime,
      uuid,
      result.p,
      result.q
    )
}

case object LoadAgentFundamentals {
  trait FixedLoadAgentFundamentals
      extends LoadAgentFundamentals[
        FixedLoadModel.FixedLoadRelevantData.type,
        FixedLoadModel
      ] {
    this: LoadAgent.FixedLoadAgent =>

    override def buildModel(
        inputModel: LoadInput,
        operationInterval: OperationInterval,
        reference: LoadReference
    ): FixedLoadModel = {
      val model = FixedLoadModel(inputModel, operationInterval, 1d, reference)
      model.enable()
      model
    }

    /** Partial function, that is able to transfer
      * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
      * into a pair of active and reactive power
      */
    override val calculateModelPowerFunc: (
        Long,
        ParticipantModelBaseStateData[
          ApparentPower,
          FixedLoadRelevantData.type,
          FixedLoadModel
        ],
        ComparableQuantity[Dimensionless]
    ) => ApparentPower = (
        tick: Long,
        baseStateData: ParticipantModelBaseStateData[
          ApparentPower,
          FixedLoadRelevantData.type,
          FixedLoadModel
        ],
        voltage: ComparableQuantity[Dimensionless]
    ) =>
      baseStateData.model.calculatePower(tick, voltage, FixedLoadRelevantData)
  }

  trait ProfileLoadAgentFundamentals
      extends LoadAgentFundamentals[
        ProfileRelevantData,
        ProfileLoadModel
      ] {
    this: LoadAgent.ProfileLoadAgent =>

    override def buildModel(
        inputModel: LoadInput,
        operationInterval: OperationInterval,
        reference: LoadReference
    ): ProfileLoadModel = {
      val model = ProfileLoadModel(inputModel, operationInterval, 1d, reference)
      model.enable()
      model
    }

    /** Partial function, that is able to transfer
      * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
      * into a pair of active and reactive power
      */
    override val calculateModelPowerFunc: (
        Long,
        ParticipantModelBaseStateData[
          ApparentPower,
          ProfileRelevantData,
          ProfileLoadModel
        ],
        ComparableQuantity[Dimensionless]
    ) => ApparentPower = (tick, baseStateData, voltage) => {
      val profileLoadModel = baseStateData.model
      val profileRelevantData = ProfileRelevantData(
        tick.toDateTime(baseStateData.startDate)
      )
      profileLoadModel.calculatePower(currentTick, voltage, profileRelevantData)
    }
  }

  trait RandomLoadAgentFundamentals
      extends LoadAgentFundamentals[
        RandomRelevantData,
        RandomLoadModel
      ] {
    this: LoadAgent.RandomLoadAgent =>

    override def buildModel(
        inputModel: LoadInput,
        operationInterval: OperationInterval,
        reference: LoadReference
    ): RandomLoadModel = {
      val model = RandomLoadModel(inputModel, operationInterval, 1d, reference)
      model.enable()
      model
    }

    /** Partial function, that is able to transfer
      * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
      * into a pair of active and reactive power
      */
    override val calculateModelPowerFunc: (
        Long,
        ParticipantModelBaseStateData[
          ApparentPower,
          RandomRelevantData,
          RandomLoadModel
        ],
        ComparableQuantity[Dimensionless]
    ) => ApparentPower = (tick, baseStateData, voltage) => {
      val randomLoadModel = baseStateData.model
      val profileRelevantData = RandomRelevantData(
        tick.toDateTime(baseStateData.startDate)
      )
      randomLoadModel.calculatePower(currentTick, voltage, profileRelevantData)
    }
  }
}
