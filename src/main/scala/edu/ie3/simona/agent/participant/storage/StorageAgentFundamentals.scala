/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.storage

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  StorageResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER,
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexControlledData,
  ParticipantModelBaseStateData,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData,
}
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InvalidRequestException,
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.StorageModel.{
  StorageRelevantData,
  StorageState,
}
import edu.ie3.simona.model.participant.{FlexChangeIndicator, StorageModel}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.typed.{ActorRef => TypedActorRef}
import squants.{Dimensionless, Each, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}

trait StorageAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      StorageRelevantData,
      StorageState,
      ParticipantStateData[ApparentPower],
      StorageInput,
      StorageRuntimeConfig,
      StorageModel,
    ] {
  this: StorageAgent =>
  override val alternativeResult: ApparentPower = ZERO_POWER

  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  override def determineModelBaseStateData(
      inputModel: ParticipantStateData.InputModelContainer[StorageInput],
      modelConfig: StorageRuntimeConfig,
      services: Iterable[SecondaryDataService[_ <: Data.SecondaryData]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[TypedActorRef[FlexResponse]],
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPower,
    StorageRelevantData,
    StorageState,
    StorageModel,
  ] = {
    if (maybeEmAgent.isEmpty)
      throw new AgentInitializationException(
        "StorageAgent needs to be EM-controlled."
      )

    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate,
      )

    ParticipantModelBaseStateData[
      ApparentPower,
      StorageRelevantData,
      StorageState,
      StorageModel,
    ](
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      SortedSet.empty,
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution,
        Each(
          inputModel.electricalInputModel.getNode
            .getvTarget()
            .to(PowerSystemUnits.PU)
            .getValue
            .doubleValue
        ),
      ),
      ValueStore(resolution),
      ValueStore(resolution),
      ValueStore(resolution),
      ValueStore(resolution),
      maybeEmAgent.map(
        FlexControlledData(_, self.toTyped[FlexRequest])
      ),
    )
  }

  override def buildModel(
      inputModel: InputModelContainer[StorageInput],
      modelConfig: StorageRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): StorageModel = StorageModel(
    inputModel.electricalInputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate,
    modelConfig.initialSoc,
    modelConfig.targetSoc,
  )

  override protected def createInitialState(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ]
  ): StorageState = StorageState(
    baseStateData.model.eStorage * baseStateData.model.initialSoc,
    zeroKW,
    SimonaConstants.INIT_SIM_TICK,
  )

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ],
      tick: Long,
  ): StorageRelevantData =
    StorageRelevantData(tick)

  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ],
      StorageState,
      Dimensionless,
  ) => ApparentPower =
    (_, _, _, _) =>
      throw new InvalidRequestException(
        "Storage model cannot be run without secondary data."
      )

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ],
      modelState: StorageState,
      currentTick: Long,
      scheduler: ActorRef,
  ): State =
    throw new InvalidRequestException(
      "StorageAgent cannot be used without EM control"
    )

  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        Power => ReactivePower
      ],
  ): ApparentPower = ParticipantAgentFundamentals.averageApparentPower(
    tickToResults,
    windowStart,
    windowEnd,
    activeToReactivePowerFuncOpt,
    log,
  )

  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower,
  ): SystemParticipantResult = new StorageResult(
    dateTime,
    uuid,
    result.p.toMegawatts.asMegaWatt,
    result.q.toMegavars.asMegaVar,
    (-1d).asPercent, // dummy value
  )

  /** Additional actions on a new calculated simulation result. Overridden here
    * because SOC needs to be calculated.
    *
    * @param baseStateData
    *   The base state data
    * @param result
    *   that has been calculated for the current tick
    * @param currentTick
    *   the current tick
    * @return
    *   updated base state data
    */
  override protected def handleCalculatedResult(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ],
      result: AccompaniedSimulationResult[ApparentPower],
      currentTick: Long,
  ): ParticipantModelBaseStateData[
    ApparentPower,
    StorageRelevantData,
    StorageState,
    StorageModel,
  ] = {
    // announce last result to listeners
    if (baseStateData.outputConfig.simulationResultInfo) {
      val uuid = baseStateData.modelUuid
      val dateTime = currentTick.toDateTime(baseStateData.startDate)

      val storedEnergy = baseStateData.stateDataStore
        .get(currentTick)
        .getOrElse(
          throw new IllegalStateException(
            s"State data for current tick $currentTick should be available."
          )
        )
        .storedEnergy

      val soc = Each(
        storedEnergy / baseStateData.model.eStorage
      ).toPercent.asPercent

      val storageResult = new StorageResult(
        dateTime,
        uuid,
        result.primaryData.p.toMegawatts.asMegaWatt,
        result.primaryData.q.toMegavars.asMegaVar,
        soc,
      )

      notifyListener(ParticipantResultEvent(storageResult))
    }

    baseStateData.copy(
      resultValueStore = ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        currentTick,
        result.primaryData,
      )
    )
  }

  /** Handle an active power change by flex control.
    *
    * @param tick
    *   Tick, in which control is issued
    * @param baseStateData
    *   Base state data of the agent
    * @param data
    *   Calculation relevant data
    * @param lastState
    *   Last known model state
    * @param setPower
    *   Setpoint active power
    * @return
    *   Updated model state, a result model and a [[FlexChangeIndicator]]
    */
  override def handleControlledPowerChange(
      tick: Long,
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel,
      ],
      data: StorageRelevantData,
      lastState: StorageState,
      setPower: Power,
  ): (
      StorageState,
      AccompaniedSimulationResult[ApparentPower],
      FlexChangeIndicator,
  ) = {
    val (updatedState, flexChangeIndicator) =
      baseStateData.model.handleControlledPowerChange(data, lastState, setPower)
    // In edge cases, the model does not accept the given set power
    // and returns an adapted value
    val updatedSetPower = updatedState.chargingPower

    val voltage = getAndCheckNodalVoltage(baseStateData, tick)
    val reactivePower = baseStateData.model.calculateReactivePower(
      updatedSetPower,
      voltage,
    )

    val apparentPower = ApparentPower(updatedSetPower, reactivePower)

    val result: AccompaniedSimulationResult[ApparentPower] =
      AccompaniedSimulationResult(apparentPower, Seq.empty[ResultEntity])

    (updatedState, result, flexChangeIndicator)

  }

  /** Update the last known model state with the given external, relevant data
    *
    * @param tick
    *   Tick to update state for
    * @param modelState
    *   Last known model state
    * @param calcRelevantData
    *   Data, relevant for calculation
    * @param nodalVoltage
    *   Current nodal voltage of the agent
    * @param model
    *   Model for calculation
    * @return
    *   The updated state at given tick under consideration of calculation
    *   relevant data
    */
  override protected def updateState(
      tick: Long,
      modelState: StorageState,
      calcRelevantData: StorageRelevantData,
      nodalVoltage: Dimensionless,
      model: StorageModel,
  ): StorageState =
    throw new InvalidRequestException(
      "StorageAgent cannot be used without EM control"
    )

}
