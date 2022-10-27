/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.storage

import akka.actor.ActorRef
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.result.system.{
  StorageResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.StorageModel
import edu.ie3.simona.model.participant.StorageModel.{
  StorageRelevantData,
  StorageState
}
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait StorageAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      StorageRelevantData,
      StorageState,
      ParticipantStateData[ApparentPower],
      StorageInput,
      StorageRuntimeConfig,
      StorageModel
    ] {
  this: StorageAgent =>
  override val alternativeResult: ApparentPower = ZERO_POWER

  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]

  override def determineModelBaseStateData(
      inputModel: StorageInput,
      modelConfig: StorageRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPower,
    StorageRelevantData,
    StorageState,
    StorageModel
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
        simulationEndDate
      )

    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      Array.empty,
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10,
        inputModel.getNode
          .getvTarget()
          .to(PowerSystemUnits.PU)
      ),
      ValueStore.forResult(resolution, 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      maybeEmAgent.map(
        FlexStateData(_, ValueStore(resolution * 10))
      ) // TODO
    )
  }

  override def buildModel(
      inputModel: StorageInput,
      modelConfig: StorageRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): StorageModel = StorageModel(
    inputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate
  )

  override protected def createInitialState(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel
      ]
  ): StorageState = StorageState(
    zeroKWH,
    zeroKW,
    SimonaConstants.INIT_SIM_TICK
  )

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel
      ],
      tick: Long
  ): StorageRelevantData =
    StorageRelevantData(tick)

  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower =
    (_, _, _) =>
      throw new InvalidRequestException(
        "PV model cannot be run without secondary data."
      )

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel
      ],
      currentTick: Long,
      scheduler: ActorRef
  ): State =
    throw new InvalidRequestException(
      "StorageAgent cannot be used without EM control"
    )

  override protected def calculateResult(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageState,
        StorageModel
      ],
      currentTick: Long,
      activePower: ComparableQuantity[Power]
  ): ApparentPower = {
    val voltage = getAndCheckNodalVoltage(baseStateData, currentTick)

    val reactivePower = baseStateData.model match {
      case model: StorageModel =>
        model.calculateReactivePower(
          activePower,
          voltage
        )
    }

    ApparentPower(activePower, reactivePower)
  }

  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ]
  ): ApparentPower = ParticipantAgentFundamentals.averageApparentPower(
    tickToResults,
    windowStart,
    windowEnd,
    activeToReactivePowerFuncOpt,
    log
  )

  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower
  ): SystemParticipantResult = new StorageResult(
    dateTime,
    uuid,
    result.p,
    result.q,
    (-1d).asPercent // dummy value
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
        StorageModel
      ],
      result: ApparentPower,
      currentTick: Long
  ): ParticipantModelBaseStateData[
    ApparentPower,
    StorageRelevantData,
    StorageState,
    StorageModel
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

      val soc = storedEnergy
        .divide(baseStateData.model.eStorage)
        .asType(classOf[Dimensionless])
        .to(Units.PERCENT)

      val storageResult = new StorageResult(
        dateTime,
        uuid,
        result.p,
        result.q,
        soc
      )

      notifyListener(ParticipantResultEvent(storageResult))
    }

    baseStateData.copy(
      resultValueStore = ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        currentTick,
        result
      )
    )
  }

}
