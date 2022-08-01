/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.hp

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput
}
import edu.ie3.datamodel.models.result.system.{
  HpResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.hp.HpAgent.neededServices
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  InputModelContainer,
  WithHeatInputContainer
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.HpModel
import edu.ie3.simona.model.participant.HpModel.HpData
import edu.ie3.simona.model.thermal.ThermalHouse
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.reflect.{ClassTag, classTag}

trait HpAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPowerAndHeat,
      HpData,
      ParticipantStateData[ApparentPowerAndHeat],
      HpInput,
      HpRuntimeConfig,
      HpModel
    ] {
  this: HpAgent =>
  override protected val pdClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  override val alternativeResult: ApparentPowerAndHeat = ApparentPowerAndHeat(
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
  )

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpData,
        HpModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPowerAndHeat =
    (_, _, _) =>
      throw new InvalidRequestException(
        "HP model cannot be run without secondary data."
      )

  /** Abstractly calculate the power output of the participant utilising
    * secondary data. However, it might appear, that not the complete set of
    * secondary data is available for the given tick. This might especially be
    * true, if the actor has been additionally activated. This method thereby
    * has to try and fill up missing data with the last known data, as this is
    * still supposed to be valid. The secondary data therefore is put to the
    * calculation relevant data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p> </p>Actual implementation
    * can be found in each participant's fundamentals.</p>
    *
    * @param collectionStateData
    *   State data with collected secondary data.
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPowerAndHeat],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = goto(
    Idle
  )

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  override def determineModelBaseStateData(
      inputModel: InputModelContainer[HpInput],
      modelConfig: HpRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPowerAndHeat,
    HpData,
    HpModel
  ] = {
    if (!services.exists(_.map(_.getClass).containsSlice(neededServices)))
      throw new AgentInitializationException(
        "HpAgent cannot be initialized without its needed services."
      )

    inputModel match {
      case withHeatContainer: WithHeatInputContainer[HpInput] =>
        /* Build the calculation model */
        val model = buildModel(
          withHeatContainer,
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
          Array.emptyLongArray,
          Map.empty,
          requestVoltageDeviationThreshold,
          ValueStore.forVoltage(
            resolution * 10,
            inputModel.electricalInputModel.getNode
              .getvTarget()
              .to(PU)
          ),
          ValueStore.forResult(resolution, 10),
          ValueStore(resolution * 10),
          ValueStore(resolution * 10)
        )
      case unsupported =>
        throw new AgentInitializationException(
          s"HpAgent cannot be initialized with wrong init state data of type '${unsupported.getClass.getName}'. " +
            s"Expected: '${WithHeatInputContainer.getClass.getName}[${classOf[HpInput].getName}]'."
        )
    }
  }

  /** Abstract method to build the calculation model from input
    *
    * @param inputModel
    *   Input model description
    * @param modelConfig
    *   Configuration for the model
    * @param simulationStartDate
    *   Wall clock time of first instant in simulation
    * @param simulationEndDate
    *   Wall clock time of last instant in simulation
    * @return
    */
  override def buildModel(
      inputModel: InputModelContainer[HpInput],
      modelConfig: HpRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): HpModel = {
    val thermalHouseInput = {
      inputModel match {
        case ParticipantStateData.SimpleInputContainer(_) =>
          None
        case WithHeatInputContainer(_, thermalGrid) =>
          /* Build the thermal model */
          thermalGrid.houses().stream().findFirst().asScala
      }
    }.getOrElse {
      new ThermalHouseInput(
        UUID.randomUUID(),
        "Random thermal house",
        new ThermalBusInput(UUID.randomUUID(), "Random thermal bus"),
        Quantities.getQuantity(1d, StandardUnits.THERMAL_TRANSMISSION),
        Quantities.getQuantity(100d, StandardUnits.HEAT_CAPACITY),
        Quantities.getQuantity(21d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(23d, StandardUnits.TEMPERATURE),
        Quantities.getQuantity(19d, StandardUnits.TEMPERATURE)
      )
    }

    /* Build the thermal model */
    val thermalHouse = ThermalHouse(thermalHouseInput)

    /* Build the actual heat pump model */
    HpModel(
      inputModel.electricalInputModel,
      modelConfig.scaling,
      simulationStartDate,
      simulationEndDate,
      thermalHouse
    )
  }

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
      tickToResults: Map[Long, ApparentPowerAndHeat],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ]
  ): ApparentPowerAndHeat =
    ParticipantAgentFundamentals.averageApparentPowerAndHeat(
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
      result: ApparentPowerAndHeat
  ): SystemParticipantResult = new HpResult(
    dateTime,
    uuid,
    result.p,
    result.q,
    result.qDot
  )
}
