/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.hp

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.result.system.{
  HpResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
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
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.HpModel
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower}
import squants.{Dimensionless, Each, Power}
import squants.energy.Megawatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}

trait HpAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPowerAndHeat,
      HpRelevantData,
      ParticipantStateData[ApparentPowerAndHeat],
      HpInput,
      HpRuntimeConfig,
      HpModel
    ] {
  this: HpAgent =>
  override protected val pdClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  override val alternativeResult: ApparentPowerAndHeat = ApparentPowerAndHeat(
    Megawatts(0d),
    Megavars(0d),
    Megawatts(0d)
  )

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpRelevantData,
        HpModel
      ],
      Dimensionless
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
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = {
    implicit val startDateTime: ZonedDateTime =
      collectionStateData.baseStateData.startDate

    val voltage =
      getAndCheckNodalVoltage(collectionStateData.baseStateData, currentTick)

    val (result, relevantData) =
      collectionStateData.baseStateData match {
        case modelBaseStateData: ParticipantModelBaseStateData[_, _, _] =>
          modelBaseStateData.model match {
            case hpModel: HpModel =>
              /* extract weather data from secondary data, which should have been requested and received before */
              val weatherData =
                collectionStateData.data
                  .collectFirst {
                    // filter secondary data for weather data
                    case (_, Some(data: WeatherData)) => data
                  }
                  .getOrElse(
                    throw new InconsistentStateException(
                      s"The model ${modelBaseStateData.model} was not provided with needed weather data."
                    )
                  )

              /* Try to get the last calc relevant data. It contains the hp state after the last calculation. If no data
               * can be found, this is the first calculation step and therefore define starting state. */
              val hpState = modelBaseStateData.calcRelevantDateStore.last(
                currentTick
              ) match {
                case Some((_, HpRelevantData(lastState, _, _))) => lastState
                case None =>
                  startingState(hpModel.thermalGrid)
              }

              val relevantData =
                HpRelevantData(
                  hpState,
                  currentTick,
                  weatherData.temp
                )

              val power = hpModel.calculatePower(
                currentTick,
                voltage,
                relevantData
              )

              val accompanyingResults = hpModel.thermalGrid.results(
                relevantData.hpState.thermalGridState
              )(modelBaseStateData.startDate)

              (
                AccompaniedSimulationResult(power, accompanyingResults),
                relevantData
              )
            case _ =>
              throw new InconsistentStateException(
                "Cannot find a model for model calculation."
              )
          }
      }

    updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler,
      collectionStateData.baseStateData,
      result,
      relevantData
    )
  }

  private def startingState(
      thermalGrid: ThermalGrid
  ): HpState = HpState(
    isRunning = false,
    -1,
    Megawatts(0d),
    Megawatts(0d),
    ThermalGrid.startingState(thermalGrid)
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
      outputConfig: NotifierConfig
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPowerAndHeat,
    HpRelevantData,
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
        ParticipantModelBaseStateData[
          ApparentPowerAndHeat,
          HpRelevantData,
          HpModel
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
            resolution * 10,
            Each(
              inputModel.electricalInputModel.getNode
                .getvTarget()
                .to(PU)
                .getValue
                .doubleValue()
            )
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
  ): HpModel = inputModel match {
    case ParticipantStateData.SimpleInputContainer(_) =>
      throw new AgentInitializationException(
        s"Unable to initialize heat pump agent '${inputModel.electricalInputModel.getUuid}' without thermal grid model."
      )
    case WithHeatInputContainer(_, thermalGrid) =>
      /* Build the actual heat pump model */
      HpModel(
        inputModel.electricalInputModel,
        modelConfig.scaling,
        simulationStartDate,
        simulationEndDate,
        ThermalGrid(thermalGrid)
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
        Power => ReactivePower
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
    Quantities.getQuantity(result.p.toKilowatts, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(result.q.toKilovars, PowerSystemUnits.KILOVAR),
    Quantities.getQuantity(result.qDot.toKilowatts, PowerSystemUnits.KILOWATT)
  )
}
