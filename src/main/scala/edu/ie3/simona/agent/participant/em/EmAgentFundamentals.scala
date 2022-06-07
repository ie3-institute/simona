/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.{BaseStateData, DataCollectionStateData, ParticipantStateData}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EmModel
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait EmAgentFundamentals
  extends ParticipantAgentFundamentals[
ApparentPowerAndHeat,
EmData,
ParticipantStateData[ApparentPowerAndHeat],
EmInput,
EmRuntimeConfig,
EmModel
] {
  this: EmAgent =>
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
  EmData,
  EmModel
  ],
  ComparableQuantity[Dimensionless]
  ) => ApparentPowerAndHeat = ???

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
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = ???

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
  inputModel: EmInput,
  modelConfig: EmRuntimeConfig,
  simulationStartDate: ZonedDateTime,
  simulationEndDate: ZonedDateTime
  ): EmModel = ???

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  override def determineModelBaseStateData(
  inputModel: EmInput,
  modelConfig: EmRuntimeConfig,
  services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
  simulationStartDate: ZonedDateTime,
  simulationEndDate: ZonedDateTime,
  resolution: Long,
  requestVoltageDeviationThreshold: Double,
  outputConfig: ParticipantNotifierConfig
  ): BaseStateData.ParticipantModelBaseStateData[
  ApparentPowerAndHeat,
  EmData,
  EmModel
  ] = ???

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
  ): ApparentPowerAndHeat = ???

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
  ): SystemParticipantResult = ???
}