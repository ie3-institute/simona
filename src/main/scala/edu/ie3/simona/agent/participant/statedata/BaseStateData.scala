/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
  ProvideFlexOptions,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicActorRef}
import squants.Dimensionless

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet

/** Trait to denote the common properties to all basic state data in participant
  * agents
  *
  * @tparam PD
  *   Type of [[PrimaryDataWithApparentPower]], that the represented Participant
  *   produces
  */
trait BaseStateData[+PD <: PrimaryDataWithApparentPower[PD]]
    extends ParticipantStateData[PD] {

  /** The date, that fits the tick 0
    */
  val startDate: ZonedDateTime

  /** The wall clock date, at which the simulation ends
    */
  val endDate: ZonedDateTime

  /** Unique identifier of the simulation model
    */
  val modelUuid: UUID

  /** By default the agent should be triggered in the same tick, where data is
    * incoming from primary or secondary sources. However, if there is no other
    * information needed, we might have the need to schedule ourselves for
    * activation triggers
    */
  val additionalActivationTicks: SortedSet[Long]

  /** A mapping from service reference to it's foreseen next availability of
    * data
    */
  val foreseenDataTicks: Map[ClassicActorRef, Option[Long]]

  /** A store, holding a map from tick to active / reactive power
    */
  val resultValueStore: ValueStore[PD]

  /** A store, holding information of the lastly requested and provided results.
    * The request from the grid always targets at [[ApparentPower]], but for the
    * sake of traceability, the whole averaged result ist stored
    */
  val requestValueStore: ValueStore[PD]

  /** A store holding the last know nodal voltages. The voltage for tick 0 is
    * set to 1.0 p.u. per default. If more information are available, the
    * attribute shall be overridden
    */
  val voltageValueStore: ValueStore[Dimensionless]

  /** Determines the output behaviour of this model
    */
  val outputConfig: NotifierConfig
}

object BaseStateData {

  /** The agent is supposed to carry out model calculations
    *
    * @tparam PD
    *   Type of [[PrimaryDataWithApparentPower]], that the represented
    *   Participant produces
    * @tparam CD
    *   Type of [[CalcRelevantData]], that is required by the included model
    * @tparam M
    *   Restricting the model to a certain class
    */
  trait ModelBaseStateData[
      +PD <: PrimaryDataWithApparentPower[PD],
      CD <: CalcRelevantData,
      MS <: ModelState,
      +M <: SystemParticipant[_ <: CalcRelevantData, PD, MS],
  ] extends BaseStateData[PD] {

    /** The physical system model
      */
    val model: M

    /** The services, the physical model depends on
      */
    val services: Iterable[SecondaryDataService[_ <: SecondaryData]]

    /** Stores all data that are relevant to model calculation
      */
    val receivedSecondaryDataStore: ValueStore[
      Map[ClassicActorRef, _ <: SecondaryData]
    ]

    val stateDataStore: ValueStore[MS]

    val flexStateData: Option[FlexControlledData]

    def isEmManaged: Boolean = flexStateData.nonEmpty
  }

  /** Basic state data, when the agent is supposed to only provide external data
    *
    * @param model
    *   Calculation model of the participant
    * @param startDate
    *   The date, that fits the tick 0
    * @param endDate
    *   The wall clock date, at which the simulation ends
    * @param outputConfig
    *   Determines the output behaviour of this model
    * @param additionalActivationTicks
    *   Additionally trigger actor at these ticks
    * @param foreseenDataTicks
    *   Ticks, at which new data is foreseen from given actors
    * @param fillUpReactivePowerWithModelFunc
    *   If missing reactive power may be filled up with model function
    * @param voltageValueStore
    *   Store holding the last known nodal voltages
    * @param resultValueStore
    *   A store, holding a map from tick to active / reactive power
    * @param requestValueStore
    *   A store, holding the lastly requested and provided results
    * @tparam M
    *   Type of the calculation model to carry
    * @tparam P
    *   Type of primary data, that the model produces
    */
  final case class FromOutsideBaseStateData[M <: SystemParticipant[
    _ <: CalcRelevantData,
    P,
    _,
  ], +P <: PrimaryDataWithApparentPower[P]](
      model: M,
      override val startDate: ZonedDateTime,
      override val endDate: ZonedDateTime,
      override val outputConfig: NotifierConfig,
      override val additionalActivationTicks: SortedSet[Long],
      override val foreseenDataTicks: Map[ClassicActorRef, Option[Long]],
      fillUpReactivePowerWithModelFunc: Boolean = false,
      requestVoltageDeviationThreshold: Double,
      override val voltageValueStore: ValueStore[
        Dimensionless
      ],
      override val resultValueStore: ValueStore[P],
      override val requestValueStore: ValueStore[P],
  ) extends BaseStateData[P] {
    override val modelUuid: UUID = model.getUuid
  }

  /** The agent is supposed to carry out model calculations
    *
    * @param startDate
    *   The date, that fits the tick 0
    * @param endDate
    *   The wall clock date, at which the simulation ends
    * @param model
    *   Physical model of the load
    * @param services
    *   Secondary data services to depend on
    * @param outputConfig
    *   Determines the output behaviour of this model
    * @param additionalActivationTicks
    *   Additionally trigger actor at these ticks
    * @param foreseenDataTicks
    *   Ticks, at which new data is foreseen from given actors
    * @param requestVoltageDeviationThreshold
    *   Threshold , after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param voltageValueStore
    *   Store holding the last known nodal voltages
    * @param resultValueStore
    *   A store, holding a map from tick to active / reactive power
    * @param requestValueStore
    *   A store, holding the lastly requested and provided results
    * @tparam PD
    *   Type of primary data, that is result of model calculation
    * @tparam CD
    *   Type of [[SecondaryData]], that is required by included model
    * @tparam M
    *   Type of model, the base state data is attached to
    */
  final case class ParticipantModelBaseStateData[
      +PD <: PrimaryDataWithApparentPower[PD],
      CD <: CalcRelevantData,
      MS <: ModelState,
      M <: SystemParticipant[_ <: CalcRelevantData, PD, MS],
  ](
      override val startDate: ZonedDateTime,
      override val endDate: ZonedDateTime,
      override val model: M,
      override val services: Iterable[SecondaryDataService[_ <: SecondaryData]],
      override val outputConfig: NotifierConfig,
      override val additionalActivationTicks: SortedSet[Long],
      override val foreseenDataTicks: Map[ClassicActorRef, Option[Long]],
      requestVoltageDeviationThreshold: Double,
      override val voltageValueStore: ValueStore[
        Dimensionless
      ],
      override val resultValueStore: ValueStore[PD],
      override val requestValueStore: ValueStore[PD],
      override val receivedSecondaryDataStore: ValueStore[
        Map[ClassicActorRef, _ <: SecondaryData]
      ],
      override val stateDataStore: ValueStore[MS],
      override val flexStateData: Option[FlexControlledData],
  ) extends ModelBaseStateData[PD, CD, MS, M] {

    /** Unique identifier of the simulation model
      */
    override val modelUuid: UUID = model.getUuid
  }

  /** The existence of this data object indicates that the corresponding agent
    * is EM-controlled (by [[emAgent]]).
    *
    * @param emAgent
    *   The parent EmAgent that is controlling this agent.
    * @param flexAdapter
    *   The flex adapter handling [[FlexRequest]] messages
    * @param lastFlexOptions
    *   Last flex options that have been calculated for this agent.
    */
  final case class FlexControlledData(
      emAgent: ActorRef[FlexResponse],
      flexAdapter: ActorRef[FlexRequest],
      lastFlexOptions: Option[ProvideFlexOptions] = None,
  )

  /** Updates the base state data with the given value stores
    *
    * @param baseStateData
    *   BaseStateData to update
    * @param updatedResultValueStore
    *   Value store with updated results
    * @param updatedRequestValueStore
    *   Value store with updated requests
    * @param updatedVoltageValueStore
    *   Value store with updated voltage information
    * @param updatedAdditionalActivationTicks
    *   Additional activation ticks
    * @param updatedForeseenTicks
    *   Mapping from [[ClassicActorRef]] to foreseen ticks
    * @tparam PD
    *   Type of primary data, that is result of model calculation
    * @return
    *   A copy of the base data with updated value stores
    */
  def updateBaseStateData[PD <: PrimaryDataWithApparentPower[PD]](
      baseStateData: BaseStateData[PD],
      updatedResultValueStore: ValueStore[PD],
      updatedRequestValueStore: ValueStore[PD],
      updatedVoltageValueStore: ValueStore[Dimensionless],
      updatedAdditionalActivationTicks: SortedSet[Long],
      updatedForeseenTicks: Map[ClassicActorRef, Option[Long]],
  ): BaseStateData[PD] = {
    baseStateData match {
      case external: FromOutsideBaseStateData[_, PD] =>
        external.copy(
          resultValueStore = updatedResultValueStore,
          requestValueStore = updatedRequestValueStore,
          voltageValueStore = updatedVoltageValueStore,
          additionalActivationTicks = updatedAdditionalActivationTicks,
          foreseenDataTicks = updatedForeseenTicks,
        )
      case model: ParticipantModelBaseStateData[PD, _, _, _] =>
        model.copy(
          resultValueStore = updatedResultValueStore,
          requestValueStore = updatedRequestValueStore,
          voltageValueStore = updatedVoltageValueStore,
          additionalActivationTicks = updatedAdditionalActivationTicks,
          foreseenDataTicks = updatedForeseenTicks,
        )
    }
  }
}
