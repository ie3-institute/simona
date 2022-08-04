/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.EmInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals.RelevantResultValues
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FromOutsideBaseStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage
}
import edu.ie3.util.quantities.{QuantityUtil => PsuQuantityUtil}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait EmAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPowerAndHeat,
      EmRelevantData,
      ParticipantStateData[ApparentPowerAndHeat],
      EmInput,
      EmRuntimeConfig,
      EmModel
    ] {
  this: EmAgent =>
  protected val ApparentPowerAndHeatClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  val alternativeResult: ApparentPowerAndHeat = ApparentPowerAndHeat(
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
  )

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
    EmRelevantData,
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

  /** Checks, if a fast reply is possible, when the very same request (in terms
    * of tick and nodal voltage) already has been answered. Then a Option on
    * stay in the same state with sending an [[AssetPowerUnchangedMessage]] is
    * given back. If a fast reply is not possible, [[None]] is given back.
    * Additionally, the listener are informed about the result.
    *
    * @param baseStateData
    *   Base state data to uApparentPowerAndHeatate
    * @param mostRecentRequest
    *   [[Option]] on a tuple of last request tick and corresponding answer
    * @param requestTick
    *   Tick of the incoming request
    * @param voltageValueStore
    *   [[ValueStore]] with nodal voltages to use in uApparentPowerAndHeatated
    *   state data
    * @param nodalVoltage
    *   Magnitude of the complex, dimensionless nodal voltage
    * @param lastNodalVoltage
    *   Lastly known magnitude of the complex, dimensionless nodal voltage
    * @return
    *   Option on a possible fast state change
    */
  final def determineFastReply(
      baseStateData: BaseStateData[ApparentPowerAndHeat],
      mostRecentRequest: Option[(Long, ApparentPowerAndHeat)],
      requestTick: Long,
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      nodalVoltage: ComparableQuantity[Dimensionless],
      lastNodalVoltage: Option[(Long, ComparableQuantity[Dimensionless])]
  ): Unit = {
    implicit val outputConfig: ParticipantNotifierConfig =
      baseStateData.outputConfig
    mostRecentRequest match {
      case Some((mostRecentRequestTick, latestProvidedValues))
          if mostRecentRequestTick == requestTick =>
        /* A request for this tick has already been answered. Check, if it has been the same request.
         * if it has been the same request we wanna answer with the same values afterwards, this data MUST always
         * be available when we already provided data for this tick */
        baseStateData match {
          case externalBaseStateData: FromOutsideBaseStateData[
                M,
                ApparentPowerAndHeat
              ] =>
          /* When data is provided from outside it is NOT altered depending on the node voltage. Send an
           * AssetPowerUnchangedMessage */
          /* TODO
            Some(
              stay() using externalBaseStateData.copy(
                voltageValueStore = voltageValueStore
              ) replying AssetPowerUnchangedMessage(
                latestProvidedValues.p,
                latestProvidedValues.q
              )
            )*/
          case modelBaseStateData: EmAgentModelBaseStateData =>
            /* Check, if the last request has been made with the same nodal voltage. If not, recalculate the reactive
             * power. */
            lastNodalVoltage match {
              case Some((voltageTick, lastVoltage))
                  if voltageTick == requestTick =>
                if (
                  PsuQuantityUtil.isEquivalentAbs(
                    lastVoltage,
                    nodalVoltage,
                    modelBaseStateData.requestVoltageDeviationThreshold
                  )
                ) {
                  /* This is the very same request (same tick and same nodal voltage). Reply with
                   * AssetPowerUnchangedMessage */
                  Some(
                    stay() using modelBaseStateData replying AssetPowerUnchangedMessage(
                      latestProvidedValues.p,
                      latestProvidedValues.q
                    )
                  )
                } else {
                  /* If the voltage is not exactly equal, continue to determine the correct reply. */
                  None
                }
              case _ =>
                throw new InconsistentStateException(
                  s"I found an already answered request for tick $requestTick, but no matching nodal voltage."
                )
            }
        }
      case _ =>
        /* No reply for the request tick or no reply at all. Continue to determine the correct reply. */
        None
    }
  }

  /** Determine a reply on a
    * [[edu.ie3.simona.ontology.messages.PowerMessage.RequestAssetPowerMessage]]
    * by looking up the detailed simulation results, averaging them and
    * returning the equivalent state transition.
    *
    * @param requestTick
    *   The tick, the request belongs to
    * @param baseStateData
    *   Base state data
    * @param mostRecentRequest
    *   The request reply, that most recently has been sent
    * @param nodalVoltage
    *   Current nodal voltage
    * @param uApparentPowerAndHeatatedVoltageValueStore
    *   Value store with uApparentPowerAndHeatated nodal voltages
    * @param alternativeResult
    *   Alternative result to use, if no reasonable result can be obtained
    * @return
    *   Matching state transition
    */
  def determineReply(
      requestTick: Long,
      baseStateData: BaseStateData[ApparentPowerAndHeat],
      mostRecentRequest: Option[(Long, ApparentPowerAndHeat)],
      nodalVoltage: ComparableQuantity[Dimensionless],
      uApparentPowerAndHeatatedVoltageValueStore: ValueStore[
        ComparableQuantity[Dimensionless]
      ],
      alternativeResult: ApparentPowerAndHeat
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = {
    /* No fast reply possible --> Some calculations have to be made */
    mostRecentRequest match {
      case Some((lastRequestTick, _)) if lastRequestTick > requestTick =>
        throw new InvalidRequestException(
          "Got a request for a tick, whereas a later tick already has been answered. This behaviour is not yet specified!"
        )
      case Some((lastRequestTick, lastResult))
          if lastRequestTick == requestTick =>
        /* Repetitive request for the same tick, but with different voltage */
        baseStateData match {
          case modelBaseStateData: ParticipantModelBaseStateData[
                ApparentPowerAndHeat,
                CD,
                M
              ] =>
            /* Active power is yet calculated, but reactive power needs uApparentPowerAndHeatate */
            val nextReactivePower = modelBaseStateData.model
              .calculateReactivePower(lastResult.p, nodalVoltage)

            /* Determine the reply, based new circumstances */
            val uApparentPowerAndHeatatedRequestValueStore =
              ValueStore.uApparentPowerAndHeatateValueStore(
                baseStateData.requestValueStore,
                requestTick,
                lastResult.withReactivePower(nextReactivePower)
              )
            val nextStateData =
              modelBaseStateData.copy(
                requestValueStore = uApparentPowerAndHeatatedRequestValueStore,
                voltageValueStore = uApparentPowerAndHeatatedVoltageValueStore
              )

            stay() using nextStateData replying AssetPowerChangedMessage(
              lastResult.p,
              nextReactivePower
            )
          case unexpectedStateData =>
            throw new IllegalStateException(
              s"The request reply should not be re-calculated for state data '$unexpectedStateData'"
            )
        }

      case _ =>
        /* There hasn't been a request for this tick, yet. Check, if there are simulation results. If at least one
         * is apparent, average them and answer the request. If no simulation results is apparent at all, reply with
         * zero power, although this case should have been handled earlier */
        getRelevantResultData(
          requestTick,
          baseStateData.resultValueStore,
          baseStateData.requestValueStore
        ) match {
          case Some(relevantData) =>
            /* There is at least one relevant simulation result apparent, which might also be the most recent one
             * before the last request. But this is still the most recent one considered being valid. */
            averagePowerAndStay(
              baseStateData,
              relevantData,
              requestTick,
              nodalVoltage,
              uApparentPowerAndHeatatedVoltageValueStore,
              alternativeResult
            )
          case None =>
            /* There is no simulation result at all. Reply with zero power */
            stayWithUApparentPowerAndHeatatedRequestValueStore(
              baseStateData,
              alternativeResult,
              requestTick,
              uApparentPowerAndHeatatedVoltageValueStore
            )
        }
    }
  }

  /** Determine the relevant simulation results to calculate the average of
    * those, to answer a given request. The relevant results are those between
    * the most recent result tick BEFORE or at the last request and the most
    * recent result tick before or at this request tick. If no new information
    * are apparent, the last known simulation result (before the last answered
    * request) is taken, as this is the only still valid information.
    *
    * @param requestTick
    *   Tick, the current request belongs to
    * @param resultValueStore
    *   Storage of simulation results
    * @param requestValueStore
    *   Storage of already answered requests
    * @return
    *   An optional mapping from tick to simulation results, if some are
    *   apparent within the tick window to be considered.
    */
  def getRelevantResultData(
      requestTick: Long,
      resultValueStore: ValueStore[ApparentPowerAndHeat],
      requestValueStore: ValueStore[ApparentPowerAndHeat]
  ): Option[RelevantResultValues[ApparentPowerAndHeat]] = {
    /* The actual tick window for averaging is the last request tick and this request tick (both including) */
    val (averagingWindowStart, averagingWindowEnd) =
      determineTickWindow(requestTick, requestValueStore)

    /* All participants simulation results between the most recent simulation tick BEFORE or at the beginning of the
     * averaging window and it's end (both including) are relevant for averaging the simulated primary data */
    val firstRelevantTick = determineFirstRelevantTick(
      averagingWindowStart,
      resultValueStore
    )

    /* Let's see, if we got some simulation results between the first relevant simulation tick and this request's tick */
    val simulationResults =
      resultValueStore.get(firstRelevantTick, averagingWindowEnd)
    if (simulationResults.nonEmpty) {
      /* There is at least one simulation result. This might as well be the last simulation result before the last
       * request reply, which still is considered to be the last valid simulation result. */
      Some(
        RelevantResultValues(
          averagingWindowStart,
          averagingWindowEnd,
          simulationResults
        )
      )
    } else {
      None
    }
  }

  /** Determine the averaging tick window. It starts with the last request tick
    * and ends with this request tick.
    *
    * @param requestTick
    *   Tick, for which the averaged primary data has to be determined
    * @param requestValueStore
    *   Storage of already answered requests
    * @return
    *   Window between the last request or simulation start and this request
    */
  private def determineTickWindow(
      requestTick: Long,
      requestValueStore: ValueStore[_]
  ): (Long, Long) =
    requestValueStore.lastKnownTick(requestTick - 1) match {
      case Some(lastRequestTick) => (lastRequestTick, requestTick)
      case None                  => (0, requestTick)
    }

  /** Determines the first tick, that is relevant for building the average
    * primary data to answer a request at the given tick. This is either the
    * last known data tick before or at the last request tick or the beginning
    * of the simulation.
    *
    * @param lastRequestTick
    *   Last tick, at which a request has been answered
    * @param resultValueStore
    *   Storage for the simulation results
    * @return
    *   The first relevant tick for averaging primary data
    */
  private def determineFirstRelevantTick(
      lastRequestTick: Long,
      resultValueStore: ValueStore[_]
  ): Long =
    resultValueStore.lastKnownTick(lastRequestTick) match {
      case Some(firstRelevantDataTick) => firstRelevantDataTick
      case None                        => 0
    }

  /** Average the given power values, answer with the equivalent reply and stay
    * in this state.
    *
    * @param baseData
    *   Current agent's base state data
    * @param relevantResults
    *   Collection of all relevant simulation results, including the needed tick
    *   window
    * @param requestTick
    *   Tick in which power has been requested
    * @param nodalVoltage
    *   Nodal voltage magnitude in the moment of request
    * @param voltageValueStore
    *   Voltage value store to be used in the uApparentPowerAndHeatated base
    *   state data
    * @param alternativeResult
    *   If no relevant data are apparent, then use this result instead
    * @return
    *   The very same state as the agent currently is in, but with
    *   uApparentPowerAndHeatated base state data
    */
  final def averagePowerAndStay(
      baseData: BaseStateData[ApparentPowerAndHeat],
      relevantResults: RelevantResultValues[ApparentPowerAndHeat],
      requestTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless],
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      alternativeResult: ApparentPowerAndHeat
  ): Unit = {
    if (relevantResults.relevantData.nonEmpty) {
      averagePowerAndStay(
        baseData,
        relevantResults.relevantData,
        requestTick,
        relevantResults.windowStart,
        relevantResults.windowEnd,
        nodalVoltage,
        voltageValueStore
      )
    } else {
      log.debug(
        s"No relevant data apparent, stay and reply with alternative result {}.",
        alternativeResult
      )
      stayWithUApparentPowerAndHeatatedRequestValueStore(
        baseData,
        alternativeResult,
        requestTick,
        voltageValueStore
      )
    }
  }

  /** Determines the reply result by averaging the given power values over the
    * queried tick frame. The next state is the very same and the agent replies
    * to the request with the freshly determined averaged power. The reply is
    * also added to the requestValueStore.
    *
    * @param baseData
    *   Current agent's base state data
    * @param tickToResult
    *   Mapping from data tick to actual data
    * @param requestTick
    *   Tick in which power has been requested
    * @param windowStartTick
    *   Beginning of the averaging window
    * @param windowEndTick
    *   End of the averaging window
    * @param nodalVoltage
    *   Nodal voltage magnitude in the moment of request
    * @param voltageValueStore
    *   Voltage value store to be used in the uApparentPowerAndHeatated base
    *   state data
    * @return
    *   The very same state as the agent currently is in, but with
    *   uApparentPowerAndHeatated base state data
    */
  def averagePowerAndStay(
      baseData: BaseStateData[ApparentPowerAndHeat],
      tickToResult: Map[Long, ApparentPowerAndHeat],
      requestTick: Long,
      windowStartTick: Long,
      windowEndTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless],
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]]
  ): Unit = {
    val averageResult = determineAverageResult(
      baseData,
      tickToResult,
      windowStartTick,
      windowEndTick,
      nodalVoltage
    )
    stayWithUApparentPowerAndHeatatedRequestValueStore(
      baseData,
      averageResult,
      requestTick,
      voltageValueStore
    )
  }

  /** Determines the average result, from which to reply to a request. The
    * values are determined by on averaging the results in the baseStateData
    * result value store within the given tick window
    *
    * @param baseStateData
    *   The [[BaseStateData]] to hold information about current results
    * @param tickToResult
    *   Mapping from result tick to actual result
    * @param windowStartTick
    *   Beginning of the averaging tick window
    * @param windowEndTick
    *   End of the averaging tick window
    * @param nodalVoltage
    *   Current nodal voltage magnitude in p.u.
    * @return
    *   Averaged result
    */
  def determineAverageResult(
      baseStateData: BaseStateData[ApparentPowerAndHeat],
      tickToResult: Map[Long, ApparentPowerAndHeat],
      windowStartTick: Long,
      windowEndTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless]
  ): ApparentPowerAndHeat = {
    /* Determine, how the single model would transfer the active into reactive power */
    val activeToReactivePowerFunction = baseStateData match {
      case _: FromOutsideBaseStateData[EmModel, ApparentPowerAndHeat] => None
      case modelBaseStateData: EmAgentModelBaseStateData =>
        Some(
          modelBaseStateData.model.activeToReactivePowerFunc(nodalVoltage)
        )
    }

    averageResults(
      tickToResult,
      windowStartTick,
      windowEndTick,
      activeToReactivePowerFunction
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
  def averageResults(
      tickToResults: Map[Long, ApparentPowerAndHeat],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ] = None
  ): ApparentPowerAndHeat

}
