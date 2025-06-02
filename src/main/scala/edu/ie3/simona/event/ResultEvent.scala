/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event

import edu.ie3.datamodel.models.result.{CongestionResult, NodeResult}
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
}
import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult,
}
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  ThermalHouseResult,
  ThermalUnitResult,
}
import edu.ie3.datamodel.models.result.system.{EmResult, HpResult}
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.event.listener.ResultEventListener
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Energy, Power, Temperature}

sealed trait ResultEvent extends Event with ResultEventListener.Request

/** Calculation result events
  */
object ResultEvent {

  /** Event that holds a calculation result of a
    * [[edu.ie3.simona.model.participant.SystemParticipant]]
    *
    * @param systemParticipantResult
    *   the calculation result
    */
  final case class ParticipantResultEvent(
      systemParticipantResult: SystemParticipantResult
  ) extends ResultEvent

  object HpResult {
    def unapply(result: HpResult): Option[
      (
          ZonedDateTime,
          UUID,
          ComparableQuantity[Power],
          ComparableQuantity[Power],
      )
    ] =
      Option(result).map { result =>
        (
          result.getTime,
          result.getInputModel,
          result.getP,
          result.getQ,
        )
      }
  }

  object EmResult {
    def unapply(result: EmResult): Option[
      (
          ZonedDateTime,
          UUID,
          ComparableQuantity[Power],
          ComparableQuantity[Power],
      )
    ] =
      Option(result).map { result =>
        (
          result.getTime,
          result.getInputModel,
          result.getP,
          result.getQ,
        )
      }
  }

  /** Event, that is triggered every time a thermal model has a new result
    * @param thermalResult
    *   Result of the thermal calculation
    */
  final case class ThermalResultEvent(
      thermalResult: ThermalUnitResult
  ) extends ResultEvent

  object ThermalHouseResult {
    def unapply(result: ThermalHouseResult): Option[
      (
          ZonedDateTime,
          UUID,
          ComparableQuantity[Power],
          ComparableQuantity[Temperature],
      )
    ] =
      Option(result).map { result =>
        (
          result.getTime,
          result.getInputModel,
          result.getqDot,
          result.getIndoorTemperature,
        )
      }
  }

  object CylindricalThermalStorageResult {
    def unapply(result: CylindricalStorageResult): Option[
      (
          ZonedDateTime,
          UUID,
          ComparableQuantity[Power],
          ComparableQuantity[Energy],
      )
    ] = {
      Option(result).map { result =>
        (
          result.getTime,
          result.getInputModel,
          result.getqDot,
          result.getEnergy,
        )
      }
    }
  }

  /** Event that holds all grid calculation results of a power flow calculation.
    * The usage of a type is necessary here, to avoid passing in other instances
    * of [[edu.ie3.datamodel.models.result.ResultEntity]] except of the wanted
    * ones
    *
    * @param nodeResults
    *   the power flow node results
    * @param switchResults
    *   the power flow switch results
    * @param lineResults
    *   the power flow line results
    * @param transformer2wResults
    *   the power flow results for two winding transformers
    * @param transformer3wResults
    *   the <b>partial</b> power flow results for three winding transformers
    * @param congestionResults
    *   the congestion found by the congestion managements (default: empty)
    */
  final case class PowerFlowResultEvent(
      nodeResults: Iterable[NodeResult],
      switchResults: Iterable[SwitchResult],
      lineResults: Iterable[LineResult],
      transformer2wResults: Iterable[Transformer2WResult],
      transformer3wResults: Iterable[PartialTransformer3wResult],
      congestionResults: Iterable[CongestionResult] = Iterable.empty,
  ) extends ResultEvent {

    def +(congestionResult: Iterable[CongestionResult]): PowerFlowResultEvent =
      copy(
        congestionResults = congestionResult
      )

  }

  /** Event that holds the flexibility options result of a
    * [[edu.ie3.simona.model.participant.SystemParticipant]]
    *
    * @param flexOptionsResult
    *   the flex options result
    */
  final case class FlexOptionsResultEvent(
      flexOptionsResult: FlexOptionsResult
  ) extends ResultEvent

}
