/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsState
import edu.ie3.simona.model.participant.flex.AbstractEnergyBoundariesFlexModel
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.DataTimeType.CurrentAndForecast
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.Energy
import squants.time.Seconds

import scala.collection.immutable.SortedMap

class EvcsEnergyBoundariesFlexModel(private val model: EvcsModel)
    extends AbstractEnergyBoundariesFlexModel[EvcsState] {

  override val hasEnergyFlexibility: Boolean = true

  override def determineFlexOptions(
      state: EvcsState,
      dataTimeType: DataTimeType,
  ): FlexOptions = {
    val (forecastResolution, forecastEnd) = dataTimeType match {
      case CurrentAndForecast(forecastLength, forecastResolution) =>
        (
          forecastResolution.toSeconds.toLong,
          forecastLength.toSeconds.toLong + state.tick,
        )
      case _ =>
        throw new CriticalFailureException(
          s"Unexpected data time type $dataTimeType"
        )
    }

    EnergyBoundariesFlexOptions(
      state.evs.map(
        determineEvFlexOptions(_, state.tick, forecastResolution, forecastEnd)
      )
    )

  }

  def determineEvFlexOptions(
      ev: EvModelWrapper,
      currentTick: Long,
      forecastResolution: Long,
      forecastEnd: Long,
  ): AssetEnergyBoundaries = {

    val maxPower = model.getMaxAvailableChargingPower(ev)
    // discharging is only allowed if V2G is enabled
    val minPower =
      if model.vehicle2grid then -maxPower
      else zeroKW

    val adaptedDisconnectTick: Option[Long] =
      if ev.departureTick > forecastEnd then {
        // departure is after forecast horizon, we thus ignore it
        None
      } else {
        // we have to provide options for at least one step
        // thus we can depart at this tick earliest
        val earliestEnd = currentTick + forecastResolution

        Some(math.max(ev.departureTick, earliestEnd))
      }

    // we want to have at least the lowest SOC amount in storage
    // before disconnecting
    val disconnectingLimits = adaptedDisconnectTick
      .map { disconnectTick =>

        // energy to charge until departure
        val requiredEnergy =
          ev.eStorage * model.departureTargetSoc - ev.storedEnergy
        val timeToDeparture = Seconds(disconnectTick - currentTick)

        // required power to reach the regular lower limit
        val requiredPower = requiredEnergy / timeToDeparture

        // since reaching the lower limit needs to be feasible,
        // we have to adapt the limit if the regular limit is
        // not reachable with max power
        val adaptedRequiredEnergy =
          if requiredPower > maxPower then
            // we cannot reach regular lower limit,
            // thus we do the most we can
            maxPower * timeToDeparture
          else
            // we can reach the regular lower limit
            requiredEnergy

        SortedMap(
          disconnectTick -> new ClosedInterval(
            ev.storedEnergy + adaptedRequiredEnergy,
            ev.eStorage,
          )
        )
      }
      .getOrElse(SortedMap.empty[Long, ClosedInterval[Energy]])

    AssetEnergyBoundaries(
      currentEnergy = ev.storedEnergy,
      energyLimits = SortedMap(
        currentTick -> new ClosedInterval(
          zeroKWh,
          ev.eStorage,
        )
      ) ++ disconnectingLimits,
      powerLimits = ClosedInterval(minPower, maxPower),
      tickDisconnect = adaptedDisconnectTick,
    )
  }
}
