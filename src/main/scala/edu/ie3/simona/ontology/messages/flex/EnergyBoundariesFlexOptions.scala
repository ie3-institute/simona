/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import squants.time.Seconds
import squants.{Dimensionless, Each, Energy, Power, Time}

import scala.collection.immutable.SortedMap

final case class EnergyBoundariesFlexOptions(
    energyBoundaries: Seq[ParticipantEnergyBoundaries]
) extends FlexOptions

object EnergyBoundariesFlexOptions {

  def apply(
      singleBoundaries: ParticipantEnergyBoundaries
  ): EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(Seq(singleBoundaries))

  /** @param energyLimits
    *   Energy potential upwards and downwards
    * @param powerLimits
    *   If energy limits (upper and lower) are the same, this is ignored for the
    *   relevant time steps.
    * @param etaCharge
    * @param etaDischarge
    * @param tickDisconnect
    */
  final case class ParticipantEnergyBoundaries(
      energyLimits: SortedMap[Long, ClosedInterval[Energy]],
      powerLimits: ClosedInterval[Power],
      etaCharge: Dimensionless = Each(1),
      etaDischarge: Dimensionless = Each(1),
      tickDisconnect: Option[Long] = None,
  )

  object ParticipantEnergyBoundaries {

    /** Creating energy boundaries for a fixed power time series.
      *
      * @param powerSeries
      *   The power time series.
      * @return
      */
    def apply(
        powerSeries: SortedMap[Long, Power]
    ): ParticipantEnergyBoundaries = {

      val (firstTick, firstPower) = powerSeries.headOption.getOrElse(
        throw new CriticalFailureException("Empty power time series!")
      )

      val (energySeries, _) =
        powerSeries.tail.foldLeft(
          (SortedMap(firstTick -> zeroKWh), firstPower)
        ) { case ((previousSeries, lastPower), (tick, power)) =>
          val (lastTick, lastEnergy) = previousSeries
            .maxBefore(tick)
            .getOrElse(
              throw new CriticalFailureException(
                s"No value before $tick in previous values $previousSeries"
              )
            )

          val addedEnergy = lastPower * Seconds(tick - lastTick)
          val tickEnergy = lastEnergy + addedEnergy
          (previousSeries.updated(tick, tickEnergy), power)
        }

      val minPower = powerSeries.values.minOption.getOrElse(zeroKW)
      val maxPower = powerSeries.values.maxOption.getOrElse(zeroKW)

      ParticipantEnergyBoundaries(
        energyLimits = energySeries.map { case (tick, energy) =>
          tick -> ClosedInterval(energy, energy)
        },
        powerLimits = ClosedInterval(minPower, maxPower),
      )
    }

  }

}
