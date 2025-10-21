/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.ontology.messages.flex.EnergyLimitFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.util.interval.ClosedInterval
import squants.{Dimensionless, Energy, Power}

import scala.collection.immutable.SortedMap

final case class EnergyLimitFlexOptions(
    energyBoundaries: Seq[ParticipantEnergyBoundaries]
) extends FlexOptions

object EnergyLimitFlexOptions {

  def apply(singleBoundaries: ParticipantEnergyBoundaries) =
    EnergyLimitFlexOptions(Seq(singleBoundaries))

  /** @param energyLimits
    *   Energy potential upwards and downwards
    * @param powerLimits
    * @param etaCharge
    * @param etaDischarge
    * @param tickDisconnect
    */
  final case class ParticipantEnergyBoundaries(
      energyLimits: SortedMap[Long, ClosedInterval[Energy]],
      powerLimits: ClosedInterval[Power],
      etaCharge: Dimensionless,
      etaDischarge: Dimensionless,
      tickDisconnect: Option[Long] = None,
  )

}
