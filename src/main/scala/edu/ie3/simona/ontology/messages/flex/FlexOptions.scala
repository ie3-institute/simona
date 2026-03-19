/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  DisaggregatedFlexOptions as ExtDisaggregatedFlexOptions,
  EnergyBoundariesFlexOptions as ExtEnergyBoundariesFlexOptions,
  FlexOptions as ExtFlexOptions,
  PowerLimitFlexOptions as ExtPowerLimitFlexOptions,
}
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsScala

/** Trait that all flex option types have to extend. */
trait FlexOptions {

  def toExt(recipient: UUID, model: UUID): ExtFlexOptions
}

object FlexOptions {

  type TYPE[FO <: FlexOptions] = FO | DisaggregatedFlexOptions[FO]

}
