/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.api.data.model.em
import edu.ie3.simona.api.data.model.em.{
  MultiFlexOptions,
  EnergyBoundariesFlexOptions as ExtEnergyBoundariesFlexOptions,
  FlexOptions as ExtFlexOptions,
  PowerLimitFlexOptions as ExtPowerLimitFlexOptions,
}
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants

import scala.jdk.CollectionConverters.MapHasAsScala
import java.util.UUID

/** Trait that all flex option types have to extend. */
trait FlexOptions {

  def toExt(recipient: UUID, model: UUID): ExtFlexOptions
}

object FlexOptions {
  type TYPE[FO <: FlexOptions] = FO | DisaggregatedFlexOptions[FO]

  def fromExt(externalFlexOptions: ExtFlexOptions): FlexOptions =
    externalFlexOptions match {
      case options: ExtPowerLimitFlexOptions =>
        PowerLimitFlexOptions(
          options.pRef.toSquants,
          options.pMin.toSquants,
          options.pMax.toSquants,
        )

      case options: MultiFlexOptions =>
        val convertedFlexOptions = options.disaggregated.asScala.map {
          case (uuid, fo) =>
            uuid -> fromExt(fo)
        }.toMap

        DisaggregatedFlexOptions(convertedFlexOptions)

      case other =>
        throw FlexException(s"Cannot convert flex option: $other")
    }

}
