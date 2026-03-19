/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.api.data.model.em.{
  DisaggregatedFlexOptions as ExtDisaggregatedFlexOptions,
  FlexOptions as ExtFlexOptions,
}

import java.util.UUID
import scala.jdk.CollectionConverters.{MapHasAsJava, MapHasAsScala}

final case class DisaggregatedFlexOptions[FO <: FlexOptions](
    disaggregated: Map[UUID, FO]
) extends FlexOptions {

  override def toExt(
      receiver: UUID,
      model: UUID,
  ): ExtDisaggregatedFlexOptions[? <: ExtFlexOptions] = {
    val data = disaggregated.map { case (disaggregateModelUuid, fo) =>
      disaggregateModelUuid -> fo.toExt(model, disaggregateModelUuid)
    }.asJava

    new ExtDisaggregatedFlexOptions(receiver, data)
  }

}

object DisaggregatedFlexOptions {

  def apply[FO <: FlexOptions](
      allFlexOptions: Iterable[(UUID, FO)]
  ): DisaggregatedFlexOptions[FO] = DisaggregatedFlexOptions(
    allFlexOptions.toMap
  )

  def apply[F <: ExtFlexOptions, FO <: FlexOptions](
      extOptions: ExtDisaggregatedFlexOptions[F]
  )(using conversion: F => FO): DisaggregatedFlexOptions[FO] = {
    val convertedFlexOptions = extOptions.disaggregated.asScala.map {
      case (uuid, fo) =>
        uuid -> conversion(fo)
    }.toMap

    DisaggregatedFlexOptions(convertedFlexOptions)
  }
}
