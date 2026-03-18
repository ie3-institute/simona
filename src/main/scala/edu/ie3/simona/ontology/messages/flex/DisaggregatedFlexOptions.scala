/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.api.data.model.em.MultiFlexOptions

import java.util.UUID

final case class DisaggregatedFlexOptions[FO <: FlexOptions](
    disaggregated: Map[UUID, FO]
) extends FlexOptions {

  override def toExt(receiver: UUID, model: UUID): MultiFlexOptions = {
    val options = new MultiFlexOptions(receiver)

    disaggregated.foreach { case (disaggregateModelUuid, fo) =>
      options.addDisaggregated(
        disaggregateModelUuid,
        fo.toExt(model, disaggregateModelUuid),
      )
    }

    options
  }

}

object DisaggregatedFlexOptions {

  def apply[FO <: FlexOptions](
      allFlexOptions: Iterable[(UUID, FO)]
  ): DisaggregatedFlexOptions[FO] = DisaggregatedFlexOptions(
    allFlexOptions.toMap
  )
}
