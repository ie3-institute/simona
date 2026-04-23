/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.api.data.model.em.FlexOptions as ExtFlexOptions

import java.util.UUID

/** Trait that all flex option types have to extend. */
trait FlexOptions {

  def toExt(recipient: UUID, model: UUID): ExtFlexOptions
}
