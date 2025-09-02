/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model

import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.datamodel.models.input.system.SystemParticipantInput

sealed trait InputModelContainer[+I <: SystemParticipantInput] {
  val electricalInputModel: I
}

object InputModelContainer {

  final case class SimpleInputContainer[+I <: SystemParticipantInput](
      override val electricalInputModel: I
  ) extends InputModelContainer[I]

  final case class WithHeatInputContainer[+I <: SystemParticipantInput](
      override val electricalInputModel: I,
      thermalGrid: ThermalGrid,
  ) extends InputModelContainer[I]

}
