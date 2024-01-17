/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import squants.Power

import java.util.UUID

object MinMaxFlexibilityMessage {

  /** Provides flexibility options of a system participant using reference,
    * minimum and maximum power. All powers can be negative, signifying a
    * feed-in
    *
    * @param modelUuid
    *   the uuid of the input model that references the system participant
    * @param referencePower
    *   the active power that the system participant would produce/consume
    *   normally
    * @param minPower
    *   the minimum power that the system participant allows
    * @param maxPower
    *   the maximum power that the system participant allows
    */
  final case class ProvideMinMaxFlexOptions(
      override val modelUuid: UUID,
      referencePower: Power,
      minPower: Power,
      maxPower: Power
  ) extends ProvideFlexOptions {

    def fits(power: Power): Boolean =
      minPower <= power && power <= maxPower
  }

  object ProvideMinMaxFlexOptions {
    def noFlexOption(
        modelUuid: UUID,
        power: Power
    ): ProvideMinMaxFlexOptions =
      ProvideMinMaxFlexOptions(modelUuid, power, power, power)
  }
}
