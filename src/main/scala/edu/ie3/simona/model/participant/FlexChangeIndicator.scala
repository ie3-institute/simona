/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

final case class FlexChangeIndicator(
    changesAtNextActivation: Boolean = false,
    changesAtTick: Option[Long] = None,
)
