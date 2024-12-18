/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.services

import edu.ie3.simona.ontology.messages.services.ServiceMessage.ServiceInternal

sealed trait WholeSalePriceMessage extends ServiceInternal

object WholeSalePriceMessage {
  final class Dummy extends WholeSalePriceMessage
}
