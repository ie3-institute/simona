/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.event.listener.ResultEventListener.ResultMessage

final case class StopMessage(simulationSuccessful: Boolean)
    extends ResultMessage
