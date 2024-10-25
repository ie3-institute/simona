/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import edu.ie3.simona.agent.participant.data.Data.PrimaryData

/** Properties common to all participant agents not yet initialized
  */
trait UninitializedStateData[+PD <: PrimaryData[_]]
    extends ParticipantStateData[PD]
