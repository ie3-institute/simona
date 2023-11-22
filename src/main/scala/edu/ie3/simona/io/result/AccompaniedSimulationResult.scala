/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower

/** A class to offer means to transport accompanying results alongside of
  * [[PrimaryDataWithApparentPower]], e.g. heat results obtained during a
  * simulation
  * @param primaryData
  *   The original primary data of the electrical asset
  * @tparam PD
  *   Type of primary data, that is carried
  */
final case class AccompaniedSimulationResult[PD <: PrimaryDataWithApparentPower[
  PD
]](primaryData: PD, accompanyingResults: Seq[ResultEntity] = Seq.empty)
