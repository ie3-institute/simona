/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.CableSetup

trait LineSegmentThermalModelInputData {
  protected val cigreT880LandCable33kV: CableSetup =
    CigreT880LandCable33kV.cable
  protected val andersSingleCore10kV: CableSetup =
    Anders1997SingleCoreCable10kV.cable
  protected val cigreT880SubmarineCable400kV: CableSetup =
    CigreT880SubmarineCable400kV.cable
}
