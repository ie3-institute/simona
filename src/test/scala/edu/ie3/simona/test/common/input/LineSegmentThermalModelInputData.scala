/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.{CableSetup, LineSegmentThermalModel}
import edu.ie3.simona.test.common.DefaultTestData

trait LineSegmentThermalModelInputData extends DefaultTestData {
  protected val cigreT880LandCable33kVcableSetup: CableSetup =
    CigreT880LandCable33kV.cable
  protected val andersSingleCore10kVcableSetup: CableSetup =
    Anders1997SingleCoreCable10kV.cable
  protected val cigreLandCable33kVlineSegmentThermalModel
      : LineSegmentThermalModel =
    CigreT880LandCable33kV.model
  protected val andersLineSegmentThermalModel: LineSegmentThermalModel =
    Anders1997SingleCoreCable10kV.model
}
