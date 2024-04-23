/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.result

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.{Dimensionless, ElectricCurrent}

trait ResultMokka extends MockitoSugar {

  protected def mockNodeResult(
      uuid: UUID,
      vMag: ComparableQuantity[Dimensionless],
  ): NodeResult = {
    val result = mock[NodeResult]
    when(result.getInputModel).thenReturn(uuid)
    when(result.getvMag()).thenReturn(vMag)

    result
  }

  protected def mockLineResult(
      uuid: UUID,
      iAMag: ComparableQuantity[ElectricCurrent],
      iBMag: ComparableQuantity[ElectricCurrent],
  ): LineResult = {
    val result = mock[LineResult]
    when(result.getInputModel).thenReturn(uuid)
    when(result.getiAMag()).thenReturn(iAMag)
    when(result.getiBMag()).thenReturn(iBMag)
    result
  }
}
