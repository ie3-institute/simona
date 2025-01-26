/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.connector.Transformer3WResult
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.util.TimeUtil
import squants.electro.Amperes
import squants.space.Degrees
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID

trait ThreeWindingResultTestData {
  val inputModel: UUID = UUID.fromString("40d02538-d8dd-421c-8e68-400f1da170c7")
  val time: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2021-06-07T08:43:00Z")
  val resultA: PartialTransformer3wResult.PortA =
    PartialTransformer3wResult.PortA(
      time,
      inputModel,
      Amperes(1d),
      Degrees(2d),
      -5,
    )
  val resultB: PartialTransformer3wResult.PortB =
    PartialTransformer3wResult.PortB(
      time,
      inputModel,
      Amperes(3d),
      Degrees(4d),
    )
  val resultC: PartialTransformer3wResult.PortC =
    PartialTransformer3wResult.PortC(
      time,
      inputModel,
      Amperes(5d),
      Degrees(6d),
    )
  val expected = new Transformer3WResult(
    time,
    inputModel,
    Quantities.getQuantity(1d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
    Quantities.getQuantity(2d, StandardUnits.ELECTRIC_CURRENT_ANGLE),
    Quantities.getQuantity(3d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
    Quantities.getQuantity(4d, StandardUnits.ELECTRIC_CURRENT_ANGLE),
    Quantities.getQuantity(5d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
    Quantities.getQuantity(6d, StandardUnits.ELECTRIC_CURRENT_ANGLE),
    -5,
  )
}
