/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result.plain

import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.simona.io.result.plain.PlainResult.PlainNodeResult
import edu.ie3.simona.io.result.plain.PlainWriter.NodeResultWriter
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.GivenWhenThen
import tech.units.indriya.quantity.Quantities

import java.time.format.DateTimeFormatter
import java.util.UUID

class PlainWriterSpec extends UnitSpec with GivenWhenThen {

  "A NodeResultWriter" should {
    val simRunId = UUID.randomUUID()
    val plainWriter = NodeResultWriter(simRunId)

    val timeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

    "should write a plain result correctly" in {
      Given("a full NodeResult")
      val time = TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
      val inputModelId = UUID.randomUUID()
      val vMag = Quantities.getQuantity(0.85d, PowerSystemUnits.PU)
      val vAng = Quantities.getQuantity(90d, PowerSystemUnits.DEGREE_GEOM)

      val nodeResultFull = new NodeResult(
        time,
        inputModelId,
        vMag,
        vAng,
      )

      When("converting to a plain result")
      val plainResult = plainWriter.writePlain(nodeResultFull)

      Then("plain result is correct")
      plainResult.time shouldBe time.format(timeFormatter)
      plainResult.inputModel shouldBe inputModelId
      plainResult.vMag shouldBe vMag
        .to(PowerSystemUnits.PU)
        .getValue
        .doubleValue()
      plainResult.vAng shouldBe vAng
        .to(PowerSystemUnits.DEGREE_GEOM)
        .getValue
        .doubleValue()
    }

    "should write a full result correctly" in {
      Given("a plain NodeResult")
      val time = "2020-01-01T00:00:00Z"
      val inputModelId = UUID.randomUUID()
      val vMag = 0.85d
      val vAng = 90d

      val nodeResultPlain = PlainNodeResult(
        simRunId,
        time,
        inputModelId,
        vMag,
        vAng,
      )

      When("converting to a full NodeResult")
      val plainResult = plainWriter.createFull(nodeResultPlain)

      Then("plain result is correct")
      plainResult.getTime shouldBe TimeUtil.withDefaults.toZonedDateTime(time)
      plainResult.getInputModel shouldBe inputModelId
      plainResult
        .getvMag() shouldBe Quantities.getQuantity(vMag, PowerSystemUnits.PU)
      plainResult.getvAng() shouldBe Quantities.getQuantity(
        vAng,
        PowerSystemUnits.DEGREE_GEOM,
      )
    }
  }
}
