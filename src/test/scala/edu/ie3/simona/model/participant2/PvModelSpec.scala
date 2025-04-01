/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.config.RuntimeConfig.PvRuntimeConfig
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import org.scalatest.GivenWhenThen
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units._

import java.util.UUID

class PvModelSpec extends UnitSpec with GivenWhenThen with DefaultTestData {

  // build the NodeInputModel (which defines the location of the pv input model)
  // the NodeInputModel needs a GeoReference for the Pv to work
  val geometryFactory = new GeometryFactory()
  val p: Point = geometryFactory.createPoint(new Coordinate(13.2491, 53.457909))
  val nodeInput = new NodeInput(
    UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
    "NodeInputModel for PvModel Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    getQuantity(1, PU),
    false,
    p,
    GermanVoltageLevelUtils.MV_20KV,
    11,
  )

  // build the PvInputModel
  val pvInput = new PvInput(
    UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
    "Pv Model Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited,
    nodeInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
    null,
    0.20000000298023224,
    getQuantity(-8.926613807678223, DEGREE_GEOM),
    getQuantity(97, PERCENT),
    getQuantity(41.01871871948242, DEGREE_GEOM),
    0.8999999761581421,
    1,
    false,
    getQuantity(10, KILOVOLTAMPERE),
    0.8999999761581421,
  )

  // build the PvModel
  val pvModel: PvModel = PvModel.create(pvInput, PvRuntimeConfig())

  private implicit val apparentPowerTolerance: ApparentPower =
    Kilovoltamperes(1e-10)

  "A PV Model" should {

    "have sMax set to be 10% higher than its sRated" in {
      When("sMax is calculated")
      val actualSMax = pvModel.sMax
      val expectedSMax = pvModel.sRated * 1.1

      Then("result should match the test data")
      actualSMax should approximate(expectedSMax)
    }
  }
}
