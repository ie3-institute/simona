/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import tech.units.indriya.unit.Units._
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.simona.model.participant.PvModel
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.quantity.Quantities.getQuantity
import squants.Dimensionless
import squants.space.{Degrees, Radians, SquareMeters}
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.scala.quantities.Irradiation
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Megavars, Sq, WattHoursPerSquareMeter}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import squants.energy.Kilowatts

import scala.Option
import squants.space.Angle

import java.time.ZonedDateTime
import java.util.UUID

/**
 * Test class that tries to cover all special cases of the current implementation of the PvModel
 *
 * Some of these test cases are taken from the examples of
 * Duffie, J. A., & Beckman, W. A. (2013). Solar engineering of thermal processes (4th ed.). Hoboken, N.J.: John Wiley & Sons.
 *
 * The page examples can be found using the page number provided in each test. Results may differ slightly from the
 * book as we sometimes use different formulas. Furthermore, sometimes the example might be slightly adapted to fit our needs.
 *
 */

class PvModelSpec extends UnitSpec with DefaultTestData {

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
    11
  )


  val pvInput = new PvInput(
    UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
    "Pv Model Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
    0.20000000298023224,
    getQuantity(-8.926613807678223, DEGREE_GEOM),
    getQuantity(97, PERCENT),
    getQuantity(41.01871871948242, DEGREE_GEOM),
    0.8999999761581421,
    1,
    false,
    getQuantity(10, KILOVOLTAMPERE),
    0.8999999761581421
  )



  val pvModel: PvModel = PvModel(
    pvInput.getUuid,
    pvInput.id,
    OperationInterval(0L, 86400L),
    scalingFactor,
    QControl(pvInput.qCharacteristics),
    Kilowatts(pvInput.getsRated.to(KILOWATT).getValue.doubleValue),
    pvInput.getCosPhiRated,
    Sq(pvInput.node.geoPosition.y, Degrees),
    Sq(pvInput.node.geoPosition.x, Degrees),
    pvInput.albedo,
    Sq(pvInput.etaConv.to(PU).value.doubleValue(), Each),
    Sq(pvInput.azimuth.to(RADIAN).value.doubleValue(), Radians),
    Sq(pvInput.elevationAngle.value.doubleValue(), Radians),
    Sq(1d, SquareMeters)
  )

  def setupSpec(): Unit = {


    // build the PvInputModel


    // build the PvModel
    val scalingFactor = 1.0d

  }


  "A PV Model" should {
    "have sMax set to be 10% higher than its sRated" in {
      val actualSMax = pvModel.sMax.toKilowatts
      val expectedSMax = pvModel.sRated.toKilowatts * 1.1

      actualSMax shouldBe expectedSMax
    }

  }

}

