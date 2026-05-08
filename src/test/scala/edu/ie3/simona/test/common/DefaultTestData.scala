/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.util.scala.OperationInterval
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import squants.electro.Kilovolts
import squants.energy.Kilowatts

import java.time.{ZoneId, ZonedDateTime}

/** Default values to be used in tests. Should be extended as needed.
  */
trait DefaultTestData {

  // Default start and end date of operation for building input models
  protected val defaultSimulationStart: ZonedDateTime =
    ZonedDateTime.of(2019, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC"))
  protected val defaultSimulationEnd: ZonedDateTime =
    ZonedDateTime.of(2019, 12, 31, 0, 0, 0, 0, ZoneId.of("UTC"))

  private val operationTimeBuilder = OperationTime.builder()
  operationTimeBuilder.withStart(defaultSimulationStart)
  operationTimeBuilder.withEnd(defaultSimulationEnd)
  protected val defaultOperationTime: OperationTime =
    operationTimeBuilder.build()

  operationTimeBuilder.withStart(defaultSimulationStart.withHour(1))
  operationTimeBuilder.withEnd(defaultSimulationEnd)
  protected val postponedOperationTime: OperationTime =
    operationTimeBuilder.build()

  protected val defaultOperationInterval: OperationInterval =
    SystemComponent.determineOperationInterval(
      defaultSimulationStart,
      defaultSimulationEnd,
      defaultOperationTime,
    )

  // default Lat/Long
  protected val defaultLatitude = 52.02083574
  protected val defaultLongitude = 7.40110716

  private val geometryFactory = new GeometryFactory()
  protected val defaultLatLong: Point = geometryFactory.createPoint(
    new Coordinate(defaultLongitude, defaultLatitude)
  )

  protected val default400Kva10KvRefSystem: RefSystem = RefSystem(
    Kilowatts(400d),
    Kilovolts(10d),
  )

  protected val defaultVoltageLimits: VoltageLimits = VoltageLimits(0.9, 1.1)

}
