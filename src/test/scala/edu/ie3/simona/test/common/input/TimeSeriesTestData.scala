/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.io.naming.timeseries.{
  ColumnScheme,
  IndividualTimeSeriesMetaInformation
}

import java.util.UUID

trait TimeSeriesTestData {
  protected val uuidP: UUID =
    UUID.fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5")
  protected val uuidPq: UUID =
    UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26")
  protected val uuidPqh: UUID =
    UUID.fromString("46be1e57-e4ed-4ef7-95f1-b2b321cb2047")

  protected val metaP: IndividualTimeSeriesMetaInformation =
    new IndividualTimeSeriesMetaInformation(
      uuidP,
      ColumnScheme.ACTIVE_POWER
    )
  protected val metaPq: IndividualTimeSeriesMetaInformation =
    new IndividualTimeSeriesMetaInformation(
      uuidPq,
      ColumnScheme.APPARENT_POWER
    )
  protected val metaPqh: IndividualTimeSeriesMetaInformation =
    new IndividualTimeSeriesMetaInformation(
      uuidPqh,
      ColumnScheme.APPARENT_POWER_AND_HEAT_DEMAND
    )
}
