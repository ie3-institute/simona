/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.Data.SecondaryData.SecondarySeriesData
import edu.ie3.simona.test.common.OptimizingTestLike
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.{Hours, Time}

import java.util.UUID

trait PowerObjectiveTestScenario extends OptimizingTestLike {

  protected val pvUUID: UUID = UUID.fromString("0-0-0-0-1")
  protected val loadUUID: UUID = UUID.fromString("0-0-0-0-2")
  protected val batUUID: UUID = UUID.fromString("0-0-0-0-3")
  protected val bat2UUID: UUID = UUID.fromString("0-0-0-0-4")

  /* SAMPLE TIME DEFINITIONS */

  protected val halfHour: Time = Hours(0.5)
  protected val halfHourTicks: Long = halfHour.toSeconds.toLong

  protected val hour: Time = Hours(1)
  protected val hourTicks: Long = hour.toSeconds.toLong

  /* SCENARIO 1 */

  protected given ticksScenario1: Seq[Long] =
    Range.Long.inclusive(0, 12 * halfHourTicks, halfHourTicks)

  protected val priceDataScenario1: SecondarySeriesData =
    (Seq.fill(2)((0.1d, 0.3d)) ++
      Seq.fill(6)((-0.02d, 0.2d)) ++
      Seq.fill(4)((0.1d, 0.3d))).toPriceData

  // 16.5 kWh of feed-in in total, more than battery can store
  protected val pvFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(0, -6, -8, -7, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap
      )
    )

  // 18 kWh of load in total, more than battery can provide
  protected val loadFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(0, 0, 0, 0, 0, 0, 8, 12, 4, 7, 5, 0).toPowerMap
      )
    )

  // low efficiency for simplicity of the test
  protected val batFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(10),
        currentEnergy = KilowattHours(0),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  protected val flexOptionsScenario1: Map[UUID, EnergyBoundariesFlexOptions] =
    Map(
      pvUUID -> pvFlexScenario1,
      loadUUID -> loadFlexScenario1,
      batUUID -> batFlexScenario1,
    )

}
