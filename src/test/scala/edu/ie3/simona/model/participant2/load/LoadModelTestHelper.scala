/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.simona.service.load.LoadProfileStore
import squants.energy.KilowattHours
import squants.time.Minutes
import squants.{Dimensionless, Each, Energy, Power, Quantity}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

trait LoadModelTestHelper {

  private val store = LoadProfileStore()

  protected def calculateEnergyDiffForYear(
      model: ProfileLoadModel,
      simulationStartDate: ZonedDateTime,
      expectedEnergy: Energy,
  ): Dimensionless = {
    val duration = Minutes(15d)

    val avgEnergy = calculatePowerForYear(
      model,
      simulationStartDate,
    ).foldLeft(KilowattHours(0)) { case (energySum, power) =>
      energySum + (power * duration)
    }

    getRelativeDifference(
      avgEnergy,
      expectedEnergy,
    )
  }

  protected def calculatePowerForYear(
      model: ProfileLoadModel,
      simulationStartDate: ZonedDateTime,
  ): Iterable[Power] = {
    val quarterHoursInYear = 365L * 96L

    (0L until quarterHoursInYear)
      .map { quarterHour =>
        val dateTime =
          simulationStartDate.plus(quarterHour * 15, ChronoUnit.MINUTES)

        val averagePower = (model match {
          case profileLoadModel: ProfileLoadModel =>
            store.entry(dateTime, profileLoadModel.loadProfile)
        }).getOrElse(
          throw new SourceException("No load value present!")
        )

        averagePower * model.referenceScalingFactor
      }
  }

  protected def getRelativeDifference[Q <: Quantity[Q]](
      actualResult: Q,
      expectedResult: Q,
  ): Dimensionless =
    Each((expectedResult - actualResult).abs / expectedResult)

  protected def get95Quantile[V](sortedArray: Array[V]): V = sortedArray(
    (sortedArray.length * 0.95).toInt
  )

}
