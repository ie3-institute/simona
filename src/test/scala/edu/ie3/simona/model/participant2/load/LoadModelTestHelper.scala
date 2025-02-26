/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile
import edu.ie3.simona.model.participant2.ParticipantModel.ActivePowerOperatingPoint
import edu.ie3.simona.model.participant2.load.LoadModel.LoadModelState
import edu.ie3.simona.service.load.LoadProfileStore
import squants.energy.KilowattHours
import squants.time.Minutes
import squants.{Dimensionless, Each, Energy, Power, Quantity}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

trait LoadModelTestHelper {

  private val store = LoadProfileStore()

  protected def calculateEnergyDiffForYear(
      model: LoadModel[LoadModelState],
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
      model: LoadModel[LoadModelState],
      simulationStartDate: ZonedDateTime,
  ): Iterable[Power] = {
    val quarterHoursInYear = 365L * 96L

    (0L until quarterHoursInYear)
      .map { quarterHour =>
        val tick = quarterHour * 15 * 60
        val dateTime =
          simulationStartDate.plus(quarterHour * 15, ChronoUnit.MINUTES)

        val averagePower = (model match {
          case profileLoadModel: ProfileLoadModel =>
            store.entry(dateTime, profileLoadModel.loadProfile)
          case _: RandomLoadModel =>
            store.entry(dateTime, RandomLoadProfile.RANDOM_LOAD_PROFILE)
        }).getOrElse(
          throw new SourceException("No load value present!")
        )

        val state = LoadModelState(
          tick,
          averagePower,
        )

        model
          .determineOperatingPoint(state) match {
          case (ActivePowerOperatingPoint(p), _) =>
            p
        }
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
