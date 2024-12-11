/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  DateTimeData,
  FixedState,
}
import squants.{Dimensionless, Each, Energy, Power, Quantity}
import squants.energy.KilowattHours
import squants.time.Minutes

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

trait LoadModelTestHelper {

  protected def calculateEnergyDiffForYear(
      model: LoadModel[DateTimeData],
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
      model: LoadModel[DateTimeData],
      simulationStartDate: ZonedDateTime,
  ): Iterable[Power] = {
    val quarterHoursInYear = 365L * 96L

    (0L until quarterHoursInYear)
      .map { quarterHour =>
        val tick = quarterHour * 15 * 60
        val relevantData = DateTimeData(
          tick,
          simulationStartDate.plus(quarterHour * 15, ChronoUnit.MINUTES),
        )

        model
          .determineOperatingPoint(
            FixedState(tick),
            relevantData,
          ) match {
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

}
