/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.config.InputConfig.NoiseConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.service.DataTimeType.CurrentAndForecast

import scala.collection.{immutable, mutable}
import scala.util.Random

/** Generates noise with a mean of mu = 0 and variance of sigma^2 = 1. Noise
  * values persist for their individual ticks throughout simulation.
  *
  * @param noiseMap
  *   Map of tick to noise values generated with variance of 1. Gets updated
  *   throughout simulation, dropping past values and adding future values that
  *   were not set yet.
  * @param autoregression
  *   Autoregression coefficient between 0 and 1, controlling how similar
  *   neighboring values are.
  * @param rng
  *   The random number generator.
  */
final case class NoiseGenerator(
    noiseMap: mutable.SortedMap[Long, Double],
    autoregression: Double,
    rng: Random,
) {

  /** Updates the internal map of noise values according to the current tick.
    * Obsolete values are discarded and missing noise values are added.
    * @param currentTick
    *   The current tick.
    * @param forecastParams
    *   The forecast parameters specifying resolution and forecast horizon.
    * @return
    *   An updated noise generator.
    */
  def update(
      currentTick: Long,
      forecastParams: CurrentAndForecast,
  ): NoiseGenerator = {
    val ticks = forecastParams.ticks(currentTick)

    ticks.foreach { tick =>
      if !noiseMap.contains(tick) then {
        val previous = noiseMap
          .maxBefore(tick)
          .map { case (_, value) =>
            value
          }
        noiseMap.update(tick, generate(previous))
      }
    }

    // remove outdated entries
    copy(noiseMap = noiseMap.rangeFrom(currentTick))
  }

  private def generate(optPreviousNoise: Option[Double]): Double = {
    val newNoise = rng.nextGaussian()

    optPreviousNoise
      .map { previousNoise =>
        autoregression * previousNoise + math.sqrt(
          1 - autoregression * autoregression
        ) * newNoise
      }
      .getOrElse(newNoise)
  }

  /** Adds noise to selected fields of the given data container. Noise values
    * are scaled according to the time distance into the future.
    *
    * @param data
    *   The data container to which noise is to be added.
    * @param applyToData
    *   Applies noise to selected fields of given data container. Since noise
    *   has been generated with variance of 1, the actual variance needs to be
    *   applied here.
    * @param forecastParams
    *   The forecast parameters specifying resolution and forecast horizon.
    * @tparam D
    *   The type of data.
    * @return
    *   Data with added noise.
    */
  def applyToSeries[D](
      data: immutable.SortedMap[Long, D],
      applyToData: (D, Double) => D,
      forecastParams: CurrentAndForecast,
  ): immutable.SortedMap[Long, D] = {
    val firstTick = data.firstKey
    val lengthInTicks = forecastParams.forecastLength.toSeconds.toLong

    data.map { case (tick, data) =>
      val (_, noise) = noiseMap
        .maxBefore(tick + 1)
        .getOrElse(
          throw new CriticalFailureException(
            s"No noise value found at or before $tick"
          )
        )
      val scaledNoise = noise * (tick - firstTick) / lengthInTicks

      tick -> applyToData(data, scaledNoise)
    }
  }

}

object NoiseGenerator {

  def apply(config: NoiseConfig): NoiseGenerator =
    NoiseGenerator(config.autoregression, config.seed)

  def apply(
      autoregression: Double,
      seed: Long,
  ): NoiseGenerator =
    NoiseGenerator(
      noiseMap = mutable.SortedMap.empty,
      autoregression = autoregression,
      rng = Random(seed),
    )

  extension [D](series: immutable.SortedMap[Long, D]) {

    def applyNoise(
        generator: NoiseGenerator,
        applyToData: (D, Double) => D,
        forecastParams: CurrentAndForecast,
    ): immutable.SortedMap[Long, D] =
      generator.applyToSeries(series, applyToData, forecastParams)

  }

}
