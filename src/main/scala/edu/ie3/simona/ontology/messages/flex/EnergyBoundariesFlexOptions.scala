/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.{
  EnergyBoundariesFlexOptionsResult,
  FlexOptionsResult,
}
import edu.ie3.simona.exceptions.{CriticalFailureException, FlexException}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.quantities.QuantityUtils.{asMegaWatt, asMegaWattHour}
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.EnergyConversions.EnergyNumeric
import squants.energy.PowerConversions.PowerNumeric
import squants.time.Seconds
import squants.{Dimensionless, Energy, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.SortedMap

/** Energy boundaries for one or several assets. See [[AssetEnergyBoundaries]]
  * for more details.
  *
  * @param energyBoundaries
  *   The energy boundaries.
  */
final case class EnergyBoundariesFlexOptions(
    energyBoundaries: Seq[AssetEnergyBoundaries]
) extends FlexOptions {

  /** The sum of all power limits (lower and upper each) of the enclosed
    * [[AssetEnergyBoundaries]].
    */
  lazy val powerLimits: ClosedInterval[Power] =
    new ClosedInterval(
      energyBoundaries
        .map(_.powerLimits.getLower)
        .sum,
      energyBoundaries
        .map(_.powerLimits.getUpper)
        .sum,
    )
}

object EnergyBoundariesFlexOptions
    extends FlexOptionsExtra[EnergyBoundariesFlexOptions] {

  private val log: Logger =
    LoggerFactory.getLogger(classOf[EnergyBoundariesFlexOptions])

  override val flexType: FlexType = FlexType.EnergyBoundaries

  override def determineFlexPower(
      flexOptions: EnergyBoundariesFlexOptions,
      flexCtrl: FlexibilityMessage.IssueFlexControl,
  ): Power =
    flexCtrl match {
      case IssuePowerControl(_, setPower) =>
        // sanity check: setPower is in range of latest flex options
        checkSetPower(flexOptions, setPower)

        setPower

      case IssueNoControl(_) =>
        log.warn(
          s"${classOf[EnergyBoundariesFlexOptions].getSimpleName} currently have no reference power, " +
            s"thus a set point of zero kW is chosen because we received $IssueNoControl."
        )
        zeroKW
    }

  override def checkSetPower(
      flexOptions: EnergyBoundariesFlexOptions,
      setPower: Power,
  ): Unit = {
    if setPower < flexOptions.powerLimits.getLower then
      throw new FlexException(
        s"The set power $setPower must not be lower than the minimum power ${flexOptions.powerLimits.getLower}!"
      )
    else if setPower > flexOptions.powerLimits.getUpper then
      throw new FlexException(
        s"The set power $setPower must not be greater than the maximum power ${flexOptions.powerLimits.getUpper}!"
      )
  }

  override def createResult(
      flexOptions: EnergyBoundariesFlexOptions,
      modelUuid: UUID,
      dateTime: ZonedDateTime,
  ): FlexOptionsResult = {

    val firstEnergyLimits = flexOptions.energyBoundaries
      .map(
        _.energyLimits.headOption.getOrElse(
          throw new CriticalFailureException(
            s"Empty energy limits. At least one entry needs to be provided."
          )
        )
      )

    val currentState = flexOptions.energyBoundaries.map { _.currentEnergy }.sum
    val lowerEnergyLimit = firstEnergyLimits.map { case (_, energyLimits) =>
      energyLimits.getLower
    }.sum
    val upperEnergyLimit = firstEnergyLimits.map { case (_, energyLimits) =>
      energyLimits.getUpper
    }.sum

    new EnergyBoundariesFlexOptionsResult(
      dateTime,
      modelUuid,
      currentState.toMegawattHours.asMegaWattHour,
      lowerEnergyLimit.toMegawattHours.asMegaWattHour,
      upperEnergyLimit.toMegawattHours.asMegaWattHour,
      flexOptions.powerLimits.getLower.toMegawatts.asMegaWatt,
      flexOptions.powerLimits.getUpper.toMegawatts.asMegaWatt,
    )
  }

  override def zero(tick: Long): EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = zeroKWh,
        currentEnergy = zeroKWh,
        pMax = zeroKW,
        etaCharge = onePU,
        etaDischarge = onePU,
        currentTick = tick,
      )
    )

  /** Creates energy boundaries with a single [[AssetEnergyBoundaries]].
    *
    * @param singleBoundaries
    *   The [[AssetEnergyBoundaries]].
    * @return
    */
  def apply(
      singleBoundaries: AssetEnergyBoundaries
  ): EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(Seq(singleBoundaries))

  /** Energy boundaries for an asset. The energy limits (valid for the interval
    * from tick to the next) constitute the boundaries between which flexibility
    * can be used.
    *
    * @param currentEnergy
    *   The current state of energy.
    * @param energyLimits
    *   Energy limits that signify the minimum and maximum state of energy for
    *   the respective tick. The energy limits for all ticks relate to the
    *   energy potential at the current tick, [[currentEnergy]].
    * @param powerLimits
    *   The power limits, which limit the power of the complete asset for all
    *   time steps. If energy limits (upper and lower) are the same at some time
    *   step, power limits are ignored.
    * @param etaCharge
    *   The charging efficiency.
    * @param etaDischarge
    *   The discharging efficiency.
    * @param tickDisconnect
    *   Optionally, the tick at which the storage will be disconnected, thus the
    *   upward or downward energy potential of the tick before
    *   [[tickDisconnect]] can not be used afterwards.
    */
  final case class AssetEnergyBoundaries(
      currentEnergy: Energy,
      energyLimits: SortedMap[Long, ClosedInterval[Energy]],
      powerLimits: ClosedInterval[Power],
      etaCharge: Dimensionless = onePU,
      etaDischarge: Dimensionless = onePU,
      tickDisconnect: Option[Long] = None,
  )

  object AssetEnergyBoundaries {

    /** Creating energy boundaries for a power that is assumed to remain
      * constant over the forecasted time horizon.
      *
      * @param constantPower
      *   The constant power.
      * @return
      *   The [[AssetEnergyBoundaries]].
      */
    def apply(
        constantPower: Power,
        currentTick: Long,
    ): AssetEnergyBoundaries = {
      AssetEnergyBoundaries(
        currentEnergy = zeroKWh,
        energyLimits =
          SortedMap(currentTick -> new ClosedInterval(zeroKWh, zeroKWh)),
        powerLimits = new ClosedInterval(constantPower, constantPower),
      )
    }

    /** Creating energy boundaries for a fixed power time series. Assumes
      * equidistant power series entries - otherwise, results are not defined!
      *
      * @param powerSeries
      *   The power time series (at equidistant ticks). Has to consist of at
      *   least two entries.
      * @return
      *   The [[AssetEnergyBoundaries]].
      */
    def apply(
        powerSeries: SortedMap[Long, Power]
    ): AssetEnergyBoundaries = {

      val (firstTick, firstPower) = powerSeries.headOption.getOrElse(
        throw new CriticalFailureException("Empty power time series!")
      )

      // adding a dummy tick at the end, so that the last actual power entry
      // in the power time series is used
      val lastSeriesTick = powerSeries.lastOption
        .map { case (tick, _) => tick }
        .getOrElse(
          throw new CriticalFailureException("Empty power time series!")
        )
      // dummy tick is the next logical tick of the tick series
      // (e.g. (5, 10, 15, 20) -> 25)
      val dummyTick =
        lastSeriesTick + (lastSeriesTick - firstTick) / (powerSeries.size - 1)

      val (energySeries, _) =
        powerSeries
          // excluding first data, which we already extracted above
          .tail
          // adding dummy tick so that last actual power entry is recognized
          .updated(dummyTick, zeroKW)
          .foldLeft(
            (SortedMap(firstTick -> zeroKWh), firstPower)
          ) { case ((previousSeries, previousPower), (tick, power)) =>
            val (lastTick, lastEnergy) = previousSeries
              .maxBefore(tick)
              .getOrElse(
                throw new CriticalFailureException(
                  s"No value before $tick in previous values $previousSeries"
                )
              )

            // added energy from last to current tick
            val addedEnergy = previousPower * Seconds(tick - lastTick)
            // total current energy
            val tickEnergy = lastEnergy + addedEnergy

            (previousSeries.updated(tick, tickEnergy), power)
          }

      val minPower = powerSeries.values.minOption.getOrElse(zeroKW)
      val maxPower = powerSeries.values.maxOption.getOrElse(zeroKW)

      AssetEnergyBoundaries(
        currentEnergy = zeroKWh,
        energyLimits = energySeries.map { case (tick, energy) =>
          tick -> ClosedInterval(energy, energy)
        },
        powerLimits = ClosedInterval(minPower, maxPower),
      )
    }

    /** Creating energy boundaries for a storage model with symmetrical maximum
      * power.
      *
      * @param eStorage
      *   The storage capacity.
      * @param currentEnergy
      *   The currently stored energy.
      * @param pMax
      *   Maximum permissible active power.
      * @param etaCharge
      *   Efficiency of the charging process.
      * @param etaDischarge
      *   Efficiency of the discharging process.
      * @param currentTick
      *   The current tick.
      * @return
      *   [[AssetEnergyBoundaries]] for the storage model.
      */
    def apply(
        eStorage: Energy,
        currentEnergy: Energy,
        pMax: Power,
        etaCharge: Dimensionless,
        etaDischarge: Dimensionless,
        currentTick: Long,
    ): AssetEnergyBoundaries =
      AssetEnergyBoundaries(
        currentEnergy = currentEnergy,
        energyLimits = SortedMap(
          currentTick -> new ClosedInterval(zeroKWh, eStorage)
        ),
        powerLimits = new ClosedInterval(-pMax, pMax),
        etaCharge = etaCharge,
        etaDischarge = etaDischarge,
      )

  }

}
