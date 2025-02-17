/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import de.lmu.ifi.dbs.elki.math.statistics.distribution.GeneralizedExtremeValueDistribution
import de.lmu.ifi.dbs.elki.utilities.random.RandomFactory
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.DayType
import edu.ie3.simona.model.participant.load.random.RandomLoadParamStore
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  DateTimeState,
  ParticipantDateTimeState,
}
import edu.ie3.simona.util.TickUtil
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.ApparentPower
import squants.Power
import squants.energy.{KilowattHours, Kilowatts, Watts}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.mutable
import scala.util.Random

class RandomLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val referenceScalingFactor: Double,
) extends LoadModel[DateTimeState]
    with ParticipantDateTimeState[ActivePowerOperatingPoint] {

  private val randomLoadParamStore = RandomLoadParamStore()

  private type GevKey = (DayType.Value, Int)
  private val gevStorage =
    mutable.Map.empty[GevKey, GeneralizedExtremeValueDistribution]

  override def determineOperatingPoint(
      state: DateTimeState
  ): (ParticipantModel.ActivePowerOperatingPoint, Option[Long]) = {
    val resolution = RandomLoadParamStore.resolution.getSeconds

    val (modelTick, modelDateTime) = TickUtil.roundToResolution(
      state.tick,
      state.dateTime,
      resolution.toInt,
    )

    val gev = getGevDistribution(modelDateTime)

    /* Get a next random power (in kW) */
    val randomPower = gev.nextRandom()
    if (randomPower < 0)
      determineOperatingPoint(state)
    else {
      val nextTick = modelTick + resolution
      (
        ActivePowerOperatingPoint(
          Kilowatts(randomPower) * referenceScalingFactor
        ),
        Some(nextTick),
      )
    }
  }

  /** Get the needed generalized extreme value distribution from the store or
    * instantiate a new one and put it to the store.
    *
    * @param dateTime
    *   Questioned date time
    * @return
    *   The needed generalized extreme value distribution
    */
  private def getGevDistribution(
      dateTime: ZonedDateTime
  ): GeneralizedExtremeValueDistribution = {
    /* Determine identifying key for a distinct generalized extreme value distribution and look it up. If it is not
     * available, yet, instantiate one. */
    val key: GevKey = (
      DayType(dateTime.getDayOfWeek),
      TimeUtil.withDefaults.getQuarterHourOfDay(dateTime),
    )
    gevStorage.get(key) match {
      case Some(foundIt) => foundIt
      case None          =>
        /* Instantiate new gev distribution, put it to storage and return it */
        val randomFactory = RandomFactory.get(Random.nextLong())
        val gevParameters = randomLoadParamStore.parameters(dateTime)
        val newGev = new GeneralizedExtremeValueDistribution(
          gevParameters.my,
          gevParameters.sigma,
          gevParameters.k,
          randomFactory,
        )
        gevStorage += (key -> newGev)
        newGev
    }
  }

}

object RandomLoadModel {

  /** The annual energy consumption that the random profile is scaled to.
    *
    * It is said in 'Kays - Agent-based simulation environment for improving the
    * planning of distribution grids', that the Generalized Extreme Value
    * distribution's parameters are sampled from input data, that is normalized
    * to 1,000 kWh annual energy consumption. However, due to inaccuracies in
    * random data reproduction, the sampled values will lead to an average
    * annual energy consumption of approx. this value. It has been found by
    * 1,000 evaluations of the year 2019.
    */
  private val profileReferenceEnergy = KilowattHours(716.5416966513656)

  /** This is the 95 % quantile resulting from 10,000 evaluations of the year
    * 2019. It is only needed, when the load is meant to be scaled to rated
    * active power.
    */
  private val maxPower: Power = Watts(159d)

  def apply(input: LoadInput, config: LoadRuntimeConfig): RandomLoadModel = {

    val referenceType = LoadReferenceType(config)

    val (referenceScalingFactor, scaledSRated) =
      LoadModel.scaleToReference(
        referenceType,
        input,
        maxPower,
        profileReferenceEnergy,
      )

    /** Safety factor to address potential higher sRated values when using
      * unrestricted probability functions
      */
    val safetyFactor = 1.1

    new RandomLoadModel(
      input.getUuid,
      input.getId,
      scaledSRated * safetyFactor,
      input.getCosPhiRated,
      QControl.apply(input.getqCharacteristics()),
      referenceScalingFactor,
    )
  }

}
