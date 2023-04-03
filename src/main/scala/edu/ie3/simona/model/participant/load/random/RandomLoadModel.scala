/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.random

import de.lmu.ifi.dbs.elki.math.statistics.distribution.GeneralizedExtremeValueDistribution
import de.lmu.ifi.dbs.elki.utilities.random.RandomFactory
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference._
import edu.ie3.simona.model.participant.load.random.RandomLoadModel.RandomRelevantData
import edu.ie3.simona.model.participant.load.{DayType, LoadModel, LoadReference}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR, PU}
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.WATT

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

/** A load model consuming energy followed by time resolved probability. The
  * referencing to rated active power maps the output's 95 % quantile to this
  * value
  *
  * @param uuid
  *   unique identifier
  * @param id
  *   human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param scalingFactor
  *   Scaling the output of the system
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param reference
  *   Scale the random consumption to this reference
  */
final case class RandomLoadModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    reference: LoadReference
) extends LoadModel[RandomRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private lazy val energyReferenceScalingFactor = reference match {
    case EnergyConsumption(energyConsumption) =>
      energyConsumption
        .divide(RandomLoadModel.randomProfileEnergyScaling)
        .asType(classOf[Dimensionless])
        .to(PU)
    case _ =>
      throw new IllegalArgumentException(
        s"Applying energy reference scaling factor for reference mode '$reference' is not supported!"
      )
  }

  private val randomLoadParamStore = RandomLoadParamStore()

  type GevKey = (DayType.Value, Int)
  private val gevStorage =
    mutable.Map.empty[GevKey, GeneralizedExtremeValueDistribution]

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  @tailrec
  override protected def calculateActivePower(
      data: RandomRelevantData
  ): ComparableQuantity[Power] = {
    val gev = getGevDistribution(data.date)

    /* Get a next random power (in kW) */
    val randomPower = gev.nextRandom()
    if (randomPower < 0)
      calculateActivePower(data)
    else {
      val profilePower = Quantities.getQuantity(randomPower, KILOWATT)
      val activePower = reference match {
        case ActivePower(activePower) =>
          /* scale the reference active power based on the random profiles averagePower/maxPower ratio */
          val referenceScalingFactor = profilePower
            .divide(RandomLoadModel.randomMaxPower)
            .asType(classOf[Dimensionless])
            .to(PU)
            .getValue
            .doubleValue()
          activePower.multiply(referenceScalingFactor)
        case _: EnergyConsumption =>
          /* scale the profiles random power based on the energyConsumption/profileEnergyScaling(=1000kWh/year) ratio  */
          profilePower
            .multiply(energyReferenceScalingFactor)
            .asType(classOf[Power])
      }
      activePower.multiply(scalingFactor)
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
      TimeUtil.withDefaults.getQuarterHourOfDay(dateTime)
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
          randomFactory
        )
        gevStorage += (key -> newGev)
        newGev
    }
  }
}

case object RandomLoadModel {

  final case class RandomRelevantData(date: ZonedDateTime)
      extends LoadRelevantData

  /** The profile energy scaling factor, the random profile is scaled to.
    *
    * It is said in 'Kays - Agent-based simulation environment for improving the
    * planning of distribution grids', that the Generalized Extreme Value
    * distribution's parameters are sampled from input data, that is normalized
    * to 1,000 kWh annual energy consumption. However, due to inaccuracies in
    * random data reproduction, the sampled values will lead to an average
    * annual energy consumption of approx. this value. It has been found by
    * 1,000 evaluations of the year 2019.
    */
  private val randomProfileEnergyScaling =
    Quantities.getQuantity(716.5416966513656, KILOWATTHOUR)

  /** This is the 95 % quantile resulting from 10,000 evaluations of the year
    * 2019. It is only needed, when the load is meant to be scaled to rated
    * active power.
    *
    * @return
    *   Reference power to use for later model calculations
    */
  private val randomMaxPower: ComparableQuantity[Power] =
    Quantities.getQuantity(159d, WATT)

  def apply(
      input: LoadInput,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      reference: LoadReference
  ): RandomLoadModel = {
    reference match {
      case ActivePower(power) =>
        val sRatedPowerScaled =
          LoadModel.scaleSRatedActivePower(input, power, 1.1)

        RandomLoadModel(
          input.getUuid,
          input.getId,
          operationInterval,
          scalingFactor,
          QControl.apply(input.getqCharacteristics()),
          sRatedPowerScaled,
          input.getCosPhiRated,
          reference
        )
      case EnergyConsumption(energyConsumption) =>
        val sRatedEnergy = LoadModel.scaleSRatedEnergy(
          input,
          energyConsumption,
          randomMaxPower,
          randomProfileEnergyScaling
        )

        RandomLoadModel(
          input.getUuid,
          input.getId,
          operationInterval,
          scalingFactor,
          QControl.apply(input.getqCharacteristics()),
          sRatedEnergy,
          input.getCosPhiRated,
          reference
        )
    }
  }
}
