/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.WecCharacteristicInput
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.WecModel.{
  WecCharacteristic,
  WecRelevantData
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.util.quantities.EmptyQuantity
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOGRAM_PER_CUBIC_METRE,
  KILOWATT,
  MEGAWATT
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble

import edu.ie3.util.scala.OperationInterval
import squants.Velocity
import squants.energy.{Kilowatts, Megawatts}
import squants.mass.{Kilograms, KilogramsPerCubicMeter}
import squants.thermal.JoulesPerKelvin
import tech.units.indriya.ComparableQuantity

import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity._
import scala.collection.SortedSet

/** A wind energy converter model used for calculating output power of a wind
  * turbine.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
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
  * @param rotorArea
  *   Area of rotor
  * @param betzCurve
  *   cₚ value for different wind velocities
  */
final case class WecModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    rotorArea: ComparableQuantity[Area],
    betzCurve: WecCharacteristic
) extends SystemParticipant[WecRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Kilowatts(sRated.to(KILOWATT).getValue.doubleValue),
      cosPhiRated
    ) {

  /** Universal gas constant
    */
  private val R = JoulesPerKelvin(8.31446261815324d)
  private val AIR_MOLAR_MASS = Kilograms(0.0289647d)

  /** Calculate the active power output of the [[WecModel]]. First determine the
    * power, then check if it exceeds rated apparent power.
    *
    * @param wecData
    *   data with wind velocity and temperature
    * @return
    *   active power output
    */
  override protected def calculateActivePower(
      wecData: WecRelevantData
  ): squants.Power = {
    val activePower = determinePower(wecData).to(MEGAWATT)
    val pMax = sMax.toKilowatts.asKiloWatt.multiply(cosPhiRated).to(MEGAWATT)

    Megawatts(
      (if (activePower.isGreaterThan(pMax)) {
         logger.warn(
           "The fed in active power is higher than the estimated maximum active power of this plant ({} > {}). " +
             "Did you provide wrong weather input data?",
           activePower,
           pMax
         )
         pMax
       } else {
         activePower
       }).multiply(-1).to(MEGAWATT).getValue.doubleValue
    )
  }

  /** Determine the turbine output power with the air density ρ, the wind
    * velocity v, the rotor area A and the betz coefficient cₚ using the
    * following formula:
    *
    * <strong>P = v³ * 0.5 * cₚ * ρ * A</strong>
    *
    * @param wecData
    *   data with wind velocity and temperature
    * @return
    *   active power output
    */
  private def determinePower(
      wecData: WecRelevantData
  ): ComparableQuantity[Power] = {
    val betzCoefficient = determineBetzCoefficient(wecData.windVelocity)
    val airDensity =
      calculateAirDensity(wecData.temperature, wecData.airPressure)
    val v = wecData.windVelocity
    val cubedVelocity = v.multiply(v).multiply(v)

    cubedVelocity
      .multiply(0.5)
      .multiply(betzCoefficient)
      .multiply(airDensity.to(KILOGRAM_PER_CUBIC_METRE))
      .multiply(rotorArea.to(SQUARE_METRE))
      .asType(classOf[Power])
  }

  /** The coefficient is dependent on the wind velocity v. Therefore use v to
    * determine the betz coefficient cₚ.
    *
    * @param windVelocity
    *   current wind velocity
    * @return
    *   betz coefficient cₚ
    */
  private def determineBetzCoefficient(
      windVelocity: squants.Velocity
  ): squants.Dimensionless = {
    betzCurve.interpolateXy(windVelocity) match {
      case (_, cp) => cp
    }
  }

  /** Calculate the correct air density, dependent on the current temperature
    * and air pressure.
    *
    * If no air pressure is given, the default density 1.2041 is returned (air
    * density for 20 degrees Celsius at sea level)
    *
    * @param temperature
    *   current temperature
    * @param airPressure
    *   current air pressure
    * @return
    */
  private def calculateAirDensity(
      temperature: squants.Temperature,
      airPressure: Option[squants.motion.Pressure]
  ): squants.Density = {
    airPressure match {
      case None =>
        KilogramsPerCubicMeter(1.2041d)
      case Some(pressure) =>
        AIR_MOLAR_MASS * (pressure.toPascals)/(R * temperature.toKelvinScale)
    }
  }
}

/** Create valid [[WecModel]] by calling the apply function.
  */
object WecModel {

  /** This class is initialized with a [[WecCharacteristicInput]], which
    * contains the needed betz curve.
    */
  final case class WecCharacteristic private (
      override val xyCoordinates: SortedSet[
        XYPair[Velocity, squants.Dimensionless]
      ]
  ) extends Characteristic[Velocity, squants.Dimensionless]

  object WecCharacteristic {
    import scala.jdk.CollectionConverters._

    /** Transform the inputs points from [[java.util.SortedSet]] to
      * [[scala.collection.SortedSet]], which is fed into [[WecCharacteristic]].
      */
    def apply(input: WecCharacteristicInput): WecCharacteristic =
      new WecCharacteristic(
        collection.immutable
          .SortedSet[XYPair[Velocity, squants.Dimensionless]]() ++
          input.getPoints.asScala.map(p =>
            XYPair[Velocity, squants.Dimensionless](
              p.getX.to(METRE_PER_SECOND),
              p.getY
            )
          )
      )
  }

  /** This class contains the needed [[CalcRelevantData]] for the
    * [[WecModel.calculateActivePower]] method.
    *
    * @param windVelocity
    *   current wind velocity
    * @param temperature
    *   current temperature
    * @param airPressure
    *   current air pressure
    */
  final case class WecRelevantData(
      windVelocity: squants.Velocity,
      temperature: squants.Temperature,
      airPressure: squants.motion.Pressure
  ) extends CalcRelevantData

  def apply(
      inputModel: WecInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): WecModel = {
    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      inputModel.getOperationTime
    )

    val model = new WecModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl(inputModel.getqCharacteristics),
      inputModel.getType.getsRated,
      inputModel.getType.getCosPhiRated,
      inputModel.getType.getRotorArea,
      WecCharacteristic(inputModel.getType.getCpCharacteristic)
    )

    model.enable()
    model
  }
}
