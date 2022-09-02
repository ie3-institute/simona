/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import java.util.UUID
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
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import edu.ie3.util.quantities.EmptyQuantity
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOGRAM_PER_CUBIC_METRE,
  MEGAWATT
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.quantities.interfaces.{Density, HeatCapacity}
import edu.ie3.util.scala.OperationInterval

import javax.measure.quantity.{
  Area,
  Dimensionless,
  Power,
  Pressure,
  Speed,
  Temperature
}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.ProductUnit
import tech.units.indriya.unit.Units.{
  JOULE,
  KELVIN,
  KILOGRAM,
  METRE_PER_SECOND,
  PASCAL,
  SQUARE_METRE
}

import java.time.ZonedDateTime
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
      sRated,
      cosPhiRated
    ) {

  /** Universal gas constant
    */
  private val R = getQuantity(
    8.31446261815324,
    new ProductUnit[HeatCapacity](JOULE.divide(KELVIN))
  )
  private val AIR_MOLAR_MASS = getQuantity(0.0289647, KILOGRAM)

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
  ): ComparableQuantity[Power] = {
    val activePower = determinePower(wecData).to(MEGAWATT)
    val pMax = sMax.multiply(cosPhiRated).to(MEGAWATT)

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
     }).multiply(-1)
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
    val v = wecData.windVelocity.to(METRE_PER_SECOND)
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
      windVelocity: ComparableQuantity[Speed]
  ): ComparableQuantity[Dimensionless] = {
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
      temperature: ComparableQuantity[Temperature],
      airPressure: ComparableQuantity[Pressure]
  ): ComparableQuantity[Density] = {
    airPressure match {
      case _: EmptyQuantity[Pressure] =>
        getQuantity(1.2041, KILOGRAM_PER_CUBIC_METRE)
      case pressure =>
        AIR_MOLAR_MASS
          .multiply(pressure.to(PASCAL))
          .divide(R.multiply(temperature.to(KELVIN)))
          .asType(classOf[Density])
    }
  }

  override def determineFlexOptions(
      data: WecRelevantData
  ): ProvideFlexOptions = {
    val power = calculateActivePower(data)

    ProvideMinMaxFlexOptions(uuid, power, power, 0d.asMegaWatt)
  }

  override def handleControlledPowerChange(
      data: WecRelevantData,
      setPower: ComparableQuantity[Power]
  ): (WecRelevantData, Option[Long]) = (data, None)
}

/** Create valid [[WecModel]] by calling the apply function.
  */
object WecModel {

  /** This class is initialized with a [[WecCharacteristicInput]], which
    * contains the needed betz curve.
    */
  final case class WecCharacteristic private (
      override val xyCoordinates: SortedSet[XYPair[Speed, Dimensionless]]
  ) extends Characteristic[Speed, Dimensionless]

  object WecCharacteristic {
    import scala.jdk.CollectionConverters._

    /** Transform the inputs points from [[java.util.SortedSet]] to
      * [[scala.collection.SortedSet]], which is fed into [[WecCharacteristic]].
      */
    def apply(input: WecCharacteristicInput): WecCharacteristic =
      new WecCharacteristic(
        collection.immutable.SortedSet[XYPair[Speed, Dimensionless]]() ++
          input.getPoints.asScala.map(p =>
            XYPair[Speed, Dimensionless](
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
      windVelocity: ComparableQuantity[Speed],
      temperature: ComparableQuantity[Temperature],
      airPressure: ComparableQuantity[Pressure]
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
