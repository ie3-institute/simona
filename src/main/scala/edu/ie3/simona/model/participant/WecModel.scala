/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.WecCharacteristicInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.WecModel.{
  WecCharacteristic,
  WecRelevantData,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants._
import squants.energy.{Kilowatts, Watts}
import squants.mass.{Kilograms, KilogramsPerCubicMeter}
import squants.motion.{MetersPerSecond, Pressure}
import squants.space.SquareMeters
import squants.thermal.JoulesPerKelvin
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
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
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    rotorArea: Area,
    betzCurve: WecCharacteristic,
) extends SystemParticipant[WecRelevantData, ApparentPower, ConstantState.type](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    )
    with ApparentPowerParticipant[WecRelevantData, ConstantState.type] {

  /** Universal gas constant
    */
  private val UniversalGasConstantR = JoulesPerKelvin(8.31446261815324d)

  /** Molar mass of air, actually in kg/mol
    */
  private val MolarMassAir = Kilograms(0.0289647d)

  /** Calculate the active power output of the [[WecModel]]. First determine the
    * power, then check if it exceeds rated apparent power.
    *
    * @param wecData
    *   data with wind velocity and temperature
    * @return
    *   active power output
    */
  override def calculateActivePower(
      modelState: ConstantState.type,
      wecData: WecRelevantData,
  ): Power = {
    val activePower = determinePower(wecData)
    val pMax = sMax * cosPhiRated

    (if (activePower > pMax) {
       logger.warn(
         "The fed in active power is higher than the estimated maximum active power of this plant ({} > {}). " +
           "Did you provide wrong weather input data?",
         activePower,
         pMax,
       )
       pMax
     } else {
       activePower
     }) * (-1)
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
  ): Power = {
    val betzCoefficient = determineBetzCoefficient(wecData.windVelocity)

    /** air density in kg/m³
      */
    val airDensity =
      calculateAirDensity(
        wecData.temperature,
        wecData.airPressure,
      ).toKilogramsPerCubicMeter

    val v = wecData.windVelocity.toMetersPerSecond

    /** cubed velocity in m³/s³
      */
    val cubedVelocity = v * v * v

    // Combined, we get (kg * m²)/s³, which is Watts
    Watts(
      cubedVelocity * 0.5 * betzCoefficient.toEach * airDensity * rotorArea.toSquareMeters
    )
  }

  /** The coefficient is dependent on the wind velocity v. Therefore use v to
    * determine the betz coefficient cₚ.
    *
    * @param windVelocity
    *   current wind velocity
    * @return
    *   betz coefficient cₚ
    */
  def determineBetzCoefficient(
      windVelocity: Velocity
  ): Dimensionless = {
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
  def calculateAirDensity(
      temperature: Temperature,
      airPressure: Option[Pressure],
  ): Density = {
    airPressure match {
      case None =>
        KilogramsPerCubicMeter(1.2041d)
      case Some(pressure) =>
        // kg * mol^-1 * J * m^-3 * J^-1 * K * mol * K^-1
        // = kg * m^-3
        KilogramsPerCubicMeter(
          MolarMassAir.toKilograms * pressure.toPascals / (UniversalGasConstantR.toJoulesPerKelvin * temperature.toKelvinScale)
        )
    }
  }

  override def determineFlexOptions(
      data: WecRelevantData,
      lastState: ConstantState.type,
  ): ProvideFlexOptions = {
    val power = calculateActivePower(ConstantState, data)

    ProvideMinMaxFlexOptions(uuid, power, power, zeroKW)
  }

  override def handleControlledPowerChange(
      data: WecRelevantData,
      lastState: ConstantState.type,
      setPower: Power,
  ): (ConstantState.type, FlexChangeIndicator) =
    (lastState, FlexChangeIndicator())
}

/** Create valid [[WecModel]] by calling the apply function.
  */
object WecModel {

  /** This class is initialized with a [[WecCharacteristicInput]], which
    * contains the needed betz curve.
    */
  final case class WecCharacteristic(
      override val xyCoordinates: SortedSet[
        XYPair[Velocity, Dimensionless]
      ]
  ) extends Characteristic[Velocity, Dimensionless]

  object WecCharacteristic {
    import scala.jdk.CollectionConverters._

    /** Transform the inputs points from [[java.util.SortedSet]] to
      * [[scala.collection.SortedSet]], which is fed into [[WecCharacteristic]].
      */
    def apply(input: WecCharacteristicInput): WecCharacteristic =
      new WecCharacteristic(
        collection.immutable
          .SortedSet[XYPair[Velocity, Dimensionless]]() ++
          input.getPoints.asScala.map(p =>
            XYPair[Velocity, Dimensionless](
              MetersPerSecond(p.getX.to(METRE_PER_SECOND).getValue.doubleValue),
              Each(p.getY.to(PU).getValue.doubleValue),
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
      windVelocity: Velocity,
      temperature: Temperature,
      airPressure: Option[Pressure],
  ) extends CalcRelevantData

  def apply(
      inputModel: WecInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): WecModel = {
    val scaledInput = inputModel.copy().scale(scalingFactor).build()

    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      scaledInput.getOperationTime,
    )

    val model = new WecModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      QControl(scaledInput.getqCharacteristics),
      Kilowatts(
        scaledInput.getType.getsRated.to(KILOWATT).getValue.doubleValue
      ),
      scaledInput.getType.getCosPhiRated,
      SquareMeters(
        scaledInput.getType.getRotorArea.to(SQUARE_METRE).getValue.doubleValue
      ),
      WecCharacteristic(scaledInput.getType.getCpCharacteristic),
    )

    model.enable()
    model
  }
}
