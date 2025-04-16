/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.WecCharacteristicInput
import edu.ie3.datamodel.models.result.system.{
  SystemParticipantResult,
  WecResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.config.RuntimeConfig.WecRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant2.WecModel.{
  WecCharacteristic,
  WecState,
  molarMassAir,
  universalGasConstantR,
}
import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.Scope
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  AreaToSimona,
  PowerConversionSimona,
}
import squants._
import squants.energy.Watts
import squants.mass.{Kilograms, KilogramsPerCubicMeter}
import squants.motion.{MetersPerSecond, Pressure}
import squants.thermal.{Celsius, JoulesPerKelvin}
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet

class WecModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val rotorArea: Area,
    private val betzCurve: WecCharacteristic,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      WecState,
    ]
    with ParticipantSimpleFlexibility[WecState]
    with LazyLogging {

  override def determineState(
      lastState: WecState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): WecState = lastState.copy(tick = tick)

  override def handleInput(
      state: WecState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): WecState =
    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .map(newData =>
        state.copy(
          windVelocity = newData.windVel,
          temperature = newData.temp,
        )
      )
      .getOrElse(state)

  override def determineOperatingPoint(
      state: WecState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val betzCoefficient = determineBetzCoefficient(state.windVelocity)

    /** air density in kg/m³
      */
    val airDensity =
      calculateAirDensity(
        state.temperature,
        state.airPressure,
      ).toKilogramsPerCubicMeter

    val v = state.windVelocity.toMetersPerSecond

    /** cubed velocity in m³/s³
      */
    val cubedVelocity = v * v * v

    val activePower = Scope(
      // Combined, we get (kg * m²)/s³, which is Watts
      Watts(
        cubedVelocity * 0.5 * betzCoefficient.toEach * airDensity * rotorArea.toSquareMeters
      )
    ).map { power =>
      if (power > pRated) {
        logger.warn(
          "The fed in active power is higher than the estimated maximum active power of this plant ({} > {}). " +
            "Did you provide wrong weather input data?",
          power,
          pRated,
        )
        pRated
      } else
        power
    }.map(_ * -1)
      .get

    (ActivePowerOperatingPoint(activePower), None)
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
          molarMassAir.toKilograms * pressure.toPascals / (universalGasConstantR.toJoulesPerKelvin * temperature.toKelvinScale)
        )
    }
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: WecState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new WecResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new WecResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

}

object WecModel {

  /** Universal gas constant
    */
  private val universalGasConstantR = JoulesPerKelvin(8.31446261815324d)

  /** Molar mass of air, actually in kg/mol
    */
  private val molarMassAir = Kilograms(0.0289647d)

  /** Holds all relevant data for wec model calculation
    *
    * @param windVelocity
    *   current wind velocity
    * @param temperature
    *   current temperature
    * @param airPressure
    *   current air pressure
    */
  final case class WecState(
      override val tick: Long,
      windVelocity: Velocity,
      temperature: Temperature,
      airPressure: Option[Pressure],
  ) extends ModelState

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

  final case class Factory(
      input: WecInput
  ) extends ParticipantModelFactory[WecState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.WeatherService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): WecState =
      WecState(
        tick,
        MetersPerSecond(0d),
        Celsius(0d),
        None,
      )

    override def create(): WecModel =
      new WecModel(
        input.getUuid,
        input.getId,
        input.getType.getsRated.toApparent,
        input.getType.getCosPhiRated,
        QControl(input.getqCharacteristics),
        input.getType.getRotorArea.toSquants,
        WecCharacteristic(input.getType.getCpCharacteristic),
      )

  }

}
