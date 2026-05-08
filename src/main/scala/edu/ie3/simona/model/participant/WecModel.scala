/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.WecCharacteristicInput
import edu.ie3.datamodel.models.result.system.{
  SystemParticipantResult,
  WecResult,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.WecModel.*
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.{
  ParticipantFlexModel,
  ParticipantInflexibleEnergyLimitFlexModel,
  ParticipantInflexiblePowerLimitFlexModel,
}
import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.Data.SecondaryData.{
  SecondarySeriesData,
  WeatherData,
}
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.{asMegaVar, asMegaWatt}
import edu.ie3.util.scala.Scope
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toApparent,
  toSquants,
}
import squants.*
import squants.energy.Watts
import squants.mass.{Kilograms, KilogramsPerCubicMeter}
import squants.motion.{MetersPerSecond, Pressure}
import squants.thermal.JoulesPerKelvin
import tech.units.indriya.unit.Units.*

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet
import scala.collection.immutable.SortedMap

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
    with LazyLogging {

  override val flexModels: Map[FlexType, ParticipantFlexModel[
    ActivePowerOperatingPoint,
    WecState,
  ]] =
    Map(
      FlexType.PowerLimit -> ParticipantInflexiblePowerLimitFlexModel(this),
      FlexType.EnergyBoundaries -> ParticipantInflexibleEnergyLimitFlexModel(
        this,
        _.toStateSeries,
      ),
    )

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
      .collectFirst {
        case weatherData: WeatherData =>
          SortedMap(state.tick -> AirWeatherData(weatherData))
        case SecondarySeriesData(series) =>
          series.map {
            case (tick, weatherData: WeatherData) =>
              tick -> AirWeatherData(weatherData)
            case (_, unexpectedData) =>
              throw new CriticalFailureException(
                s"Unexpected secondary data $unexpectedData"
              )
          }
      }
      .map(newData => state.copy(weatherData = newData))
      .getOrElse(state)

  override def determineOperatingPoint(
      state: WecState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val (_, weatherData) = state.weatherData
      .maxBefore(state.tick + 1)
      .getOrElse(
        throw new CriticalFailureException(
          s"No weather data available for current tick ${state.tick}"
        )
      )

    val betzCoefficient = determineBetzCoefficient(weatherData.windVelocity)

    /** air density in kg/m³
      */
    val airDensity =
      calculateAirDensity(
        weatherData.temperature,
        weatherData.airPressure,
      ).toKilogramsPerCubicMeter

    val v = weatherData.windVelocity.toMetersPerSecond

    /** cubed velocity in m³/s³
      */
    val cubedVelocity = v * v * v

    val activePower = Scope(
      // Combined, we get (kg * m²)/s³, which is Watts
      Watts(
        cubedVelocity * 0.5 * betzCoefficient.toEach * airDensity * rotorArea.toSquareMeters
      )
    ).map { power =>
      if power > pRated then {
        logger.warn(
          "The fed in active power is higher than the estimated maximum active power of this plant ({} > {}). " +
            "Did you provide wrong weather input data?",
          power,
          pRated,
        )
        pRated
      } else power
    }.map(_ * -1)
      .get

    (ActivePowerOperatingPoint(activePower), None)
  }

  override def determineOperatingPoint(
      state: WecState,
      setPower: Power,
  ): ActivePowerOperatingPoint = ActivePowerOperatingPoint(setPower)

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
      data: PrimaryDataWithComplexPower[?],
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

  /** Holds all relevant data for WEC model calculation.
    *
    * @param tick
    *   The current tick.
    * @param weatherData
    *   A map of tick to corresponding weather data. For regular calculations of
    *   the operating point, only the weather data for the current tick is used.
    *   For forecasts, the map needs to contain further data for future ticks.
    */
  final case class WecState(
      override val tick: Long,
      weatherData: SortedMap[Long, AirWeatherData] = SortedMap.empty,
  ) extends ModelState {

    /** Creates states for forecast calculation given the current state.
      *
      * @return
      *   States for forecast calculation.
      */
    def toStateSeries: SortedMap[Long, WecState] = {
      weatherData.map { case (dataTick, _) =>
        val tickState = WecState(
          dataTick,
          weatherData,
        )

        dataTick -> tickState
      }
    }
  }

  object WecState {

    /** Convenience constructor for creating a state for regular operating point
      * calculation at the current point in simulation time.
      *
      * @param tick
      *   The current tick.
      * @param windVelocity
      *   The current wind velocity.
      * @param temperature
      *   The current temperature.
      * @param airPressure
      *   Optionally, the current air pressure.
      * @return
      *   A state for calculation at the current point in simulation time.
      */
    def apply(
        tick: Long,
        windVelocity: Velocity,
        temperature: Temperature,
        airPressure: Option[Pressure],
    ): WecState = WecState(
      tick,
      SortedMap(
        tick -> AirWeatherData(
          windVelocity = windVelocity,
          temperature = temperature,
          airPressure = airPressure,
        )
      ),
    )

  }

  /** Relevant weather data for a specific point in simulation time.
    *
    * @param windVelocity
    *   The current wind velocity.
    * @param temperature
    *   The current temperature.
    * @param airPressure
    *   Optionally, the current air pressure.
    */
  final case class AirWeatherData(
      windVelocity: Velocity,
      temperature: Temperature,
      airPressure: Option[Pressure] = None,
  )

  object AirWeatherData {

    def apply(weatherData: WeatherData): AirWeatherData =
      AirWeatherData(
        windVelocity = weatherData.windVel,
        temperature = weatherData.temp,
      )

  }

  /** This class is initialized with a [[WecCharacteristicInput]], which
    * contains the needed betz curve.
    */
  final case class WecCharacteristic(
      override val xyCoordinates: SortedSet[
        XYPair[Velocity, Dimensionless]
      ]
  ) extends Characteristic[Velocity, Dimensionless]

  object WecCharacteristic {

    import scala.jdk.CollectionConverters.*

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
      WecState(tick)

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
