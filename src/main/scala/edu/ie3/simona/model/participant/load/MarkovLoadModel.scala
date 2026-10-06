/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  AdditionalFactoryData,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.{
  ParticipantFlexModel,
  ParticipantInflexiblePowerLimitFlexModel,
}
import edu.ie3.simona.model.participant.load.MarkovLoadModel.MarkovLoadModelState
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.SecondaryData.MarkovDataFunction
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.energy.Energy
import squants.{Dimensionless, Power}

import java.time.{Duration, ZonedDateTime}
import java.util.UUID
import scala.jdk.OptionConverters.RichOptional

/** Load model based on a Markov load profile. The state of the Markov chain is
  * kept per load.
  */
class MarkovLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val referenceScalingFactor: Double,
    val seed: Long,
) extends LoadModel[MarkovLoadModelState] {

  override val flexModels: Map[FlexType, ParticipantFlexModel[
    ActivePowerOperatingPoint,
    MarkovLoadModelState,
  ]] =
    Map(
      FlexType.PowerLimit -> ParticipantInflexiblePowerLimitFlexModel(this)
    )

  override def determineOperatingPoint(
      state: MarkovLoadModelState
  ): (ActivePowerOperatingPoint, Option[Long]) =
    (ActivePowerOperatingPoint(state.power * referenceScalingFactor), None)

  override def determineState(
      lastState: MarkovLoadModelState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): MarkovLoadModelState = lastState.copy(tick = tick)

  override def handleInput(
      state: MarkovLoadModelState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): MarkovLoadModelState =
    receivedData
      .collectFirst { case MarkovDataFunction(stepFunction) =>
        val (power, nextMarkovState) = stepFunction(state.markovState, seed)
        state.copy(power = power, markovState = nextMarkovState)
      }
      .getOrElse(state)
}

object MarkovLoadModel {

  /** Period the Markov chain is run before the first time step.
    */
  private val warmUpPeriod: Duration = Duration.ofDays(1)

  /** State to start the warm-up with (lowest load values).
    */
  private val warmUpStartState: Int = 0

  /** Holds all relevant data for the Markov load model calculation.
    *
    * @param tick
    *   The current tick.
    * @param power
    *   The unscaled power of the current interval.
    * @param markovState
    *   The current state of the Markov chain.
    */
  final case class MarkovLoadModelState(
      override val tick: Long,
      power: Power,
      markovState: Int,
  ) extends ModelState

  /** Holds additional data for the Markov load model factory.
    *
    * @param maxPower
    *   The maximal power of the Markov model.
    * @param energyScaling
    *   The energy scaling of the Markov model.
    * @param resolution
    *   The resolution of the Markov model in seconds.
    * @param stepFunction
    *   Function: (time, previous state, seed) => (power, next state).
    */
  final case class MarkovLoadFactoryData(
      maxPower: Option[Power],
      energyScaling: Option[Energy],
      resolution: Long,
      stepFunction: (ZonedDateTime, Int, Long) => (Power, Int),
  ) extends AdditionalFactoryData

  final case class Factory(
      input: LoadInput,
      config: LoadRuntimeConfig,
      factoryData: Option[MarkovLoadFactoryData] = None,
  ) extends ParticipantModelFactory[MarkovLoadModelState] {

    /** Seed of the Markov chain, derived from the uuid of the load.
      */
    val seed: Long =
      input.getUuid.getMostSignificantBits ^ input.getUuid.getLeastSignificantBits

    override def update(
        data: AdditionalFactoryData
    ): Factory = data match {
      case markovData: MarkovLoadFactoryData =>
        copy(factoryData = Some(markovData))

      case unexpected =>
        throw new CriticalFailureException(
          s"Received unexpected data '$unexpected', while updating the Markov load model factory."
        )
    }

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.MarkovLoadProfileService)

    /** Runs the Markov chain for the warm-up period before the simulation start
      * or, if later, the start of operation.
      */
    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): MarkovLoadModelState = {
      val data = getFactoryData
      val steps = warmUpPeriod.getSeconds / data.resolution

      val firstStepTime = input.getOperationTime.getStartDate.toScala
        .filter(_.isAfter(simulationTime))
        .map { operationStart =>
          val secondsUntilStart =
            Duration.between(simulationTime, operationStart).getSeconds
          simulationTime.plusSeconds(
            secondsUntilStart / data.resolution * data.resolution
          )
        }
        .getOrElse(simulationTime)

      val (power, markovState) = Range.Long
        .inclusive(steps, 1, -1)
        .foldLeft((zeroKW, warmUpStartState)) {
          case ((_, previousState), step) =>
            data.stepFunction(
              firstStepTime.minusSeconds(step * data.resolution),
              previousState,
              seed,
            )
        }

      MarkovLoadModelState(tick, power, markovState)
    }

    override def create(): MarkovLoadModel = {
      val data = getFactoryData

      val maxPower = data.maxPower.getOrElse(
        throw new SourceException(
          s"Expected a maximal power value for this Markov load profile: ${input.getLoadProfile}!"
        )
      )

      val (referenceScalingFactor, sRated) =
        LoadReferenceType(config.reference) match {
          case LoadReferenceType.ENERGY_CONSUMPTION =>
            val energyScaling = data.energyScaling.getOrElse(
              throw new CriticalFailureException(
                s"Expected a profile energy scaling value for this Markov load profile: ${input.getLoadProfile}!"
              )
            )

            LoadModel.scaleToReference(
              LoadReferenceType.ENERGY_CONSUMPTION,
              input,
              maxPower,
              energyScaling,
            )

          case LoadReferenceType.ACTIVE_POWER =>
            // max power stems from the training data, only apply config scaling
            (
              config.scaling,
              Kilovoltamperes(
                maxPower.toKilowatts / input.getCosPhiRated
              ) * config.scaling,
            )
        }

      new MarkovLoadModel(
        input.getUuid,
        input.getId,
        sRated,
        input.getCosPhiRated,
        QControl.apply(input.getqCharacteristics()),
        referenceScalingFactor,
        seed,
      )
    }

    private def getFactoryData: MarkovLoadFactoryData =
      factoryData.getOrElse(
        throw new CriticalFailureException(
          s"Expected Markov load factory data for this load profile: ${input.getLoadProfile}!"
        )
      )

  }

}
