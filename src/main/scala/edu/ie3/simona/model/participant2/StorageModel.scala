/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.result.system.{
  StorageResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.StorageModel.RefTargetSocParams
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  OperationRelevantData,
}
import edu.ie3.simona.model.participant2.StorageModel.{
  StorageRelevantData,
  StorageState,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWH}
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Dimensionless, Each, Energy, Power, Seconds}

import java.time.ZonedDateTime
import java.util.UUID

class StorageModel private (
    override val uuid: UUID,
    override val sRated: Power,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    eStorage: Energy,
    pMax: Power,
    eta: Dimensionless,
    targetSoc: Option[Double],
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      StorageState,
      StorageRelevantData,
    ] {

  private val minEnergy = zeroKWH

  /** Tolerance for power comparisons. With very small (dis-)charging powers,
    * problems can occur when calculating the future tick at which storage is
    * full or empty. For sufficiently large time frames, the maximum Long value
    * ([[Long.MaxValue]]) can be exceeded, thus the Long value overflows and we
    * get undefined behavior.
    *
    * Thus, small (dis-)charging powers compared to storage capacity have to be
    * set to zero. The given tolerance value below amounts to 1 W for 1 GWh
    * storage capacity and is sufficient in preventing Long overflows.
    */
  private implicit val powerTolerance: Power = eStorage / Seconds(1) / 3.6e12

  /** In order to avoid faulty behavior of storages, we want to avoid offering
    * charging/discharging when storage is very close to full, to empty or to a
    * target.
    *
    * In particular, we want to avoid offering the option to (dis-)charge if
    * that operation could last less than our smallest possible time step, which
    * is one second. Thus, we establish a safety margin of the energy
    * (dis-)charged with maximum power in one second.
    */
  private val toleranceMargin: Energy = pMax * Seconds(1d)

  /** Minimal allowed energy with tolerance margin added
    */
  private val minEnergyWithMargin: Energy =
    minEnergy + (toleranceMargin / eta.toEach)

  /** Maximum allowed energy with tolerance margin added
    */
  private val maxEnergyWithMargin: Energy =
    eStorage - (toleranceMargin * eta.toEach)

  private val refTargetSoc: Option[RefTargetSocParams] = targetSoc.map {
    target =>
      val targetEnergy = eStorage * target

      val targetWithPosMargin =
        targetEnergy + (toleranceMargin / eta.toEach)

      val targetWithNegMargin =
        targetEnergy - (toleranceMargin * eta.toEach)

      RefTargetSocParams(
        targetEnergy,
        targetWithPosMargin,
        targetWithNegMargin,
      )
  }

  override def determineOperatingPoint(
      state: StorageState,
      relevantData: StorageRelevantData,
  ): (ActivePowerOperatingPoint, Option[Long]) =
    throw new CriticalFailureException(
      "Storage model cannot calculate operation point without flexibility control."
    )

  override def determineState(
      lastState: StorageState,
      operatingPoint: ActivePowerOperatingPoint,
      currentTick: Long,
  ): StorageState = {
    val timespan = Seconds(currentTick - lastState.tick)
    val netPower = calcNetPower(operatingPoint.activePower)
    val energyChange = netPower * timespan

    // don't allow under- or overcharge e.g. due to tick rounding error
    val currentEnergy =
      minEnergy.max(eStorage.min(lastState.storedEnergy + energyChange))

    StorageState(currentEnergy, currentTick)
  }

  override def createResults(
      state: StorageState,
      operatingPoint: ActivePowerOperatingPoint,
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new StorageResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
        (state.storedEnergy / eStorage).asPu,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithApparentPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new StorageResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
      (-1).asPu, // FIXME currently not supported
    )

  override def getRequiredServices: Iterable[ServiceType] = Iterable.empty

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
  ): StorageRelevantData = {
    if (receivedData.nonEmpty) {
      throw new CriticalFailureException(
        s"Expected no received data, got $receivedData"
      )
    }

    StorageRelevantData(tick)
  }

  override def calcFlexOptions(
      state: StorageState,
      relevantData: StorageRelevantData,
  ): FlexibilityMessage.ProvideFlexOptions = {

    val chargingPossible = !isFull(state.storedEnergy)
    val dischargingPossible = !isEmpty(state.storedEnergy)

    val refPower = refTargetSoc
      .map { targetParams =>
        if (state.storedEnergy <= targetParams.targetWithPosMargin) {
          if (state.storedEnergy >= targetParams.targetWithNegMargin) {
            // is within target +/- margin, no charging needed
            zeroKW
          } else {
            // below target - margin, charge up to target
            pMax
          }
        } else {
          // above target + margin, discharge to target
          pMax * (-1d)
        }
      }
      .getOrElse {
        // no target set
        zeroKW
      }

    ProvideMinMaxFlexOptions(
      uuid,
      refPower,
      if (dischargingPossible) pMax * (-1) else zeroKW,
      if (chargingPossible) pMax else zeroKW,
    )
  }

  override def handlePowerControl(
      state: StorageState,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (ActivePowerOperatingPoint, ParticipantModel.ModelChangeIndicator) = {
    val adaptedSetPower =
      if (
        // if power is close to zero, set it to zero
        (setPower ~= zeroKW)
        // do not keep charging if we're already full (including safety margin)
        || (setPower > zeroKW && isFull(state.storedEnergy))
        // do not keep discharging if we're already empty (including safety margin)
        || (setPower < zeroKW && isEmpty(state.storedEnergy))
      )
        zeroKW
      else
        setPower

    // net power after considering efficiency
    val netPower = calcNetPower(adaptedSetPower)

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(state.storedEnergy) || isFull(state.storedEnergy)
    // if target soc is enabled, we can also be at that exact point
    val isAtTarget = refTargetSoc.exists { targetParams =>
      state.storedEnergy <= targetParams.targetWithPosMargin &&
      state.storedEnergy >= targetParams.targetWithNegMargin
    }
    val isChargingOrDischarging = netPower != zeroKW
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set netPower to zero (see above) and also want to refresh flex options
    // at the next activation.
    // Similarly, if the ref target margin area is hit before hitting target SOC, we want
    // to refresh flex options.
    val hasObsoleteFlexOptions =
      (isFull(state.storedEnergy) && setPower > zeroKW) ||
        (isEmpty(state.storedEnergy) && setPower < zeroKW) ||
        (isAtTarget && setPower != zeroKW)

    val activateAtNextTick =
      ((isEmptyOrFull || isAtTarget) && isChargingOrDischarging) || hasObsoleteFlexOptions

    // calculate the time span until we're full or empty, if applicable
    val maybeTimeSpan =
      if (!isChargingOrDischarging) {
        // we're at 0 kW, do nothing
        None
      } else if (netPower > zeroKW) {
        // we're charging, calculate time until we're full or at target energy

        val closestEnergyTarget = refTargetSoc
          .flatMap { targetParams =>
            Option.when(
              state.storedEnergy <= targetParams.targetWithNegMargin
            )(targetParams.targetSoc)
          }
          .getOrElse(eStorage)

        val energyToFull = closestEnergyTarget - state.storedEnergy
        Some(energyToFull / netPower)
      } else {
        // we're discharging, calculate time until we're at lowest energy allowed or at target energy

        val closestEnergyTarget = refTargetSoc
          .flatMap { targetParams =>
            Option.when(
              state.storedEnergy >= targetParams.targetWithPosMargin
            )(targetParams.targetSoc)
          }
          .getOrElse(minEnergy)

        val energyToEmpty = state.storedEnergy - closestEnergyTarget
        Some(energyToEmpty / (netPower * (-1)))
      }

    // calculate the tick from time span
    val maybeNextTick = maybeTimeSpan.map { timeSpan =>
      val timeSpanTicks = Math.round(timeSpan.toSeconds)
      state.tick + timeSpanTicks
    }

    (
      ActivePowerOperatingPoint(adaptedSetPower),
      ParticipantModel.ModelChangeIndicator(activateAtNextTick, maybeNextTick),
    )
  }

  private def calcNetPower(setPower: Power): Power =
    if (setPower > zeroKW) {
      // multiply eta if we're charging
      setPower * eta.toEach
    } else {
      // divide by eta if we're discharging
      // (draining the battery more than we get as output)
      setPower / eta.toEach
    }

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(storedEnergy: Energy): Boolean =
    storedEnergy >= maxEnergyWithMargin

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is less than the minimal charged energy
    *   allowed (plus a tolerance margin)
    */
  private def isEmpty(storedEnergy: Energy): Boolean =
    storedEnergy <= minEnergyWithMargin

  def getInitialState(config: StorageRuntimeConfig): StorageState = {
    val initialStorage = eStorage * config.initialSoc
    StorageState(storedEnergy = initialStorage, -1L)
  }
}

object StorageModel {
  final case class StorageRelevantData(
      currentTick: Long
  ) extends OperationRelevantData

  /** @param storedEnergy
    *   The amount of currently stored energy
    * @param tick
    *   The tick at which this state is valid
    */
  final case class StorageState(
      storedEnergy: Energy,
      tick: Long,
  ) extends ModelState

  def apply(
      inputModel: StorageInput,
      config: StorageRuntimeConfig,
  ): StorageModel =
    new StorageModel(
      inputModel.getUuid,
      Kilowatts(
        inputModel.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getType.getCosPhiRated,
      QControl.apply(inputModel.getqCharacteristics),
      KilowattHours(
        inputModel.getType.geteStorage
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue
      ),
      Kilowatts(
        inputModel.getType.getpMax
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      Each(
        inputModel.getType.getEta.to(PowerSystemUnits.PU).getValue.doubleValue
      ),
      config.targetSoc,
    )
}
