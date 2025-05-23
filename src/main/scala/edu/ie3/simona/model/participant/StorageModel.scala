/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.result.system.{
  StorageResult,
  SystemParticipantResult,
}
import edu.ie3.simona.config.RuntimeConfig.StorageRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.StorageModel.{
  RefTargetSocParams,
  StorageState,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.QuantityUtils.{asPu, asMegaWatt, asMegaVar}
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  DimensionlessToSimona,
  EnergyToSimona,
  PowerConversionSimona,
}
import squants.{Dimensionless, Energy, Power, Seconds}

import java.time.ZonedDateTime
import java.util.UUID

class StorageModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    eStorage: Energy,
    pMax: Power,
    eta: Dimensionless,
    targetSoc: Option[Double],
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      StorageState,
    ] {

  private val minEnergy = zeroKWh

  /** Tolerance for power comparisons. With very small (dis-)charging powers,
    * problems can occur when calculating the future tick at which storage is
    * full or empty. For sufficiently large time frames, the maximum Long value
    * ([[Long.MaxValue]]) can be exceeded, thus the Long value overflows, and we
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

  /** Minimal allowed energy with tolerance margin added.
    */
  private val minEnergyWithMargin: Energy =
    minEnergy + (toleranceMargin / eta.toEach)

  /** Maximum allowed energy with tolerance margin added.
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

  override def determineState(
      lastState: StorageState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): StorageState = {
    val currentEnergy = ChargingHelper.calcEnergy(
      lastState.storedEnergy,
      operatingPoint.activePower,
      lastState.tick,
      tick,
      eStorage,
      minEnergy,
      eta,
    )

    StorageState(currentEnergy, tick)
  }

  override def determineOperatingPoint(
      state: StorageState
  ): (ActivePowerOperatingPoint, Option[Long]) =
    throw new CriticalFailureException(
      "Storage model cannot calculate operation point without flexibility control."
    )

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: StorageState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
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
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new StorageResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
      // Stored energy currently not supported by primary data time series
      -1.asPu,
    )

  override def determineFlexOptions(
      state: StorageState
  ): FlexOptions = {

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
          pMax * -1d
        }
      }
      .getOrElse {
        // no target set
        zeroKW
      }

    MinMaxFlexOptions(
      refPower,
      if (dischargingPossible) pMax * -1 else zeroKW,
      if (chargingPossible) pMax else zeroKW,
    )
  }

  override def determineOperatingPoint(
      state: StorageState,
      setPower: Power,
  ): (ActivePowerOperatingPoint, ParticipantModel.OperationChangeIndicator) = {
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

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(state.storedEnergy) || isFull(state.storedEnergy)
    // if target soc is enabled, we can also be at that exact point
    val isAtTarget = refTargetSoc.exists { targetParams =>
      state.storedEnergy <= targetParams.targetWithPosMargin &&
      state.storedEnergy >= targetParams.targetWithNegMargin
    }
    val isChargingOrDischarging = adaptedSetPower != zeroKW
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set the power to zero (see above) and also want to refresh flex options
    // at the next activation.
    // Similarly, if the ref target margin area is hit before hitting target SOC, we want
    // to refresh flex options.
    val hasObsoleteFlexOptions =
      (isFull(state.storedEnergy) && setPower > zeroKW) ||
        (isEmpty(state.storedEnergy) && setPower < zeroKW) ||
        (isAtTarget && setPower != zeroKW)

    val activateAtNextTick =
      ((isEmptyOrFull || isAtTarget) && isChargingOrDischarging) || hasObsoleteFlexOptions

    // when charging, calculate time until we're full or at target energy
    val chargingEnergyTarget = () =>
      refTargetSoc
        .filter(_.targetWithNegMargin >= state.storedEnergy)
        .map(_.targetSoc)
        .getOrElse(eStorage)

    // when discharging, calculate time until we're at lowest energy allowed or at target energy
    val dischargingEnergyTarget = () =>
      refTargetSoc
        .filter(_.targetWithPosMargin <= state.storedEnergy)
        .map(_.targetSoc)
        .getOrElse(minEnergy)

    // calculate the tick from time span
    val maybeNextTick = ChargingHelper.calcNextEventTick(
      state.storedEnergy,
      adaptedSetPower,
      state.tick,
      chargingEnergyTarget,
      dischargingEnergyTarget,
      eta,
    )

    (
      ActivePowerOperatingPoint(adaptedSetPower),
      ParticipantModel.OperationChangeIndicator(
        activateAtNextTick,
        maybeNextTick,
      ),
    )
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

}

object StorageModel {

  /** @param storedEnergy
    *   The amount of currently stored energy
    * @param tick
    *   The tick at which this state is valid
    */
  final case class StorageState(
      storedEnergy: Energy,
      tick: Long,
  ) extends ModelState

  /** @param targetSoc
    *   The SOC that the StorageModel aims at, i.e. that it prefers to
    *   charge/discharge towards
    * @param targetWithPosMargin
    *   The targetSoc plus a tolerance margin
    * @param targetWithNegMargin
    *   The targetSoc minus a tolerance margin
    */
  final case class RefTargetSocParams(
      targetSoc: Energy,
      targetWithPosMargin: Energy,
      targetWithNegMargin: Energy,
  )

  final case class Factory(
      input: StorageInput,
      config: StorageRuntimeConfig,
  ) extends ParticipantModelFactory[StorageState] {

    private val eStorage = input.getType.geteStorage.toSquants

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): StorageState = {
      val initialStoredEnergy = eStorage * config.initialSoc
      StorageState(storedEnergy = initialStoredEnergy, tick)
    }

    override def create(): StorageModel =
      new StorageModel(
        input.getUuid,
        input.getId,
        input.getType.getsRated.toApparent,
        input.getType.getCosPhiRated,
        QControl.apply(input.getqCharacteristics),
        eStorage,
        input.getType.getpMax.toSquants,
        input.getType.getEta.toSquants,
        config.targetSoc,
      )

  }

}
