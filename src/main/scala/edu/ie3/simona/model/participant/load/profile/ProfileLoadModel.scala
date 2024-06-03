/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.profile.StandardLoadProfile
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference._
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.ProfileRelevantData
import edu.ie3.simona.model.participant.load.{LoadModel, LoadReference}
import edu.ie3.util.scala.OperationInterval
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

/** Power model consuming power according to standard load profiles
  *
  * @param uuid
  *   unique identifier
  * @param id
  *   human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param loadProfile
  *   The load profile to take
  * @param reference
  *   Scale the profiles to this reference
  */
final case class ProfileLoadModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    loadProfile: StandardLoadProfile,
    reference: LoadReference,
) extends LoadModel[ProfileRelevantData](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    ) {

  private val loadProfileStore: LoadProfileStore = LoadProfileStore()

  /* maximum energy throughout the year of the selected load profile*/
  private val profileMaxPower = loadProfileStore.maxPower(loadProfile)

  /* energy reference is always models yearly energy consumption divided by the energy the profile is scaled to */
  private lazy val energyReferenceScalingFactor =
    reference match {
      case EnergyConsumption(energyConsumption) =>
        energyConsumption / LoadProfileStore.defaultLoadProfileEnergyScaling
      case _ =>
        throw new IllegalArgumentException(
          s"Applying energy reference scaling factor for reference mode '$reference' is not supported!"
        )
    }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      modelState: ConstantState.type,
      data: ProfileRelevantData,
  ): Power = {
    /* The power comes in W and is delivered all 15 minutes */
    val averagePower: Power = loadProfileStore
      .entry(data.date, loadProfile)

    reference match {
      case ActivePower(activePower) =>
        /* scale the reference active power based on the profiles averagePower/maxPower ratio */
        val referenceScalingFactor = averagePower / profileMaxPower
        activePower * referenceScalingFactor
      case _: EnergyConsumption =>
        /* scale the profiles average power based on the energyConsumption/profileEnergyScaling(=1000kWh/year) ratio  */
        averagePower * energyReferenceScalingFactor
    }
  }
}

object ProfileLoadModel {

  final case class ProfileRelevantData(date: ZonedDateTime)
      extends LoadRelevantData

  def apply(
      input: LoadInput,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      reference: LoadReference,
  ): ProfileLoadModel = {

    val scaledReference = reference.scale(scalingFactor)
    val scaledInput = input.copy().scale(scalingFactor).build()

    val scaledSRated = scaledReference match {
      case LoadReference.ActivePower(power) =>
        LoadModel.scaleSRatedActivePower(scaledInput, power)

      case LoadReference.EnergyConsumption(energyConsumption) =>
        val loadProfileMax =
          LoadProfileStore().maxPower(
            scaledInput.getLoadProfile.asInstanceOf[StandardLoadProfile]
          )
        LoadModel.scaleSRatedEnergy(
          scaledInput,
          energyConsumption,
          loadProfileMax,
          LoadProfileStore.defaultLoadProfileEnergyScaling,
        )
    }

    val model = ProfileLoadModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      QControl.apply(scaledInput.getqCharacteristics()),
      scaledSRated,
      scaledInput.getCosPhiRated,
      scaledInput.getLoadProfile.asInstanceOf[StandardLoadProfile],
      scaledReference,
    )

    model.enable()
    model
  }
}
