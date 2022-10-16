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
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}

/** Power model consuming power according to standard load profiles
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
  * @param loadProfile
  *   The load profile to take
  * @param reference
  *   Scale the profiles to this reference
  */
final case class ProfileLoadModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    loadProfile: StandardLoadProfile,
    reference: LoadReference
) extends LoadModel[ProfileRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private val loadProfileStore: LoadProfileStore = LoadProfileStore()

  /* maximum energy throughout the year of the selected load profile*/
  private val profileMaxPower = loadProfileStore.maxPower(loadProfile)

  /* energy reference is always models yearly energy consumption divided by the energy the profile is scaled to */
  private lazy val energyReferenceScalingFactor =
    reference match {
      case EnergyConsumption(energyConsumption) =>
        energyConsumption
          .divide(LoadProfileStore.defaultLoadProfileEnergyScaling)
          .asType(classOf[Dimensionless])
          .to(PU)
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
      maybeModelState: Option[ConstantState.type],
      data: ProfileRelevantData
  ): ComparableQuantity[Power] = {
    /* The power comes in W and is delivered all 15 minutes */
    val averagePower: ComparableQuantity[Power] = loadProfileStore
      .entry(data.date, loadProfile)

    val activePower = reference match {
      case ActivePower(activePower) =>
        /* scale the reference active power based on the profiles averagePower/maxPower ratio */
        val referenceScalingFactor = averagePower
          .divide(profileMaxPower)
          .asType(classOf[Dimensionless])
          .to(PU)
          .getValue
          .doubleValue()
        activePower.multiply(referenceScalingFactor)
      case _: EnergyConsumption =>
        /* scale the profiles average power based on the energyConsumption/profileEnergyScaling(=1000kWh/year) ratio  */
        averagePower
          .multiply(energyReferenceScalingFactor)
          .asType(classOf[Power])
    }
    activePower.multiply(scalingFactor)
  }
}

object ProfileLoadModel {

  final case class ProfileRelevantData(date: ZonedDateTime)
      extends LoadRelevantData

  def apply(
      input: LoadInput,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      reference: LoadReference
  ): ProfileLoadModel = reference match {
    case LoadReference.ActivePower(power) =>
      val sRatedPowerScaled = LoadModel.scaleSRatedActivePower(input, power)
      ProfileLoadModel(
        input.getUuid,
        input.getId,
        operationInterval,
        scalingFactor,
        QControl.apply(input.getqCharacteristics()),
        sRatedPowerScaled,
        input.getCosPhiRated,
        input.getLoadProfile.asInstanceOf[StandardLoadProfile],
        reference
      )

    case LoadReference.EnergyConsumption(energyConsumption) =>
      val loadProfileMax =
        LoadProfileStore().maxPower(
          input.getLoadProfile.asInstanceOf[StandardLoadProfile]
        )
      val sRatedEnergy = LoadModel.scaleSRatedEnergy(
        input,
        energyConsumption,
        loadProfileMax,
        LoadProfileStore.defaultLoadProfileEnergyScaling
      )
      ProfileLoadModel(
        input.getUuid,
        input.getId,
        operationInterval,
        scalingFactor,
        QControl.apply(input.getqCharacteristics()),
        sRatedEnergy,
        input.getCosPhiRated,
        input.getLoadProfile.asInstanceOf[StandardLoadProfile],
        reference
      )
  }
}
