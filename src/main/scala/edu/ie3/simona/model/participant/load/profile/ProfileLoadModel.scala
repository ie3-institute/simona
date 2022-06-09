/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.datamodel.models.profile.{
  LoadProfile,
  StandardLoadProfile,
  TemperatureDependantLoadProfile
}
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference._
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.{
  ProfileRelevantData,
  StandardProfileRelevantData,
  TemperatureProfileRelevantData
}
import edu.ie3.simona.model.participant.load.profile.standard.{
  StandardLoadProfileStore,
  StandardProfileLoadModel
}
import edu.ie3.simona.model.participant.load.profile.temperature.{
  TemperatureDependantLoadProfileStore,
  TemperatureDependantProfileLoadModel
}
import edu.ie3.simona.model.participant.load.{LoadModel, LoadReference}
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power, Temperature}

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
abstract class ProfileLoadModel[P <: LoadProfile, D <: ProfileRelevantData](
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    loadProfile: P,
    reference: LoadReference,
    protected val loadProfileStore: LoadProfileStore[P]
) extends LoadModel[D](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {}

case object ProfileLoadModel {

  sealed trait ProfileRelevantData extends LoadRelevantData {
    def date: ZonedDateTime
  }

  final case class StandardProfileRelevantData(date: ZonedDateTime)
      extends ProfileRelevantData

  final case class TemperatureProfileRelevantData(
      date: ZonedDateTime,
      averageTemperature: ComparableQuantity[Temperature]
  ) extends ProfileRelevantData

  def buildProfileLoadModel(
      input: LoadInput,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      reference: LoadReference
  ): ProfileLoadModel[_ <: LoadProfile, _ <: ProfileRelevantData] = {

    val sRated = reference match {
      case LoadReference.ActivePower(power) =>
        LoadModel.scaleSRatedActivePower(input, power)
      case LoadReference.EnergyConsumption(energyConsumption) =>
        val loadProfileMax = StandardLoadProfileStore().maxPower(
          input.getLoadProfile.asInstanceOf[StandardLoadProfile]
        )
        LoadModel.scaleSRatedEnergy(
          input,
          energyConsumption,
          loadProfileMax,
          StandardLoadProfileStore.defaultLoadProfileEnergyScaling
        )
    }
    input.getLoadProfile match {
      case profile: StandardLoadProfile =>
        StandardProfileLoadModel(
          input.getUuid,
          input.getId,
          operationInterval,
          scalingFactor,
          QControl.apply(input.getqCharacteristics()),
          sRated,
          input.getCosPhiRated,
          profile,
          reference,
          StandardLoadProfileStore()
        )
      case profile: TemperatureDependantLoadProfile =>
        TemperatureDependantProfileLoadModel(
          input.getUuid,
          input.getId,
          operationInterval,
          scalingFactor,
          QControl.apply(input.getqCharacteristics()),
          sRated,
          input.getCosPhiRated,
          profile,
          reference,
          TemperatureDependantLoadProfileStore()
        )
      case unknown =>
        throw new IllegalArgumentException(
          s"Can not build a ProfileLoadModel with load profile $unknown"
        )
    }

  }
}
