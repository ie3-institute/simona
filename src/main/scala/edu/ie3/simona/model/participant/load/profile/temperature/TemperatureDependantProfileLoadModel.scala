/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile.temperature

import edu.ie3.datamodel.models.profile.{
  StandardLoadProfile,
  TemperatureDependantLoadProfile
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.{
  StandardProfileRelevantData,
  TemperatureProfileRelevantData
}
import edu.ie3.simona.model.participant.load.profile.standard.StandardLoadProfileStore
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power

/** Power model consuming power according to temperature dependant load profiles
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
final case class TemperatureDependantProfileLoadModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    loadProfile: TemperatureDependantLoadProfile,
    reference: LoadReference,
    private val loadProfileStore: TemperatureDependantLoadProfileStore
) extends ProfileLoadModel[
      TemperatureDependantLoadProfile,
      TemperatureProfileRelevantData
    ](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated,
      loadProfile,
      reference,
      loadProfileStore
    ) {

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      data: TemperatureProfileRelevantData
  ): ComparableQuantity[Power] = ???
}
