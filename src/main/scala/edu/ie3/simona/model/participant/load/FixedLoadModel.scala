/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.ParticipantModel
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedState,
  ParticipantFixedState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  EnergyToSimona,
  PowerConversionSimona,
}
import squants.Power
import squants.time.Days

import java.time.ZonedDateTime
import java.util.UUID

class FixedLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val activePower: Power,
) extends LoadModel[FixedState]
    with ParticipantFixedState[ActivePowerOperatingPoint] {

  override def determineOperatingPoint(
      state: FixedState
  ): (ActivePowerOperatingPoint, Option[Long]) =
    (ActivePowerOperatingPoint(activePower), None)

}

object FixedLoadModel {

  final case class Factory(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ) extends ParticipantModelFactory[FixedState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): FixedState = FixedState(tick)

    override def create(): FixedLoadModel = {
      val referenceType = LoadReferenceType(config.reference)

      val sRated = input.getsRated.toApparent

      val activePower: Power = referenceType match {
        case LoadReferenceType.ACTIVE_POWER =>
          sRated.toActivePower(input.getCosPhiRated)
        case LoadReferenceType.ENERGY_CONSUMPTION =>
          val eConsAnnual = input.geteConsAnnual().toSquants
          eConsAnnual / Days(365d)
      }

      new FixedLoadModel(
        input.getUuid,
        input.getId,
        sRated,
        input.getCosPhiRated,
        QControl.apply(input.getqCharacteristics),
        activePower,
      )
    }

  }

}
