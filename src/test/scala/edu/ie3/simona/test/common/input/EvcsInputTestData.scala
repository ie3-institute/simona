/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointTypeUtils
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.config.RuntimeConfig.SimpleRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.simona.test.common.DefaultTestData

import java.util.UUID

trait EvcsInputTestData extends DefaultTestData with NodeInputTestData {

  protected val evcsInputModel = new EvcsInput(
    UUID.randomUUID(),
    "Dummy_EvcsModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    ChargingPointTypeUtils.ChargingStationType2,
    2,
    0.95,
    EvcsLocationType.HOME,
    true,
    /*fixme mh removed through dev
    true
  )

  protected val simonaConfig: SimonaConfig =
    createSimonaConfig(
      LoadModelBehaviour.FIX,
      LoadReference.ActivePower(Kilowatts(0.0))
    )

  private val configUtil = ConfigUtil.ParticipantConfigUtil(
    simonaConfig.runtime.participant

     */
  )

  protected val evcsStandardModel: EvcsModel = EvcsModel(
    evcsInputModel,
    1.0,
    defaultSimulationStart,
    defaultSimulationEnd,
    "maxPower",
    lowestEvSoc = 0.2,
  )
  /* fixme mh replaced with above
  protected val defaultOutputConfig: ParticipantNotifierConfig =
    ParticipantNotifierConfig(
      simonaConfig.output.participant.defaultConfig.simulationResult,
      simonaConfig.output.participant.defaultConfig.powerRequestReply
    )

   */
/* fixme mh removed
  protected val modelConfig: SimpleRuntimeConfig =
    configUtil.getOrDefault[SimpleRuntimeConfig](
      evcsInputModel.getUuid
    )

  protected implicit val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  protected val simulationEndDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 02:00:00")

 */
}
