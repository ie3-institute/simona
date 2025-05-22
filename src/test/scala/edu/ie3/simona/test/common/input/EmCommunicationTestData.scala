/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.datamodel.models.input.system.{LoadInput, PvInput, StorageInput}
import edu.ie3.datamodel.models.input.{EmInput, NodeInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.agent.participant.ParticipantAgentInit.SimulationParameters
import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.geo.GeoUtils
import edu.ie3.util.quantities.QuantityUtils.*
import squants.Each

import java.time.ZonedDateTime
import java.util.UUID

trait EmCommunicationTestData extends DefaultTestData {

  protected implicit val simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
  protected implicit val simulationEnd: ZonedDateTime =
    simulationStart.plusHours(2)

  protected val simonaConfig: SimonaConfig = createSimonaConfig()

  protected val outputConfig: NotifierConfig = NotifierConfig(
    simulationResultInfo = true,
    powerRequestReply = false,
    flexResult = true, // also test FlexOptionsResult if EM-controlled
  )

  protected val simulationParams: SimulationParameters = SimulationParameters(
    expectedPowerRequestTick = Long.MaxValue,
    requestVoltageDeviationTolerance = Each(1e-14d),
    simulationStart = simulationStart,
    simulationEnd = simulationEnd,
  )

  protected val modelConfig: EmRuntimeConfig = EmRuntimeConfig(
    calculateMissingReactivePowerWithModel = false,
    scaling = 1,
    uuids = List.empty,
    aggregateFlex = "SELF_OPT_EXCL_REG",
    curtailRegenerative = false,
  )

  val node3 = new NodeInput(
    UUID.fromString("33f29587-f63e-45b7-960b-037bda37a3cb"),
    "Node_3",
    1.0.asPu,
    false,
    GeoUtils.buildPoint(51.4843281, 7.4116482),
    GermanVoltageLevelUtils.LV,
    2,
  )

  val node4 = new NodeInput(
    UUID.fromString("401f37f8-6f2c-4564-bc78-6736cb9cbf8d"),
    "Node_4",
    1.0.asPu,
    false,
    GeoUtils.buildPoint(51.4843281, 7.4116482),
    GermanVoltageLevelUtils.LV,
    2,
  )

  val emSup = new EmInput(
    UUID.fromString("858f3d3d-4189-49cd-9fe5-3cd49b88dc70"),
    "EM_SUP",
    "PROPORTIONAL",
    null,
  )

  val emNode3 = new EmInput(
    UUID.fromString("fd1a8de9-722a-4304-8799-e1e976d9979c"),
    "emNode3",
    "PRIORITIZED",
    emSup,
  )

  val emNode4 = new EmInput(
    UUID.fromString("ff0b995a-86ff-4f4d-987e-e475a64f2180"),
    "emNode4",
    "PRIORITIZED",
    emSup,
  )

  val pvNode3 = new PvInput(
    UUID.fromString("9d7cd8e2-d859-4f4f-9c01-abba06ef2e2c"),
    "PV_Node_3",
    node3,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,0.9)}"),
    emNode3,
    0.20000000298023224,
    -14.803051948547363.asDegreeGeom,
    96.0.asPercent,
    42.391395568847656.asDegreeGeom,
    0.8999999761581421,
    1.0,
    false,
    10.0.asKiloVoltAmpere,
    0.8999999761581421,
  )

  val pvNode4 = new PvInput(
    UUID.fromString("a1eb7fc1-3bee-4b65-a387-ef3046644bf0"),
    "PV_Node_4",
    node4,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,0.9)}"),
    emNode4,
    0.20000000298023224,
    -8.999500274658203.asDegreeGeom,
    98.0.asPercent,
    37.14517593383789.asDegreeGeom,
    0.8999999761581421,
    1.0,
    false,
    10.0.asKiloVoltAmpere,
    0.8999999761581421,
  )

  val storageType = new StorageTypeInput(
    UUID.fromString("95d4c980-d9e1-4813-9f2a-b0942488a570"),
    "Typ_1",
    0.0.asEuro,
    0.65.asEuroPerKiloWattHour,
    16.0.asKiloWattHour,
    4.166666666666667.asKiloVoltAmpere,
    0.96,
    4.0.asKiloWatt,
    1.0.asPercentPerHour,
    93.0.asPercent,
  )

  val storageInput: StorageInput = new StorageInput(
    UUID.fromString("a2a92cfd-3492-465f-9587-e789f4620af8"),
    "Storage_Node_3",
    node3,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,0.98)}"),
    emNode3,
    storageType,
  )

  val loadInput: LoadInput = new LoadInput(
    UUID.fromString("283a1252-a774-4b04-bfcf-fe8879065982"),
    "Load_Node_4",
    node4,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,1.0)}"),
    emNode4,
    BdewStandardLoadProfile.H0,
    4000.0.asKiloWattHour,
    2.3157899379730225.asKiloVoltAmpere,
    0.949999988079071,
  )
}
