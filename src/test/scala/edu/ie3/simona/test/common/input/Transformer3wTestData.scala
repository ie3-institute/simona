/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import breeze.math.Complex
import edu.ie3.datamodel.models.input.connector.`type`.Transformer3WTypeInput
import edu.ie3.datamodel.models.input.connector.{
  LineInput,
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput
}
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  RawGridElements
}
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.grid.{RefSystem, Transformer3wModel}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.quantities.PowerSystemUnits._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.{TableFor2, TableFor4}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{OHM, PERCENT, SIEMENS}

import java.util.UUID
import javax.measure.MetricPrefix
import scala.jdk.CollectionConverters._

/** Test data for a [[Transformer3WInput]]
  *
  * id = HöS-HS-MS_1 v_a = 380 kV v_b = 110 kV v_c = 20 kV s_rated_a = 120.000
  * MVA s_rated_b = 60.000 MVA s_rated _c = 40.000 MVA
  * --> s_Ref = 120.000 MVA, v_Ref = 380 kV --> Z_Ref = 1.203,3333 Ω, Y_Ref =
  * 831.02493 nS
  *
  * r_sc_a 0.300 Ω --> 249.30755E-6 p.u. r_sc_b = 0.2983 Ω --> 247.8947E-6 p.u.
  * r_sc_c = 0.2888 Ω --> 240.0000E-6 p.u. x_sc_a = 1.00000 Ω --> 831.02495E-6
  * p.u. x_sc_b = 0.954711 Ω --> 793.38866E-6 p.u. x_sc_c = 1.083000 Ω -->
  * 900.00002E-6 p.u. g_m = 40 nS --> 48.1333E-3 p.u. b_m = 1 nS --> 1.2033E-4
  * p.u. tap_max = 10 tap_neutr = 0 tap_min = -10 d_phi = 0 °/tap d_V = 1.5 % /
  * tap capex = 100.000 € opex = 0 €
  */
trait Transformer3wTestData extends DefaultTestData {
  val mainRefSystemEhv: RefSystem = {
    val nominalPower = Quantities.getQuantity(1000, MEGAVOLTAMPERE)
    val nominalVoltage = Quantities.getQuantity(380, KILOVOLT)
    RefSystem(nominalPower, nominalVoltage)
  }

  val mainRefSystemHv: RefSystem = {
    val nominalPower = Quantities.getQuantity(60, MEGAVOLTAMPERE)
    val nominalVoltage = Quantities.getQuantity(110, KILOVOLT)
    RefSystem(nominalPower, nominalVoltage)
  }

  val mainRefSystemLv: RefSystem = {
    val nominalPower = Quantities.getQuantity(400, KILOVOLTAMPERE)
    val nominalVoltage = Quantities.getQuantity(20, KILOVOLT)
    RefSystem(nominalPower, nominalVoltage)
  }

  private val nodeA = new NodeInput(
    UUID.fromString("52ec3d30-635a-4b29-9335-d94e068d5818"),
    "nodeA",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    true,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.EHV_380KV,
    1
  )
  private val nodeB = new NodeInput(
    UUID.fromString("dd037896-d2e0-4cdd-a4a1-c9e2b5e8366a"),
    "nodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.HV,
    2
  )
  private val nodeC = new NodeInput(
    UUID.fromString("c838f8a5-03d4-40d3-94fa-815e1bcd0aa0"),
    "nodeC",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    Quantities.getQuantity(1d, PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.MV_20KV,
    3
  )

  protected val transformer3wType = new Transformer3WTypeInput(
    UUID.randomUUID(),
    "HöS-HS-MS_1",
    Quantities.getQuantity(120d, MEGAVOLTAMPERE),
    Quantities.getQuantity(60d, MEGAVOLTAMPERE),
    Quantities.getQuantity(40d, MEGAVOLTAMPERE),
    Quantities.getQuantity(380d, KILOVOLT),
    Quantities.getQuantity(110d, KILOVOLT),
    Quantities.getQuantity(20d, KILOVOLT),
    Quantities.getQuantity(0.3d, OHM),
    Quantities.getQuantity(0.2983d, OHM),
    Quantities.getQuantity(0.2888d, OHM),
    Quantities.getQuantity(1d, OHM),
    Quantities.getQuantity(0.954711d, OHM),
    Quantities.getQuantity(1.083000d, OHM),
    Quantities.getQuantity(40d, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(-1d, MetricPrefix.NANO(SIEMENS)),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    0,
    -10,
    10
  )

  protected val transformer3wInput: Transformer3WInput = new Transformer3WInput(
    UUID.fromString("c81f58d8-0c80-4fa6-b925-f6999014a110"),
    "testTransformer3W",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeA,
    nodeB,
    nodeC,
    1,
    transformer3wType,
    0,
    false
  )

  protected val transformer3wInputPostponed: Transformer3WInput =
    new Transformer3WInput(
      UUID.fromString("ca6d9ccf-7e2b-4837-a2da-4ef678e27cbb"),
      "testTransformer3W",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      postponedOperationTime,
      nodeA,
      nodeB,
      nodeC,
      1,
      transformer3wType,
      0,
      false
    )

  protected val transformer3wInputTapped: Transformer3WInput =
    new Transformer3WInput(
      UUID.fromString("f1626716-31da-4dad-b3e5-851fcecdcfcc"),
      "testTransformer3W",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      defaultOperationTime,
      nodeA,
      nodeB,
      nodeC,
      1,
      transformer3wType,
      10,
      false
    )

  protected def transformerModelEhv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemEhv,
      1,
      defaultSimulationStart,
      defaultSimulationEnd
    )

  protected def transformerModelHv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemHv,
      2,
      defaultSimulationStart,
      defaultSimulationEnd
    )

  protected def transformerModelLv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemHv,
      3,
      defaultSimulationStart,
      defaultSimulationEnd
    )

  protected val transformer3wTestGrid: JointGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB, nodeC).asJava,
      Set.empty[LineInput].asJava,
      Set.empty[Transformer2WInput].asJava,
      Set(transformer3wInput).asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava
    )
    TestGridFactory.createJointGrid(
      gridName = "transformer3WTestGrid",
      rawGridElements = rawGridElements
    )
  }

  protected val tapDependentEquivalentCircuitParametersEhv
      : TableFor4[Int, Complex, Complex, Complex] = Table(
    ("tapPos", "yij", "yii", "yjj"),
    (
      -10,
      Complex(28.7143597498711, -95.7146707115102),
      Complex(0, 0),
      Complex(5.96146454759955, -19.8715580775854)
    ),
    (
      -9,
      Complex(29.7367499292005, -99.1226428970515),
      Complex(0, 0),
      Complex(5.36531867043959, -17.8844022842669)
    ),
    (
      -8,
      Complex(30.7770244848446, -102.590229756392),
      Complex(0, 0),
      Complex(4.76917279327964, -15.8972464909483)
    ),
    (
      -7,
      Complex(31.8351834168035, -106.117431289533),
      Complex(0, 0),
      Complex(4.17302691611968, -13.9100906976298)
    ),
    (
      -6,
      Complex(32.9112267250772, -109.704247496473),
      Complex(0, 0),
      Complex(3.57688103895973, -11.9229349043112)
    ),
    (
      -5,
      Complex(34.0051544096657, -113.350678377212),
      Complex(0, 0),
      Complex(2.98073516179977, -9.93577911099265)
    ),
    (
      -4,
      Complex(35.1169664705691, -117.056723931751),
      Complex(0, 0),
      Complex(2.38458928463982, -7.94862331767411)
    ),
    (
      -3,
      Complex(36.2466629077872, -120.82238416009),
      Complex(0, 0),
      Complex(1.78844340747987, -5.96146752435556)
    ),
    (
      -2,
      Complex(37.3942437213201, -124.647659062228),
      Complex(0, 0),
      Complex(1.19229753031991, -3.97431173103701)
    ),
    (
      -1,
      Complex(38.5597089111678, -128.532548638166),
      Complex(0, 0),
      Complex(0.596151653159955, -1.98715593771845)
    ),
    (
      0,
      Complex(39.7430584773303, -132.477052887903),
      Complex(0, 0),
      Complex(0.000005776, -0.0000001443999)
    ),
    (
      1,
      Complex(40.9442924198076, -136.48117181144),
      Complex(0, 0),
      Complex(-0.596140101159951, 1.98715564891864)
    ),
    (
      2,
      Complex(42.1634107385997, -140.544905408777),
      Complex(0, 0),
      Complex(-1.19228597831991, 3.97431144223721)
    ),
    (
      3,
      Complex(43.4004134337066, -144.668253679913),
      Complex(0, 0),
      Complex(-1.78843185547986, 5.96146723555574)
    ),
    (
      4,
      Complex(44.6553005051283, -148.851216624848),
      Complex(0, 0),
      Complex(-2.38457773263982, 7.94862302887431)
    ),
    (
      5,
      Complex(45.9280719528648, -153.093794243583),
      Complex(0, 0),
      Complex(-2.98072360979977, 9.93577882219285)
    ),
    (
      6,
      Complex(47.2187277769161, -157.395986536118),
      Complex(0, 0),
      Complex(-3.57686948695973, 11.9229346155114)
    ),
    (
      7,
      Complex(48.5272679772822, -161.757793502452),
      Complex(0, 0),
      Complex(-4.17301536411968, 13.91009040883)
    ),
    (
      8,
      Complex(49.8536925539631, -166.179215142586),
      Complex(0, 0),
      Complex(-4.76916124127964, 15.8972462021485)
    ),
    (
      9,
      Complex(51.1980015069588, -170.660251456519),
      Complex(0, 0),
      Complex(-5.36530711843959, 17.8844019954671)
    ),
    (
      10,
      Complex(52.5601948362693, -175.200902444252),
      Complex(0, 0),
      Complex(-5.96145299559954, 19.8715577887856)
    )
  )

  val tapDependentVoltRatioEhv: TableFor2[Int, String] = Table(
    ("tapPos", "voltRatio"),
    (-10, "0.85"),
    (-9, "0.865"),
    (-8, "0.88"),
    (-7, "0.895"),
    (-6, "0.91"),
    (-5, "0.925"),
    (-4, "0.94"),
    (-3, "0.955"),
    (-2, "0.97"),
    (-1, "0.985"),
    (0, "1"),
    (1, "1.015"),
    (2, "1.03"),
    (3, "1.045"),
    (4, "1.06"),
    (5, "1.075"),
    (6, "1.09"),
    (7, "1.105"),
    (8, "1.12"),
    (9, "1.135"),
    (10, "1.15")
  )
}
