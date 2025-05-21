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
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  RawGridElements,
}
import edu.ie3.datamodel.models.input.{
  MeasurementUnitInput,
  NodeInput,
  OperatorInput,
}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.grid.{RefSystem, Transformer3wModel}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.quantities.PowerSystemUnits.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.{TableFor2, TableFor4}
import squants.electro.Kilovolts
import squants.energy.{Kilowatts, Megawatts}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{OHM, PERCENT}

import java.util.UUID
import scala.jdk.CollectionConverters.*

/** Test data for a [[Transformer3WInput]] */
trait Transformer3wTestData extends DefaultTestData {
  val mainRefSystemEhv: RefSystem = {
    val nominalPower = Megawatts(1000d)
    val nominalVoltage = Kilovolts(380d)
    RefSystem(nominalPower, nominalVoltage)
  }

  val mainRefSystemHv: RefSystem = {
    val nominalPower = Megawatts(60d)
    val nominalVoltage = Kilovolts(110d)
    RefSystem(nominalPower, nominalVoltage)
  }

  val mainRefSystemLv: RefSystem = {
    val nominalPower = Kilowatts(400d)
    val nominalVoltage = Kilovolts(20d)
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
    1,
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
    2,
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
    3,
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
    Quantities.getQuantity(0.15d, OHM),
    Quantities.getQuantity(0.58d, OHM),
    Quantities.getQuantity(1.15d, OHM),
    Quantities.getQuantity(24.02d, OHM),
    Quantities.getQuantity(60.15d, OHM),
    Quantities.getQuantity(200.75d, OHM),
    Quantities.getQuantity(12.98d, NANOSIEMENS),
    Quantities.getQuantity(-519.48d, NANOSIEMENS),
    Quantities.getQuantity(1.5, PERCENT),
    Quantities.getQuantity(0d, DEGREE_GEOM),
    0,
    -10,
    10,
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
    false,
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
      false,
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
      false,
    )

  protected def transformerModelEhv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemEhv,
      1,
      defaultSimulationStart,
      defaultSimulationEnd,
    )

  protected def transformerModelHv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemHv,
      2,
      defaultSimulationStart,
      defaultSimulationEnd,
    )

  protected def transformerModelLv: Transformer3wModel =
    Transformer3wModel(
      transformer3wInput,
      mainRefSystemHv,
      3,
      defaultSimulationStart,
      defaultSimulationEnd,
    )

  protected val transformer3wTestGrid: JointGridContainer = {
    val rawGridElements = new RawGridElements(
      Set(nodeA, nodeB, nodeC).asJava,
      Set.empty[LineInput].asJava,
      Set.empty[Transformer2WInput].asJava,
      Set(transformer3wInput).asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )
    TestGridFactory.createJointGrid(
      gridName = "transformer3WTestGrid",
      rawGridElements = rawGridElements,
    )
  }

  protected val tapDependentEquivalentCircuitParametersEhv
      : TableFor4[Int, Complex, Complex, Complex] = Table(
    ("tapPos", "yij", "yii", "yjj"),
    (
      -10,
      Complex(0.0441648321658824, -7.07226179085529),
      Complex.zero,
      Complex(-0.00662285051288235, 1.06076425571629),
    ),
    (
      -9,
      Complex(0.0433989680242775, -6.94962141297919),
      Complex.zero,
      Complex(-0.00585698637127746, 0.938123877840191),
    ),
    (
      -8,
      Complex(0.0426592128875, -6.83116195707614),
      Complex.zero,
      Complex(-0.0051172312345, 0.819664421937137),
    ),
    (
      -7,
      Complex(0.0419442540122905, -6.71667320919218),
      Complex.zero,
      Complex(-0.0044022723592905, 0.705175674053178),
    ),
    (
      -6,
      Complex(0.0412528652098901, -6.60595881563407),
      Complex.zero,
      Complex(-0.00371088355689011, 0.594461280495065),
    ),
    (
      -5,
      Complex(0.0405838998281081, -6.49883515916432),
      Complex.zero,
      Complex(-0.0030419181751081, 0.487337624025323),
    ),
    (
      -4,
      Complex(0.0399362844053192, -6.39513034279468),
      Complex.zero,
      Complex(-0.00239430275231915, 0.383632807655681),
    ),
    (
      -3,
      Complex(0.0393090129225131, -6.29468326934764),
      Complex.zero,
      Complex(-0.00176703126951309, 0.283185734208644),
    ),
    (
      -2,
      Complex(0.0387011415886598, -6.19734280641959),
      Complex.zero,
      Complex(-0.0011591599356598, 0.185845271280588),
    ),
    (
      -1,
      Complex(0.0381117841025381, -6.10296702764162),
      Complex.zero,
      Complex(-0.000569802449538069, 0.091469492502624),
    ),
    (
      0,
      Complex(0.037540107341, -6.011422522227),
      Complex.zero,
      Complex(0.000001874312, -0.000075012912),
    ),
    (
      1,
      Complex(0.0369853274295567, -5.92258376574089),
      Complex.zero,
      Complex(0.000556654223443345, -0.0889137693981125),
    ),
    (
      2,
      Complex(0.0364467061563107, -5.83633254585146),
      Complex.zero,
      Complex(0.00109527549668932, -0.175164989287544),
    ),
    (
      3,
      Complex(0.0359235476947368, -5.7525574375378),
      Complex.zero,
      Complex(0.00161843395826315, -0.2589400976012),
    ),
    (
      4,
      Complex(0.035415195604717, -5.67115332285566),
      Complex.zero,
      Complex(0.00212678604828302, -0.34034421228334),
    ),
    (
      5,
      Complex(0.0349210300846512, -5.59202095090884),
      Complex.zero,
      Complex(0.00262095156834884, -0.419476584230163),
    ),
    (
      6,
      Complex(0.0344404654504587, -5.51506653415321),
      Complex.zero,
      Complex(0.00310151620254129, -0.496431000985789),
    ),
    (
      7,
      Complex(0.0339729478199095, -5.440201377581),
      Complex.zero,
      Complex(0.0035690338330905, -0.571296157558004),
    ),
    (
      8,
      Complex(0.0335179529830357, -5.36734153770268),
      Complex.zero,
      Complex(0.00402402866996429, -0.644155997436322),
    ),
    (
      9,
      Complex(0.0330749844414097, -5.29640750857004),
      Complex.zero,
      Complex(0.00446699721159031, -0.715090026568956),
    ),
    (
      10,
      Complex(0.0326435716008696, -5.22732393237131),
      Complex.zero,
      Complex(0.00489841005213043, -0.784173602767695),
    ),
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
    (10, "1.15"),
  )
}
