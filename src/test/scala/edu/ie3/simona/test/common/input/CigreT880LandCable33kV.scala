/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.{
  CableMaterial,
  CableSetup,
  Layer,
  ScreenLayer,
}
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  OhmsPerMeter,
}
import squants.Meters
import squants.electro.{Kilovolts, Nanofarads, OhmMeters}
import squants.space.{Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.time.Hertz

import java.util.UUID

object CigreT880LandCable33kV {
  // // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 195ff

  protected val conductor: Layer = {
    val mat = CableMaterial.fromString("Copper")
    Layer(
      "conductor",
      mat,
      Millimeters(0),
      Millimeters(18.4),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      Some(SquareMeters(0.00024)),
    )
  }

  protected val conductorScreen: Layer = {
    val mat = CableMaterial.fromString("semicondscreen")
    Layer(
      "conductorScreen",
      mat,
      Millimeters(18.4),
      Millimeters(19.4),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val insulation: Layer = {
    val mat = CableMaterial.fromString("XLPE")
    Layer(
      "insulation",
      mat,
      Millimeters(19.4),
      Millimeters(34.8),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val insulationScreen: Layer = {
    val mat = CableMaterial.fromString("semicondscreen")
    Layer(
      "insulationScreen",
      mat,
      Millimeters(34.8),
      Millimeters(35.8),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val screenTape: Layer = {
    val mat = CableMaterial.fromString("copperwoventape")
    Layer(
      "screenTape",
      mat,
      Millimeters(35.8),
      Millimeters(36.8),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val screen: ScreenLayer = {
    ScreenLayer(
      CableMaterial.Copper,
      Millimeters(36.8),
      Millimeters(38.6),
      CableSetup.materialProps(CableMaterial.Copper)._1,
      CableSetup.materialProps(CableMaterial.Copper)._2,
      Some(SquareMeters(35.62566069e-6)),
      56,
      Millimeters(0.9),
      Some(Millimeters(240)),
      OhmMeters(1.7241e-8),
    )
  }

  protected val jackTape: Layer = {
    val mat = CableMaterial.fromString("copperwoventape")
    Layer(
      "jackTape",
      mat,
      Millimeters(38.6),
      Millimeters(39.2),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val jack: Layer = {
    val mat = CableMaterial.fromString("XLPE")
    Layer(
      "jack",
      mat,
      Millimeters(39.2),
      Millimeters(43.6),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val outerCover: Layer = {
    val mat = CableMaterial.fromString("semicondscreen")
    Layer(
      "outerCover",
      mat,
      Millimeters(43.6),
      Millimeters(44.0),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  val cable: CableSetup = new CableSetup(
    UUID.fromString("b8152c3f-d12f-4857-9746-a30aef6aee08"),
    "CigreT880_33kVLandCable",
    Coordinate3D(0.0, 0.0, -1.0),
    Coordinate3D(1.0, 0.0, -1.0),
    conductor,
    List(conductorScreen, insulation, insulationScreen, screenTape),
    Some(screen),
    List.empty[Layer],
    List.empty[Layer],
    List(jackTape, jack, outerCover),
    "trefoil-touching",
    Meters(1),
    Meters(0.044),
    KelvinMetersPerWatt(1.0),
    JoulesPerMeterKelvin(1.0), // FIXME check this
    Celsius(90),
    Kilovolts(33),
    Hertz(50),
    OhmsPerMeter(0.0754e-3),
    1.0,
    1.0,
    Nanofarads(0.237683304),
    0.004,
    0.0435122656,
    0.0,
  )
}
