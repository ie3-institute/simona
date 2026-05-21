/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.{CableMaterial, CableSetup, Layer}
import edu.ie3.util.scala.quantities.{JoulesPerMeterKelvin, KelvinMetersPerWatt}
import squants.Meters
import squants.electro.{Kilovolts, Nanofarads, Ohms}
import squants.space.{Millimeters, SquareMeters}
import squants.thermal.Celsius
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
    val mat = CableMaterial.fromString("cooperwoventape")
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

  protected val screen: Layer = {
    val mat = CableMaterial.fromString("Copper")
    Layer(
      "screen",
      mat,
      Millimeters(36.8),
      Millimeters(38.6),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None, // FIXME add Degree of Cover (S.202 TB880)
    )
  }

  protected val jackTape: Layer = {
    val mat = CableMaterial.fromString("cooperwoventape")
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
    "CigreT880_33kVLancCable",
    conductor,
    List(conductorScreen, insulation, insulationScreen, screenTape),
    List(screen),
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
    Ohms(0.0754e-3),
    Nanofarads(0.237683304),
    0.004,
    0.0435122656,
    0.0,
  )
}
