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

object CigreT880SubmarineCable400kV {
  // // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 238ff

  protected val conductor: Layer = {
    val mat = CableMaterial.fromString("Copper")
    Layer(
      "conductor",
      mat,
      Millimeters(0),
      Millimeters(34.7),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      Some(SquareMeters(0.000800)),
    )
  }

  protected val conductorTape: Layer = {
    val mat = CableMaterial.fromString("copperwoventape")
    Layer(
      "conductorTape",
      mat,
      Millimeters(34.7),
      Millimeters(35.8),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val conductorScreen: Layer = {
    val mat = CableMaterial.fromString("semicondscreen")
    Layer(
      "conductorScreen",
      mat,
      Millimeters(35.8),
      Millimeters(39.8),
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
      Millimeters(39.8),
      Millimeters(100.6),
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
      Millimeters(100.6),
      Millimeters(103.7),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val tapeUnderScreen: Layer = {
    val mat = CableMaterial.fromString("copperwoventape")
    Layer(
      "tapeUnderScreen",
      mat,
      Millimeters(103.7),
      Millimeters(104.7),
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
      Millimeters(104.7),
      Millimeters(109.7),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None, // FIXME add Degree of Cover (S.202 TB880)
    )
  }

  protected val tapeOverScreen: Layer = {
    val mat = CableMaterial.fromString("copperwoventape")
    Layer(
      "tapeOverScreen",
      mat,
      Millimeters(104.7),
      Millimeters(109.7),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None, // FIXME add Degree of Cover (S.202 TB880)
    )
  }

  protected val leadAllowSheath: Layer = {
    val mat = CableMaterial.fromString("lead")
    Layer(
      "leadAllowSheath",
      mat,
      Millimeters(109.7),
      Millimeters(116.9),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val semiconductivePeSheath: Layer = {
    val mat = CableMaterial.fromString("PE")
    Layer(
      "sheath",
      mat,
      Millimeters(116.9),
      Millimeters(122.9),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val armour: Layer = {
    val mat = CableMaterial.fromString("steel")
    Layer(
      "armour",
      mat,
      Millimeters(122.9),
      Millimeters(128.3),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  protected val outerServing: Layer = {
    val mat = CableMaterial.fromString("PP")
    Layer(
      "outerServing",
      mat,
      Millimeters(128.3),
      Millimeters(134.3),
      CableSetup.materialProps(mat)._1,
      CableSetup.materialProps(mat)._2,
      None,
    )
  }

  val cable: CableSetup = new CableSetup(
    UUID.fromString("b8152c3f-d12f-4857-9746-a30aef6aee08"),
    "CigreT880_33kVLancCable",
    conductor,
    List(
      conductorTape,
      conductorScreen,
      insulation,
      insulationScreen,
      tapeUnderScreen,
    ),
    List(screen),
    List(
      tapeOverScreen,
      semiconductivePeSheath,
    ), // FIXME wo muss leadAllowSheath hin?
    List(armour),
    List(outerServing),
    "flat-distance",
    Meters(1),
    Meters(5),
    KelvinMetersPerWatt(1.0),
    JoulesPerMeterKelvin(1.0), // FIXME check this
    Celsius(90),
    Kilovolts(400),
    Ohms(0.0221e-3),
    Nanofarads(0.237683304), // FIXME
    0.004, // FIXME
    0.0435122656, // FIXME
    0.0, // FIXME
  )
}
