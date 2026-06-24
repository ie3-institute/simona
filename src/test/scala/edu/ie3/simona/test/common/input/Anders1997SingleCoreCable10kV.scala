/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.connector.`type`.CableMaterial
import edu.ie3.simona.model.grid.ampacity.{CableSetup, Layer, ScreenLayer}
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  OhmsPerMeter,
}
import squants.Meters
import squants.electro.{Kilovolts, Nanofarads}
import squants.space.{Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.time.Hertz

import java.util.UUID

object Anders1997SingleCoreCable10kV {
  // // Anders Rating of electric power cables: ampacity computations for transmission, distribution, and industrial applications p 364ff

  protected val conductor: Layer = {
    val mat = CableMaterial.fromString("Copper")
    Layer(
      "conductor",
      mat,
      Millimeters(0),
      Millimeters(20.5),
      mat.getThermalProperties.resistivity().toSquants,
      mat.getThermalProperties.capacitance().toSquants,
      Some(SquareMeters(0.00024)),
    )
  }

  protected val conductorScreen: Layer = {
    val mat = CableMaterial.fromString("XLPE")
    Layer(
      "conductorScreen",
      mat,
      Millimeters(20.5),
      Millimeters(21.7),
      mat.getThermalProperties.resistivity().toSquants,
      mat.getThermalProperties.capacitance().toSquants,
      None,
    )
  }

  protected val insulation: Layer = {
    val mat = CableMaterial.fromString("XLPE")
    Layer(
      "insulation",
      mat,
      Millimeters(21.7),
      Millimeters(28.5),
      mat.getThermalProperties.resistivity().toSquants,
      mat.getThermalProperties.capacitance().toSquants,
      None,
    )
  }

  protected val insulationScreen: Layer = {
    val mat = CableMaterial.fromString("XLPE")
    Layer(
      "insulationScreen",
      mat,
      Millimeters(28.5),
      Millimeters(30.1),
      mat.getThermalProperties.resistivity().toSquants,
      mat.getThermalProperties.capacitance().toSquants,
      None,
    )
  }

  protected val screen: ScreenLayer = {
    ScreenLayer(
      CableMaterial.COPPER,
      Millimeters(30.1),
      Millimeters(31.4), // This is 31.4 mm, not 31.2mm (Anders 2005)
      CableMaterial.COPPER.getThermalProperties.resistivity().toSquants,
      CableMaterial.COPPER.getThermalProperties.capacitance().toSquants,
      None,
      76,
      Millimeters(0.65),
      None,
      CableMaterial.COPPER.getElectricalResistivity().toSquants,
    )
  }

  protected val jack: Layer = {
    val mat = CableMaterial.fromString("PVC")
    Layer(
      "jack",
      mat,
      Millimeters(31.4), // This is 31.4 mm, not 31.2mm (Anders 2005)
      Millimeters(35.8),
      mat.getThermalProperties.resistivity().toSquants,
      mat.getThermalProperties.capacitance().toSquants,
      None,
    )
  }

  val cable: CableSetup = new CableSetup(
    UUID.fromString("b8152c3f-d12f-4857-9746-a30aef6aee08"),
    "CigreT880_33kVLancCable",
    Coordinate3D(0.0, 0.0, -1.0),
    Coordinate3D(1.0, 0.0, -1.0),
    conductor,
    List(conductorScreen, insulation, insulationScreen),
    Some(screen),
    List.empty[Layer],
    List.empty[Layer],
    List(jack),
    "flat-distance",
    Meters(1),
    Meters(2 * 0.0358),
    KelvinMetersPerWatt(1.0),
    JoulesPerMeterKelvin(1.0), // FIXME DF check this
    Celsius(90),
    Kilovolts(10),
    Hertz(50),
    OhmsPerMeter(0.0601e-3),
    1.0,
    1.0,
    Nanofarads(0.0), // No capacity provided
    0.004,
    0.09,
    0.0,
  )
}
