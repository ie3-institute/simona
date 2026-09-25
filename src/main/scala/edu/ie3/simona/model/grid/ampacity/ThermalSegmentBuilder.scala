/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.{
  CableDeploymentInput,
  LineInput,
}
import edu.ie3.datamodel.models.input.connector.`type`.{
  CableMaterial,
  CableTypeInput,
  LineTypeInput,
  ConductorInput as JConductorInput,
  LayerInput as JLayerInput,
  ScreenLayerInput as JScreenLayerInput,
}
import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.ampacity.CableSetup
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel
import edu.ie3.simona.model.grid.ampacity.SoilDataParser
import edu.ie3.simona.model.grid.ampacity.SoilLayer
import edu.ie3.simona.model.grid.ampacity.SoilType
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import play.api.libs.json.*
import squants.Meters
import squants.space.Millimeters

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutableMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Failure, Success, Try}

/** Builder responsible for constructing [[LineSegmentThermalModel]] instances
  * from a [[SubGridContainer]], including validation of soil layers, cable
  * types, and cable deployments required for the ampacity calculation.
  */
object ThermalSegmentBuilder {

  /** Result of building thermal segments.
    *
    * @param soilLayers
    *   The validated soil layers read from configuration.
    * @param thermalLineSegments
    *   The generated thermal line segments.
    * @param segmentCoordinates
    *   A map from segment UUID to the midpoint coordinate (latitude, longitude)
    *   used for weather registration.
    */
  final case class BuildResult(
      soilLayers: Seq[SoilLayer],
      thermalLineSegments: Set[LineSegmentThermalModel],
      segmentCoordinates: Map[UUID, Coordinate] = Map.empty,
  )

  /** Builds thermal line segments for the given subgrid.
    *
    * Validates soil data, cable types, and cable deployments when ampacity
    * calculation is activated. Generates thermal segments for all lines that
    * have both a cable type and a cable deployment. Writes the generated
    * segments to the simulation output folder.
    *
    * @param subGridContainer
    *   The subgrid container with all input models.
    * @param simonaConfig
    *   The SIMONA configuration.
    * @return
    *   A [[BuildResult]] containing the soil layers and thermal line segments.
    */
  def build(
      subGridContainer: SubGridContainer,
      simonaConfig: SimonaConfig,
  ): BuildResult = {
    // 1. Read and validate soil data
    val soilLayers = readAndValidateSoilLayers(simonaConfig)
    val soilTypes = readAndValidateSoilTypes(simonaConfig)
    validateSoilReferences(soilLayers, soilTypes)

    // 2. Validate cable deployments
    // NOTE: Validation of cable types and cable deployments on subgrid level
    // is currently skipped, since not every subgrid is required to have cable
    // types or cable deployments. This should be validated on a higher level
    // (e.g., simulation level) in the future.
    // validateCableTypesPresent(subGridContainer)
    // validateCableDeployments(subGridContainer, deploymentsByLine)

    // 3. Generate thermal segments
    val (thermalLineSegments, generatedSegments, segmentCoordinates) =
      generateThermalSegments(subGridContainer)

    // 4. Write generated segments to output
    writeThermalSegmentsToOutput(generatedSegments, simonaConfig)

    BuildResult(soilLayers, thermalLineSegments, segmentCoordinates)
  }

  /** Reads soil layers from the configured directory and validates them.
    */
  private def readAndValidateSoilLayers(
      simonaConfig: SimonaConfig
  ): Seq[SoilLayer] = {
    val soilLayersTry: Try[Seq[SoilLayer]] = readFromConfiguredDir[SoilLayer](
      "soilLayers.csv",
      SoilDataParser.readSoilLayers,
      simonaConfig,
    )
    val soilLayers: Seq[SoilLayer] =
      soilLayersTry.getOrElse(Seq.empty[SoilLayer])

    soilLayersTry match
      case Failure(cause) =>
        throw new GridAgentInitializationException(
          "Ampacity calculation is activated, but reading soil layers failed. Please ensure 'soilLayers.csv' is present and valid.",
          cause,
        )
      case Success(layers) if layers.isEmpty =>
        throw new GridAgentInitializationException(
          "Ampacity calculation is activated, but no soil layers were found. Please provide 'soilLayers.csv' with valid entries."
        )
      case _ => // ok
    soilLayers
  }

  /** Reads soil types from the configured directory and validates them.
    */
  private def readAndValidateSoilTypes(
      simonaConfig: SimonaConfig
  ): Seq[SoilType] = {
    val soilTypesTry: Try[Seq[SoilType]] = readFromConfiguredDir[SoilType](
      "soilTypes.csv",
      SoilDataParser.readSoilTypes,
      simonaConfig,
    )
    val soilTypes: Seq[SoilType] = soilTypesTry.getOrElse(Seq.empty[SoilType])

    soilTypesTry match
      case Failure(cause) =>
        throw new GridAgentInitializationException(
          "Ampacity calculation is activated, but reading soil types failed. Please ensure 'soilTypes.csv' is present and valid.",
          cause,
        )
      case Success(types) if types.isEmpty =>
        throw new GridAgentInitializationException(
          "Ampacity calculation is activated, but no soil types were found. Please provide 'soilTypes.csv' with valid entries."
        )
      case _ => // ok
    soilTypes
  }

  /** Validates that all soil layers reference existing soil types.
    */
  private def validateSoilReferences(
      soilLayers: Seq[SoilLayer],
      soilTypes: Seq[SoilType],
  ): Unit = {
    if soilLayers.nonEmpty && soilTypes.nonEmpty then
      val missingRefs =
        SoilDataParser
          .associateLayersWithTypes(soilLayers, soilTypes)
          .collect { case (l, None) => l }
      if missingRefs.nonEmpty then
        throw new GridAgentInitializationException(
          s"Ampacity calculation is activated, but ${missingRefs.size} soil layers reference missing soil types. Please ensure soilTypes.csv contains the referenced UUIDs."
        )
  }

  /** Validates that at least one line has a cable type attached to its line
    * type.
    */
  private def validateCableTypesPresent(
      subGridContainer: SubGridContainer
  ): Unit = {
    val hasCableType =
      subGridContainer.getRawGrid.getLines.asScala.toSeq
        .exists(line =>
          Option(line.getType).exists(_.getCableType.toScala.isDefined)
        )
    if !hasCableType then
      throw new GridAgentInitializationException(
        "Ampacity calculation is activated, but no cable types are available. Please provide at least one cable type."
      )
  }

  /** Validates that cable deployment entries exist and that every line with a
    * cable type also has a cable deployment.
    */
  private def validateCableDeployments(
      subGridContainer: SubGridContainer,
      deploymentsByLine: scala.collection.Map[UUID, java.util.List[
        CableDeploymentInput
      ]],
  ): Unit = {
    if deploymentsByLine.isEmpty then
      throw new GridAgentInitializationException(
        "Ampacity calculation is activated, but no cable deployment entries were provided. Please provide at least one cable deployment."
      )

    val missingDeployments =
      subGridContainer.getRawGrid.getLines.asScala.toSeq.flatMap { lineInput =>
        Option(lineInput.getType)
          .flatMap(resolveCableType)
          .toSeq
          .flatMap { cableType =>
            val hasDeployment =
              deploymentsByLine
                .get(lineInput.getUuid)
                .exists(_.asScala.nonEmpty)
            if hasDeployment then None
            else
              Some(
                s"line ${lineInput.getUuid} (id: ${lineInput.getId}, lineType=${lineInput.getType.getUuid}, cableType=${cableType.getUuid})"
              )
          }
      }

    if missingDeployments.nonEmpty then
      throw new GridAgentInitializationException(
        s"Ampacity calculation is activated, but ${missingDeployments.size} line(s) reference a cable_type and do not have a cable deployment in cable_deployment_input.csv:\n" +
          missingDeployments.mkString("\n") +
          "\nPlease add a cable deployment entry for each of these lines."
      )
  }

  /** Resolves the cable type directly attached to a line type.
    *
    * @param lineType
    *   The line type input.
    * @return
    *   An optional [[CableTypeInput]].
    */
  private def resolveCableType(
      lineType: LineTypeInput
  ): Option[CableTypeInput] =
    Option(lineType).flatMap(_.getCableType.toScala)

  /** Generates thermal line segments for all lines that have both a cable type
    * and a cable deployment.
    *
    * @param subGridContainer
    *   The subgrid container.
    * @return
    *   A tuple of the generated segments, their coordinates for output, and the
    *   midpoint coordinates for weather registration. //FIXME DF not always
    *   midpoint
    */
  private def generateThermalSegments(
      subGridContainer: SubGridContainer
  ): (
      Set[LineSegmentThermalModel],
      Seq[(LineSegmentThermalModel, (Double, Double), (Double, Double))],
      Map[UUID, Coordinate],
  ) = {
    val deploymentsByLine =
      subGridContainer.getRawGrid.getCableDeploymentsByLine.asScala

    val generatedSegments: ListBuffer[
      (LineSegmentThermalModel, (Double, Double), (Double, Double))
    ] = ListBuffer.empty

    val segmentCoordinatesBuffer: MutableMap[UUID, Coordinate] =
      MutableMap.empty

    val thermalLineSegments: Set[LineSegmentThermalModel] =
      subGridContainer.getRawGrid.getLines.asScala.flatMap { lineInput =>
        // Only build thermal segments if both a cable type and a cable deployment exist
        Option(lineInput.getType).toSeq.flatMap { lineType =>
          resolveCableType(lineType).toSeq.flatMap { cableTypeInput =>
            val deploymentListOpt =
              deploymentsByLine.get(lineInput.getUuid)
            val firstDeploymentOpt =
              deploymentListOpt.map(_.asScala).flatMap(_.headOption)

            // If no deployment, skip this line
            firstDeploymentOpt.toSeq.flatMap { firstDeployment =>
              // Geometrische Stützpunkte aus dem GeoJSON extrahieren
              val jsonStringLineInput = lineInputToJson(lineInput)
              val json = Json.parse(jsonStringLineInput)
              val coordinates: Seq[(Double, Double)] =
                (json \ "coordinates")
                  .asOpt[JsArray]
                  .map(_.value.toSeq.collect {
                    case pair: JsArray if pair.value.size >= 2 =>
                      (pair.value(0).as[Double], pair.value(1).as[Double])
                  })
                  .getOrElse(Seq.empty)

              val conductor: Layer =
                mapConductor(cableTypeInput.getConductor)
              val isolation: List[Layer] =
                cableTypeInput.getIsolation.asScala
                  .map(mapLayer)
                  .toList
              val screen: Option[ScreenLayer] =
                cableTypeInput.getScreen.toScala.map(mapScreen)
              val filler: List[Layer] = Option(cableTypeInput.getFiller)
                .map(_.asScala.map(mapLayer).toList)
                .getOrElse(List.empty)
              val armor: List[Layer] = Option(cableTypeInput.getArmor)
                .map(_.asScala.map(mapLayer).toList)
                .getOrElse(List.empty)
              val jack: List[Layer] =
                cableTypeInput.getJack.asScala.map(mapLayer).toList

              val deploymentPattern: String =
                Option(firstDeployment.getLayoutFormation).getOrElse(
                  throw new NoSuchElementException(
                    "No deployment pattern available"
                  )
                )

              val conductorDistance =
                Option(firstDeployment.getDistanceCables)
                  .map(_.toSquants)
                  .getOrElse(Meters(1))

              val cable: CableSetup = CableSetup(
                cableTypeInput.getUuid,
                cableTypeInput.getId,
                Coordinate3D(0.0, 0.0, -1.0),
                Coordinate3D(1.0, 0.0, -1.0),
                conductor,
                isolation,
                screen,
                filler,
                armor,
                jack,
                deploymentPattern,
                conductorDistance,
                cableTypeInput.getJack.asScala.lastOption
                  .map(_.outerDiameter().toSquants)
                  .getOrElse(
                    throw new NoSuchElementException("No jack available")
                  ),
                KelvinMetersPerWatt(1),
                JoulesPerCubicMeterKelvin(1),
                cableTypeInput.getLimitTemperature.toSquants,
                lineType.getvRated().toSquants,
                cableTypeInput.getFrequency.toSquants,
                lineType.getR.toResistancePerLength,
                cableTypeInput.getSkinEffectCoefficient,
                cableTypeInput.getProximityEffectCoefficient,
                cableTypeInput.getElectricalCapacitance.toSquants,
                cableTypeInput.getTanDelta,
                cableTypeInput.getCirculatingLossFactor,
                cableTypeInput.getEddyCurrentLossFactor,
              )

              val segments =
                if coordinates.size >= 2 then
                  coordinates
                    .sliding(2)
                    .collect { case Seq(start, end) =>
                      val segment = LineSegmentThermalModel(
                        UUID.randomUUID(),
                        s"LineTher_${lineInput.getId}_${start}_${end}",
                        lineInput.getUuid,
                        cable,
                        KelvinMetersPerWatt(1),
                        KelvinMetersPerWatt(1),
                        KelvinMetersPerWatt(1),
                        KelvinMetersPerWatt(1),
                        JoulesPerCubicMeterKelvin(1),
                        JoulesPerCubicMeterKelvin(1),
                        JoulesPerCubicMeterKelvin(1),
                        JoulesPerCubicMeterKelvin(1),
                        JoulesPerCubicMeterKelvin(1),
                        cableTypeInput.getLimitTemperature.toSquants,
                      )
                      val entry: (
                          LineSegmentThermalModel,
                          (Double, Double),
                          (Double, Double),
                      ) = (segment, start, end)
                      generatedSegments += entry
                      // Store midpoint coordinate for weather registration.
                      // GeoJSON coordinates are (longitude, latitude),
                      // Coordinate expects (latitude, longitude).
                      segmentCoordinatesBuffer(segment.uuid) = Coordinate(
                        (start._2 + end._2) / 2.0, // latitude
                        (start._1 + end._1) / 2.0, // longitude
                      )
                      segment
                    }
                    .toSet
                else Set.empty
              segments
            }
          }
        }
      }.toSet

    (
      thermalLineSegments,
      generatedSegments.toSeq,
      segmentCoordinatesBuffer.toMap,
    )
  }

  /** Converts a [[LineInput]] to a GeoJSON string representation.
    */
  private def lineInputToJson(lineInput: LineInput): String = {
    val lineString = lineInput.getGeoPosition

    val coordinatesJson = lineString.getCoordinates
      .map { coord =>
        s"[${coord.x}, ${coord.y}]"
      }
      .mkString(",")

    s"""{"type": "LineString", "coordinates": [$coordinatesJson]}"""
  }

  /** Reads a CSV file from the configured grid CSV directory.
    */
  private def readFromConfiguredDir[T](
      fileName: String,
      parser: Path => Try[Seq[T]],
      simonaConfig: SimonaConfig,
  ): Try[Seq[T]] = {
    simonaConfig.input.grid.datasource.csvParams.map(_.directoryPath) match {
      case Some(dir) =>
        val p = Paths.get(dir).resolve(fileName)
        if Files.exists(p) then parser(p)
        else Success(Seq.empty[T])
      case None => Success(Seq.empty[T])
    }
  }

  /** Writes the generated thermal line segments to a CSV file in the simulation
    * output folder (configured via `simona.output.base.dir`).
    *
    * @param segments
    *   The generated thermal segments, each with the start/end coordinates it
    *   was built from.
    * @param simonaConfig
    *   The SIMONA configuration (used to resolve the output base dir).
    */
  private def writeThermalSegmentsToOutput(
      segments: Iterable[
        (LineSegmentThermalModel, (Double, Double), (Double, Double))
      ],
      simonaConfig: SimonaConfig,
  ): Unit = {
    // If nothing to write, nothing to do
    if segments.isEmpty then return

    val baseOutputDir = Paths.get(simonaConfig.output.base.dir)
    val simulationName = simonaConfig.simulationName

    val runDirOpt: Option[Path] =
      try
        val stream = Files.list(baseOutputDir)
        try
          val dirs = stream
            .filter(p =>
              Files.isDirectory(p) && p.getFileName.toString.startsWith(
                simulationName
              )
            )
            .iterator()
            .asScala
            .toSeq
          if dirs.nonEmpty then
            Some(dirs.maxBy(p => Files.getLastModifiedTime(p).toMillis))
          else None
        finally stream.close()
      catch case _: Exception => None

    val runDir = runDirOpt.getOrElse(baseOutputDir.resolve(simulationName))
    val rawOutputDir = runDir.resolve("rawOutputData")
    Files.createDirectories(rawOutputDir)

    val outPath = rawOutputDir.resolve("thermal_line_segments.csv")

    val header =
      "segmentUuid,lineUuid,startX,startY,endX,endY,limitTemperature"
    val rows = segments.map { case (segment, (sx, sy), (ex, ey)) =>
      Seq(
        segment.uuid.toString,
        segment.lineUuid.toString,
        sx.toString,
        sy.toString,
        ex.toString,
        ey.toString,
        segment.upperBoundaryTemperature.value.toString,
      ).mkString(",")
    }.toSeq

    val content = rows.mkString("\n") + "\n"

    // Append if the file already exists, otherwise create it with the header.
    if Files.exists(outPath) then
      Files.write(
        outPath,
        content.getBytes("UTF-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND,
      )
    else
      Files.write(
        outPath,
        (header +: rows).mkString("\n").getBytes("UTF-8"),
      )
  }

  private def mapConductor(jc: JConductorInput): Layer = {
    val mat = CableMaterial.fromString(jc.material().toString)
    Layer(
      jc.name(),
      mat,
      Millimeters(0.0),
      jc.diameter().toSquants,
      jc.thermalResistivity().toSquants,
      jc.thermalCapacitance().toSquantsJoulePerCubicMeterKelvin,
      jc.area().toScala.map(_.toSquants),
    )
  }

  private def mapLayer(jl: JLayerInput): Layer = {
    val mat = CableMaterial.fromString(jl.material().toString)
    Layer(
      jl.name(),
      mat,
      jl.innerDiameter().toSquants,
      jl.outerDiameter().toSquants,
      jl.thermalResistivity().toSquants,
      jl.thermalCapacitance().toSquantsJoulePerCubicMeterKelvin,
      jl.area().toScala.map(_.toSquants),
    )
  }

  private def mapScreen(js: JScreenLayerInput): ScreenLayer = {
    val mat = CableMaterial.fromString(js.material().toString)
    ScreenLayer(
      mat,
      js.innerDiameter().toSquants,
      js.outerDiameter().toSquants,
      js.thermalResistivity().toSquants,
      js.thermalCapacitance().toSquantsJoulePerCubicMeterKelvin,
      js.area().toScala.map(_.toSquants),
      js.wiresNumber,
      js.wireDiameter.toSquants,
      js.lengthOfLay().toScala.map(_.toSquants),
      js.electricalResistivity.toSquants,
    )
  }

}
