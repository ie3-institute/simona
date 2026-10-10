/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.`type`.{
  CableTypeInput,
  LineTypeInput,
  ConductorInput as JConductorInput,
  LayerInput as JLayerInput,
  ScreenLayerInput as JScreenLayerInput,
}
import edu.ie3.datamodel.models.input.connector.{
  CableDeploymentInput,
  LineInput,
}
import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.ampacity.{
  CableSetup,
  LineSegmentThermalModel,
  SoilDataParser,
  SoilLayer,
  SoilType,
}
import edu.ie3.simona.util.{Coordinate, Coordinate3D}
import edu.ie3.util.geo.RichGeometries.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import com.typesafe.scalalogging.LazyLogging
import org.locationtech.jts.operation.distance.DistanceOp
import org.locationtech.jts.geom.{
  Geometry,
  GeometryFactory,
  Coordinate as JtsCoordinate,
}
import org.locationtech.jts.linearref.LengthIndexedLine
import squants.space.{Length, Millimeters}
import squants.{Meters, Temperature}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.UUID
import scala.collection.mutable.{
  ListBuffer,
  Map as MutableMap,
  Set as MutableSet,
}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.{Failure, Success, Try}

/** Builder responsible for constructing [[LineSegmentThermalModel]] instances
  * from a [[SubGridContainer]], including validation of soil layers, cable
  * types, and cable deployments required for the ampacity calculation.
  */
object ThermalSegmentBuilder extends LazyLogging {

  /** Information about a neighbour line used for parallel-proximity detection.
    *
    * @param lineUuid
    *   The UUID of the neighbour line.
    * @param geoPosition
    *   The centreline coordinates as (longitude, latitude) pairs.
    * @param hasDeployment
    *   Whether the line has a [[CableDeploymentInput]] (buried line).
    */
  final case class NeighbourLineInfo(
      lineUuid: UUID,
      geoPosition: Seq[(Double, Double)],
      hasDeployment: Boolean,
  )

  /** Minimum parallel-stretch length in meters below which a split is skipped.
    */
  private val minOverlapLengthMeters: Double = 5.0

  /** Maximum sampling step in degrees for parallel-proximity detection along a
    * straight segment (about one meter).
    */
  private val minSampleStepDegrees: Double = 1e-5

  /** Upper bound on the number of samples used for parallel-proximity detection
    * along a single straight segment.
    */
  private val maxSamples: Int = 200000 // 200km at 1m, unreasonable long
  /** Result of building thermal segments.
    *
    * @param soilLayers
    *   The validated soil layers read from configuration.
    * @param thermalLineSegments
    *   The generated thermal line segments.
    * @param segmentCoordinates
    *   A map from segment UUID to the midpoint coordinate (latitude, longitude)
    *   used for weather registration.
    * @param parallelInfluence
    *   A map from segment UUID to the list of (neighbour line UUID, minimum
    *   lateral distance in meters) for each proximate neighbour.
    */
  final case class BuildResult(
      soilLayers: Seq[SoilLayer],
      thermalLineSegments: Set[LineSegmentThermalModel],
      segmentCoordinates: Map[UUID, Coordinate] = Map.empty,
      parallelInfluence: Map[UUID, Seq[(UUID, Double)]] = Map.empty,
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
    * @param neighbourLines
    *   Neighbour line geometries from all subgrids, used for parallel-proximity
    *   detection. Empty when the feature is not activated.
    * @return
    *   A [[BuildResult]] containing the soil layers and thermal line segments.
    */
  def build(
      subGridContainer: SubGridContainer,
      simonaConfig: SimonaConfig,
      neighbourLines: Seq[NeighbourLineInfo] = Seq.empty,
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

    // 3. Generate line segments thermal model
    val ppThreshold =
      if simonaConfig.ampacityCalculation.activateParallelProximitySplitting
      then simonaConfig.ampacityCalculation.parallelProximityThreshold
      else None
    val (
      lineSegmentThermalModels,
      generatedSegments,
      segmentCoordinates,
      parallelInfluence,
    ) = generateThermalSegments(
      subGridContainer,
      soilLayers,
      soilTypes,
      neighbourLines,
      ppThreshold,
    )
    // 4. Write generated segments to output
    writeThermalSegmentsToOutput(generatedSegments, simonaConfig)
    BuildResult(
      soilLayers,
      lineSegmentThermalModels,
      segmentCoordinates,
      parallelInfluence,
    )
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
  private[ampacity] def validateCableTypesPresent(
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
  private[ampacity] def validateCableDeployments(
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
    * @param soilLayers
    *   The validated soil layers.
    * @param soilTypes
    *   The validated soil types.
    * @param neighbourLines
    *   Neighbour line geometries from all subgrids, used for parallel-proximity
    *   detection. Empty when the feature is not activated.
    * @param ppThreshold
    *   The parallel-proximity threshold in meters, if activated.
    * @return
    *   A tuple of the generated segments, their coordinates for output, the
    *   coordinates for weather registration, and the parallel-influence map.
    */
  private def generateThermalSegments(
      subGridContainer: SubGridContainer,
      soilLayers: Seq[SoilLayer],
      soilTypes: Seq[SoilType],
      neighbourLines: Seq[NeighbourLineInfo],
      ppThreshold: Option[Double],
  ): (
      Set[LineSegmentThermalModel],
      Seq[(LineSegmentThermalModel, (Double, Double), (Double, Double))],
      Map[UUID, Coordinate],
      Map[UUID, Seq[(UUID, Double)]],
  ) = {
    val deploymentsByLine =
      subGridContainer.getRawGrid.getCableDeploymentsByLine.asScala
    val generatedSegments: ListBuffer[
      (LineSegmentThermalModel, (Double, Double), (Double, Double))
    ] = ListBuffer.empty
    val segmentCoordinatesBuffer: MutableMap[UUID, Coordinate] =
      MutableMap.empty
    val parallelInfluenceBuffer: MutableMap[UUID, Seq[(UUID, Double)]] =
      MutableMap.empty
    val warnedPairs: MutableSet[(UUID, UUID)] = MutableSet.empty
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
              // Extract line points from geoposition
              val coordinates: Seq[(Double, Double)] =
                Option(lineInput.getGeoPosition)
                  .map(_.getCoordinates.toSeq.map(c => (c.x, c.y)))
                  .getOrElse(Seq.empty)
              // TODO DF: In case some information are missing in the provided input data, these might be filled by data in PSDM (Cable Material Data). Might worth an improvement in PSDM, noted here to keep track.
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
                Option(firstDeployment.getLayoutFormation)
                  .getOrElse(
                    throw new NoSuchElementException(
                      s"No deployment pattern available for line ${lineInput.getUuid}"
                    )
                  )
              val cableDepth = Option(firstDeployment.getDepthCables)
                .map(_.toSquants)
                .getOrElse(Meters(1))
              val cableDistance =
                Option(firstDeployment.getDistanceCables)
                  .map(_.toSquants)
                  .getOrElse(Meters(1))
              val cable: CableSetup = CableSetup(
                cableTypeInput.getUuid,
                cableTypeInput.getId,
                conductor,
                isolation,
                screen,
                filler,
                armor,
                jack,
                deploymentPattern,
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
              val segments: Set[LineSegmentThermalModel] =
                splitAndAssignSegments(
                  lineInput,
                  cable,
                  coordinates,
                  cableDepth,
                  cableDistance,
                  soilLayers,
                  soilTypes,
                  cableTypeInput.getLimitTemperature.toSquants,
                  generatedSegments,
                  segmentCoordinatesBuffer,
                  neighbourLines,
                  ppThreshold,
                  parallelInfluenceBuffer,
                  warnedPairs,
                )
              segments
            }
          }
        }
      }.toSet
    (
      thermalLineSegments,
      generatedSegments.toSeq,
      segmentCoordinatesBuffer.toMap,
      parallelInfluenceBuffer.toMap,
    )
  }

  /** Splits a line into thermal segments at soil boundaries and, optionally, at
    * parallel-proximity stretch boundaries.
    *
    * @param lineInput
    *   The candidate line.
    * @param cable
    *   The cable setup.
    * @param coordinates
    *   The line's support-point coordinates as (lon, lat) pairs.
    * @param cableDepth
    *   The cable burial depth.
    * @param cableDistance
    *   The distance between cables in a multi-cable deployment.
    * @param soilLayers
    *   The validated soil layers.
    * @param soilTypes
    *   The validated soil types.
    * @param limitTemperature
    *   The cable's limit temperature.
    * @param generatedSegments
    *   A mutable buffer to collect generated segments.
    * @param segmentCoordinatesBuffer
    *   A mutable map to collect segment coordinates.
    * @param neighbourLines
    *   Neighbour lines for parallel-proximity detection.
    * @param ppThreshold
    *   The parallel-proximity threshold in meters, if activated.
    * @param parallelInfluenceBuffer
    *   A mutable map to collect parallel-influence marks per segment.
    * @param warnedPairs
    *   A set of already-warned (candidate, neighbour) UUID pairs.
    * @return
    *   The set of generated [[LineSegmentThermalModel]] instances.
    */
  private[ampacity] def splitAndAssignSegments(
      lineInput: LineInput,
      cable: CableSetup,
      coordinates: Seq[(Double, Double)],
      cableDepth: Length,
      cableDistance: Length,
      soilLayers: Seq[SoilLayer],
      soilTypes: Seq[SoilType],
      limitTemperature: Temperature,
      generatedSegments: ListBuffer[
        (LineSegmentThermalModel, (Double, Double), (Double, Double))
      ],
      segmentCoordinatesBuffer: MutableMap[UUID, Coordinate],
      neighbourLines: Seq[NeighbourLineInfo],
      ppThreshold: Option[Double],
      parallelInfluenceBuffer: MutableMap[UUID, Seq[(UUID, Double)]],
      warnedPairs: MutableSet[(UUID, UUID)],
  ): Set[LineSegmentThermalModel] = {
    val geometryFactory = new GeometryFactory()
    val depthMeters = cableDepth.toMeters
    val soilTypesById: Map[UUID, SoilType] =
      soilTypes.map(t => t.uuid -> t).toMap
    // Pre-compute neighbour centerlines for parallel-proximity detection.
    // NOTE: The haversine-on-sampled-points approach is geodesically consistent
    // but still relies on planar (lon, lat) sampling and a coarse per-sample
    // resolution. For full geometric coverage (exact metric proximity and
    // boundary placement at any latitude and segment heading), the geometries
    // should be projected into a metric coordinate reference system (e.g., UTM
    // or Mercator) before proximity evaluation. This is not yet implemented.
    val neighbourLineStrings: Seq[(NeighbourLineInfo, Geometry)] =
      ppThreshold match {
        case Some(_) =>
          neighbourLines
            .filter(_.lineUuid != lineInput.getUuid)
            .filter(_.geoPosition.size >= 2)
            .map { neighbour =>
              val neighbourLine = geometryFactory.createLineString(
                neighbour.geoPosition
                  .map(c => new JtsCoordinate(c._1, c._2))
                  .toArray
              )
              neighbour -> neighbourLine
            }
        case None => Seq.empty
      }
    coordinates
      .sliding(2)
      .collect { case Seq(start, end) =>
        val jtsLine = geometryFactory.createLineString(
          Array(
            new JtsCoordinate(start._1, start._2),
            new JtsCoordinate(end._1, end._2),
          )
        )
        val indexedLine = new LengthIndexedLine(jtsLine)
        val segLength = jtsLine.getLength
        val rejectedNeighbours: MutableSet[UUID] = MutableSet.empty
        // Soil boundary intersection indices
        val soilIndices: Seq[Double] =
          soilLayers.flatMap { layer =>
            val boundary = layer.geometry.getBoundary
            val intersection = boundary.intersection(jtsLine)
            if intersection.isEmpty then Seq.empty
            else
              intersection.getCoordinates.toSeq
                .map(c => indexedLine.indexOf(c))
                .filterNot(_.isNaN)
          }
        // Parallel-proximity split indices for this straight segment
        val ppIndices: Seq[Double] =
          ppThreshold match {
            case Some(thresholdMeters) =>
              neighbourLineStrings.flatMap { case (neighbour, neighbourLine) =>
                if !neighbour.hasDeployment then
                  // Undeployed neighbour: warn and skip
                  val minDist =
                    neighborLineDistanceMeters(
                      neighbour.geoPosition,
                      jtsLine,
                      geometryFactory,
                    )
                  if minDist < thresholdMeters then
                    val pairKey =
                      (lineInput.getUuid, neighbour.lineUuid)
                    if warnedPairs.add(pairKey) then
                      logger.warn(
                        s"Undeployed line ${neighbour.lineUuid} is within ${minDist}m of line ${lineInput.getUuid} (id: ${lineInput.getId}). No parallel-proximity split performed."
                      )
                  Seq.empty
                else
                  val idxs = sampledProximityIndices(
                    indexedLine,
                    segLength,
                    neighbourLine,
                    thresholdMeters,
                    geometryFactory,
                  )
                  if idxs.size >= 2 then
                    val overlapStart = indexedLine.extractPoint(idxs.min)
                    val overlapEnd = indexedLine.extractPoint(idxs.max)
                    val overlapMeters =
                      overlapStart
                        .haversineDistance(overlapEnd)
                        .getValue
                        .doubleValue()
                    if overlapMeters < minOverlapLengthMeters then
                      logger.warn(
                        s"Parallel stretch between line ${lineInput.getUuid} (id: ${lineInput.getId}) " +
                          s"and neighbour ${neighbour.lineUuid} is only ${overlapMeters}m " +
                          s"(< ${minOverlapLengthMeters}m minimum). No split performed for this stretch."
                      )
                      rejectedNeighbours += neighbour.lineUuid
                      Seq.empty
                    else idxs
                  else idxs
              }
            case None => Seq.empty
          }
        val allIndices: Seq[Double] =
          (Seq(0.0, segLength) ++ soilIndices ++ ppIndices).distinct.sorted
        allIndices
          .sliding(2)
          .collect {
            case Seq(idxStart, idxEnd) if idxEnd - idxStart > 1e-9 =>
              val p1 = indexedLine.extractPoint(idxStart)
              val p2 = indexedLine.extractPoint(idxEnd)
              val midIdx = (idxStart + idxEnd) / 2.0
              val midCoord = indexedLine.extractPoint(midIdx)
              val midPoint = geometryFactory.createPoint(midCoord)
              val coveringLayers =
                soilLayers.filter(layer => layer.geometry.contains(midPoint))
              if coveringLayers.isEmpty then
                throw new GridAgentInitializationException(
                  s"Ampacity build failed: No soil layer covers the subsegment midpoint " +
                    s"(${midCoord.x}, ${midCoord.y}) of line ${lineInput.getUuid} " +
                    s"between ${p1} and ${p2}."
                )
              val assignedLayer =
                coveringLayers
                  .find { layer =>
                    val minZ =
                      math.min(layer.zFrom.toMeters, layer.zTo.toMeters)
                    val maxZ =
                      math.max(layer.zFrom.toMeters, layer.zTo.toMeters)
                    depthMeters >= minZ && depthMeters <= maxZ
                  }
                  .getOrElse(
                    throw new GridAgentInitializationException(
                      s"Ampacity build failed: The cable depth of ${depthMeters} m is " +
                        s"not covered by any soil layer spanning line ${lineInput.getUuid} " +
                        s"at midpoint (${midCoord.x}, ${midCoord.y})."
                    )
                  )
              val assignedSoilType = soilTypesById.getOrElse(
                assignedLayer.soilType,
                throw new GridAgentInitializationException(
                  s"Ampacity build failed: Soil layer ${assignedLayer.uuid} references " +
                    s"unknown soil type ${assignedLayer.soilType} for line ${lineInput.getUuid}."
                ),
              )
              val s = (p1.x, p1.y)
              val e = (p2.x, p2.y)
              val segment = LineSegmentThermalModel(
                UUID.randomUUID(),
                s"LineTher_${lineInput.getId}_${s}_${e}",
                lineInput.getUuid,
                cable,
                Coordinate3D(p1.y, p1.x, depthMeters),
                Coordinate3D(p2.y, p2.x, depthMeters),
                cableDepth,
                cableDistance,
                KelvinMetersPerWatt(1),
                KelvinMetersPerWatt(1),
                KelvinMetersPerWatt(1),
                KelvinMetersPerWatt(1),
                JoulesPerCubicMeterKelvin(1),
                JoulesPerCubicMeterKelvin(1),
                JoulesPerCubicMeterKelvin(1),
                JoulesPerCubicMeterKelvin(1),
                JoulesPerCubicMeterKelvin(1),
                limitTemperature,
                assignedSoilType,
              )
              generatedSegments += ((segment, s, e))
              segmentCoordinatesBuffer(segment.uuid) =
                SegmentCalculationPoints.coordinateFor(
                  segment.uuid,
                  s,
                  e,
                  soilLayers.map(_.geometry.getBoundary),
                )
              // Record parallel-proximity influence
              ppThreshold.foreach { thresholdMeters =>
                val influence: Seq[(UUID, Double)] = neighbourLineStrings
                  .filter(_._1.hasDeployment)
                  .filterNot(n => rejectedNeighbours.contains(n._1.lineUuid))
                  .flatMap { case (neighbour, neighbourLine) =>
                    val distMeters =
                      neighborLineDistanceMeters(
                        neighbour.geoPosition,
                        midPoint,
                        geometryFactory,
                      )
                    if distMeters < thresholdMeters
                    then Some((neighbour.lineUuid, distMeters))
                    else None
                  }
                if influence.nonEmpty then
                  parallelInfluenceBuffer(segment.uuid) = influence
              }
              segment
          }
          .toSet
      }
      .flatten
      .toSet
  }

  /** Computes the minimum geodesic distance in meters between a neighbour
    * line's geoPosition and a JTS geometry (line segment or point). The closest
    * pair of points is located with JTS [[DistanceOp]] in the planar (lon, lat)
    * space, and their true distance is then measured with the haversine
    * formula.
    *
    * @param geoPosition
    *   The neighbour line's centreline coordinates as (lon, lat) pairs.
    * @param geom
    *   The candidate geometry (line segment or point).
    * @param geometryFactory
    *   The JTS geometry factory.
    * @return
    *   The minimum distance in meters, or [[Double.MaxValue]] if the neighbour
    *   line has fewer than two support points.
    */
  private def neighborLineDistanceMeters(
      geoPosition: Seq[(Double, Double)],
      geom: Geometry,
      geometryFactory: GeometryFactory,
  ): Double = {
    if geoPosition.size < 2 then Double.MaxValue
    else
      val line = geometryFactory.createLineString(
        geoPosition.map(c => new JtsCoordinate(c._1, c._2)).toArray
      )
      val nearest = DistanceOp.nearestPoints(line, geom)
      nearest(0).haversineDistance(nearest(1)).getValue.doubleValue()
  }

  /** Locates the parallel-proximity stretch boundaries of a straight segment
    * relative to a neighbour line by sampling the segment and measuring the
    * haversine distance of each sample to the neighbour centerline.
    *
    * The neighbour line is a plain centerline, so proximity is measured
    * geodesically via the haversine formula.
    *
    * @param indexedLine
    *   The length-indexed line of the candidate straight segment.
    * @param segLength
    *   The planar length of the candidate straight segment.
    * @param neighbourLine
    *   The neighbour centerline.
    * @param thresholdMeters
    *   The parallel-proximity threshold in meters.
    * @param geometryFactory
    *   The JTS geometry factory.
    * @return
    *   The in/out transition indices within the open segment, sorted. An empty
    *   sequence when no sample lies within the threshold.
    */
  private[ampacity] def sampledProximityIndices(
      indexedLine: LengthIndexedLine,
      segLength: Double,
      neighbourLine: Geometry,
      thresholdMeters: Double,
      geometryFactory: GeometryFactory,
  ): Seq[Double] = {
    val n = math.max(
      2,
      math.min(maxSamples, (segLength / minSampleStepDegrees).toInt + 1),
    )
    val sampleIdxs: Seq[Double] =
      (0 to n).map(i => segLength * i / n)
    val flags: Seq[Boolean] =
      sampleIdxs.map { idx =>
        val samplePoint = geometryFactory.createPoint(
          indexedLine.extractPoint(idx)
        )
        neighborLineDistanceMeters(
          neighbourLine.getCoordinates.toSeq.map(c => (c.x, c.y)),
          samplePoint,
          geometryFactory,
        ) < thresholdMeters
      }
    sampleIdxs.zipWithIndex.collect {
      case (idx, i)
          if i > 0 && i < sampleIdxs.size && flags(i) != flags(i - 1) =>
        idx
    }.sorted
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
    val mat = jc.material()
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
    val mat = jl.material()
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
    val mat = js.material()
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
