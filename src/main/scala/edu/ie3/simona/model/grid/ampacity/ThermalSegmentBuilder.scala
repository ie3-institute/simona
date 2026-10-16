/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity
import com.typesafe.scalalogging.LazyLogging
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
import edu.ie3.datamodel.models.input.container.{
  SoilLayer,
  SoilType,
  SubGridContainer,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.ampacity.{CableSetup, LineSegmentThermalModel}
import edu.ie3.simona.util.{Coordinate, Coordinate3D}
import edu.ie3.util.geo.RichGeometries.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.scala.quantities.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import org.locationtech.jts.geom.{
  Geometry,
  GeometryFactory,
  Coordinate as JtsCoordinate,
}
import org.locationtech.jts.linearref.LengthIndexedLine
import org.locationtech.jts.operation.distance.DistanceOp
import squants.space.{Length, Millimeters}
import squants.{Meters, Temperature}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.UUID
import scala.collection.mutable.{ListBuffer, Set as MutableSet}
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

  /** Target distance between samples in Meters for parallel-proximity detection
    * along a straight segment.
    */
  private val sampleStepMeters = 1.0

  /** Safety cap on the number of samples used for parallel-proximity detection
    * along a single straight segment (10 km at the 1 m target step). When the
    * cap is reached, the effective sample distance grows beyond
    * [[sampleStepMeters]] and a warning is logged.
    */
  private val maxSamples: Int = 10000

  /** Result of building thermal segments.
    *
    * @param soilLayers
    *   The validated soil layers read from configuration.
    * @param thermalLineSegments
    *   The generated thermal line segments.
    */
  final case class BuildResult(
      soilLayers: Seq[SoilLayer],
      thermalLineSegments: Set[LineSegmentThermalModel],
  )

  /** A generated thermal line segment together with the geographic start and
    * end coordinates it was built from.
    *
    * @param segment
    *   The generated thermal line segment.
    * @param start
    *   The segment start coordinate.
    * @param end
    *   The segment end coordinate.
    */
  final case class BuiltSegment(
      segment: LineSegmentThermalModel,
      start: Coordinate,
      end: Coordinate,
  )

  /** Context with everything required to split a line into thermal segments.
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
    *   A mutable buffer to collect the generated segments.
    * @param neighbourLines
    *   Neighbour lines for parallel-proximity detection.
    * @param ppThreshold
    *   The parallel-proximity threshold in meters, if activated.
    * @param warnedPairs
    *   A set of already-warned (candidate, neighbour) UUID pairs.
    */
  final case class SegmentBuildContext(
      lineInput: LineInput,
      cable: CableSetup,
      coordinates: Seq[(Double, Double)],
      cableDepth: Length,
      cableDistance: Length,
      soilLayers: Seq[SoilLayer],
      soilTypes: Seq[SoilType],
      limitTemperature: Temperature,
      generatedSegments: ListBuffer[BuiltSegment],
      neighbourLines: Seq[NeighbourLineInfo],
      ppThreshold: Option[Double],
      warnedPairs: MutableSet[(UUID, UUID)],
  )

  /** Builds thermal line segments for the given subgrid and writes them to the
    * results.
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
    // 1. Extract soil types and layers from the SubGridContainer
    val soilLayers = extractSoilLayersFromContainer(subGridContainer)
    val soilTypes = extractSoilTypesFromContainer(subGridContainer)

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
    ) = generateThermalSegments(
      subGridContainer,
      soilLayers,
      soilTypes,
      neighbourLines,
      ppThreshold,
    )
    // 4. Write generated segments to output
    writeThermalSegmentsToOutput(
      generatedSegments,
      simonaConfig,
    )
    BuildResult(
      soilLayers,
      lineSegmentThermalModels,
    )
  }

  /** Extracts soil layers from the SubGridContainer
    */
  private def extractSoilLayersFromContainer(
      subGridContainer: SubGridContainer
  ): Seq[SoilLayer] = {
    subGridContainer
      .allEntitiesAsList()
      .asScala
      .collect { case l: SoilLayer =>
        l
      }
      .toSeq
  }

  /** Extracts soil types from SubGridContainer
    */
  private def extractSoilTypesFromContainer(
      subGridContainer: SubGridContainer
  ): Seq[SoilType] = {
    Option(subGridContainer.getSoilData)
      .map(_.getSoilTypes.asScala.toSeq)
      .getOrElse(Seq.empty)
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
    *   Neighbour lines for parallel-proximity detection.
    * @param ppThreshold
    *   The parallel-proximity threshold in meters, if activated.
    * @return
    *   A tuple of the generated segments and the generated [[BuiltSegment]]s
    *   with their start and end coordinates for output.
    */
  private def generateThermalSegments(
      subGridContainer: SubGridContainer,
      soilLayers: Seq[SoilLayer],
      soilTypes: Seq[SoilType],
      neighbourLines: Seq[NeighbourLineInfo],
      ppThreshold: Option[Double],
  ): (Set[LineSegmentThermalModel], ListBuffer[BuiltSegment]) = {
    val deploymentsByLine =
      subGridContainer.getRawGrid.getCableDeploymentsByLine.asScala
    val generatedSegments: ListBuffer[BuiltSegment] = ListBuffer.empty
    val warnedPairs: MutableSet[(UUID, UUID)] = MutableSet.empty
    val thermalLineSegments: Set[LineSegmentThermalModel] =
      subGridContainer.getRawGrid.getLines.asScala.flatMap { lineInput =>
        // Only build thermal segments if both a cable type and a cable deployment exist
        Option(lineInput.getType).toSeq.flatMap { lineType =>
          resolveCableType(lineType).toSeq.flatMap { cableTypeInput =>
            val deploymentListOpt =
              deploymentsByLine.get(lineInput.getUuid)
            val firstDeploymentOpt = singleDeploymentFor(
              lineInput,
              deploymentListOpt,
            )
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
              val isolation = layersList(cableTypeInput.getIsolation)
              val screen: Option[ScreenLayer] =
                cableTypeInput.getScreen.toScala.map(mapScreen)
              val filler = Option(cableTypeInput.getFiller)
                .map(layersList)
                .getOrElse(List.empty)
              val armor = Option(cableTypeInput.getArmor)
                .map(layersList)
                .getOrElse(List.empty)
              val jack = layersList(cableTypeInput.getJack)
              val deploymentPattern: String =
                Option(firstDeployment.getLayoutFormation)
                  .getOrElse(
                    throw new GridAgentInitializationException(
                      s"Ampacity build failed: No cable deployment pattern available for line ${lineInput.getUuid}"
                    )
                  )
              val cableDepth = Option(firstDeployment.getDepthCables)
                .map(_.toSquants)
                .getOrElse(
                  throw new GridAgentInitializationException(
                    s"Ampacity build failed: No cable depth available for line ${lineInput.getUuid}"
                  )
                )
              val cableDistance =
                Option(firstDeployment.getDistanceCables)
                  .map(_.toSquants)
                  .getOrElse(
                    throw new GridAgentInitializationException(
                      s"Ampacity build failed: No cable distance available for line ${lineInput.getUuid}"
                    )
                  )
              val limitTemperature =
                cableTypeInput.getLimitTemperature.toSquants
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
                limitTemperature,
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
              val context = SegmentBuildContext(
                lineInput,
                cable,
                coordinates,
                cableDepth,
                cableDistance,
                soilLayers,
                soilTypes,
                limitTemperature,
                generatedSegments,
                neighbourLines,
                ppThreshold,
                warnedPairs,
              )
              splitAndAssignSegments(context)
            }
          }
        }
      }.toSet
    (
      thermalLineSegments,
      generatedSegments,
    )
  }

  /** Splits a line into thermal segments at soil boundaries and, optionally, at
    * parallel-proximity stretch boundaries. The generated segments are
    * collected in the context's generated segments buffer.
    *
    * @param context
    *   The context with everything required to split the line.
    * @return
    *   The set of generated [[LineSegmentThermalModel]] instances.
    */
  private[ampacity] def splitAndAssignSegments(
      context: SegmentBuildContext
  ): Set[LineSegmentThermalModel] = {
    val geometryFactory = new GeometryFactory()
    val depthMeters = context.cableDepth.toMeters
    val soilTypesById: Map[UUID, SoilType] =
      context.soilTypes.map(t => t.getUuid -> t).toMap
    // Pre-compute neighbour centerlines for parallel-proximity detection.
    // NOTE: The haversine-on-sampled-points approach is geodesically consistent
    // but still relies on planar (lon, lat) sampling and a coarse per-sample
    // resolution. For full geometric coverage (exact metric proximity and
    // boundary placement at any latitude and segment heading), the geometries
    // should be projected into a metric coordinate reference system (e.g., UTM
    // or Mercator) before proximity evaluation. This is not yet implemented.
    val neighbourLineStrings: Seq[(NeighbourLineInfo, Geometry)] =
      context.ppThreshold match {
        case Some(_) =>
          context.neighbourLines
            .filter(_.lineUuid != context.lineInput.getUuid)
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
    context.coordinates
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
          context.soilLayers.flatMap { layer =>
            val boundary = layer.getGeometry.getBoundary
            val intersection = boundary.intersection(jtsLine)
            if intersection.isEmpty then Seq.empty
            else
              intersection.getCoordinates.toSeq
                .map(c => indexedLine.indexOf(c))
                .filterNot(_.isNaN)
          }
        // Parallel-proximity split indices for this straight segment
        val ppIndices: Seq[Double] =
          context.ppThreshold match {
            case Some(thresholdMeters) =>
              neighbourLineStrings.flatMap { case (neighbour, neighbourLine) =>
                if !neighbour.hasDeployment then
                  // Undeployed neighbour: warn and skip
                  val minDist =
                    neighbourLineDistanceMeters(
                      neighbour.geoPosition,
                      jtsLine,
                      geometryFactory,
                    )
                  if minDist < thresholdMeters then
                    val pairKey =
                      (context.lineInput.getUuid, neighbour.lineUuid)
                    if context.warnedPairs.add(pairKey) then
                      logger.warn(
                        s"Undeployed line ${neighbour.lineUuid} is within ${minDist}m of line ${context.lineInput.getUuid} (id: ${context.lineInput.getId}). No parallel-proximity split performed."
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
                        s"Parallel stretch between line ${context.lineInput.getUuid} (id: ${context.lineInput.getId}) " +
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
                context.soilLayers.filter(layer =>
                  layer.getGeometry.contains(midPoint)
                )
              if coveringLayers.isEmpty then
                throw new GridAgentInitializationException(
                  s"Ampacity build failed: No soil layer covers the subsegment midpoint " +
                    s"(${midCoord.x}, ${midCoord.y}) of line ${context.lineInput.getUuid} " +
                    s"between ${p1} and ${p2}."
                )
              val assignedLayer =
                coveringLayers
                  .find { layer =>
                    val minZ =
                      math.min(
                        layer.getZFrom.toSquants.toMeters,
                        layer.getZTo.toSquants.toMeters,
                      )
                    val maxZ =
                      math.max(
                        layer.getZFrom.toSquants.toMeters,
                        layer.getZTo.toSquants.toMeters,
                      )
                    depthMeters >= minZ && depthMeters <= maxZ
                  }
                  .getOrElse(
                    throw new GridAgentInitializationException(
                      s"Ampacity build failed: The cable depth of ${depthMeters} m is " +
                        s"not covered by any soil layer spanning line ${context.lineInput.getUuid} " +
                        s"at midpoint (${midCoord.x}, ${midCoord.y})."
                    )
                  )
              val assignedSoilType = soilTypesById.getOrElse(
                assignedLayer.getSoilType,
                throw new GridAgentInitializationException(
                  s"Ampacity build failed: Soil layer ${assignedLayer.getUuid} references " +
                    s"unknown soil type ${assignedLayer.getSoilType} for line ${context.lineInput.getUuid}."
                ),
              )
              val sCoord = toCoordinate(p1)
              val eCoord = toCoordinate(p2)
              val segmentUuid = UUID.randomUUID()
              val calculationPoint = SegmentCalculationPoints.coordinateFor(
                segmentUuid,
                sCoord,
                eCoord,
                context.soilLayers.map(_.getGeometry.getBoundary),
              )
              val influence: Seq[(UUID, Double)] =
                context.ppThreshold match {
                  case Some(thresholdMeters) =>
                    neighbourLineStrings
                      .filter(_._1.hasDeployment)
                      .filterNot(n =>
                        rejectedNeighbours.contains(n._1.lineUuid)
                      )
                      .flatMap { case (neighbour, _) =>
                        val distMeters =
                          neighbourLineDistanceMeters(
                            neighbour.geoPosition,
                            midPoint,
                            geometryFactory,
                          )
                        if distMeters < thresholdMeters
                        then Some((neighbour.lineUuid, distMeters))
                        else None
                      }
                  case None => Seq.empty
                }
              val segment = LineSegmentThermalModel(
                segmentUuid,
                s"LineTher_${context.lineInput.getId}_${sCoord.longitude}_${sCoord.latitude}_${eCoord.longitude}_${eCoord.latitude}",
                context.lineInput.getUuid,
                context.cable,
                Coordinate3D(sCoord.latitude, sCoord.longitude, depthMeters),
                Coordinate3D(eCoord.latitude, eCoord.longitude, depthMeters),
                context.cableDepth,
                context.cableDistance,
                KelvinMetersPerWatt(1), // Placeholder
                KelvinMetersPerWatt(1), // Placeholder
                KelvinMetersPerWatt(1), // Placeholder
                KelvinMetersPerWatt(1), // Placeholder
                JoulesPerCubicMeterKelvin(1), // Placeholder
                JoulesPerCubicMeterKelvin(1), // Placeholder
                JoulesPerCubicMeterKelvin(1), // Placeholder
                JoulesPerCubicMeterKelvin(1), // Placeholder
                JoulesPerCubicMeterKelvin(1), // Placeholder
                context.limitTemperature,
                assignedSoilType,
                calculationPoint,
                influence,
              )
              context.generatedSegments +=
                BuiltSegment(segment, sCoord, eCoord)
              segment
          }
          .toSet
      }
      .flatten
      .toSet
  }

  /** Converts a JTS coordinate (x = longitude, y = latitude) to a
    * [[Coordinate]].
    */
  private def toCoordinate(c: JtsCoordinate): Coordinate =
    Coordinate(c.y, c.x)

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
  private def neighbourLineDistanceMeters(
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
    * geodesically via the haversine formula. The number of samples is derived
    * from the geodesic length of the segment so that the sample distance stays
    * near [[sampleStepMeters]] at any latitude; the returned indices are planar
    * (lon, lat) indices within the given length-indexed line.
    *
    * @param indexedLine
    *   The length-indexed line of the candidate straight segment.
    * @param segLength
    *   The planar length of the candidate straight segment (used for indexing).
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
    val segLengthMeters =
      indexedLine
        .extractPoint(0.0)
        .haversineDistance(indexedLine.extractPoint(segLength))
        .getValue
        .doubleValue()
    val targetSamples = (segLengthMeters / sampleStepMeters).toInt + 1
    if targetSamples > maxSamples then
      logger.warn(
        s"Segment of ${segLengthMeters}m exceeds the sampling cap of ${maxSamples} samples; " +
          s"parallel-proximity resolution degrades to ${segLengthMeters / maxSamples}m per sample."
      )
    val n = math.max(
      2,
      math.min(maxSamples, targetSamples),
    )
    val sampleIdxs: Seq[Double] =
      (0 to n).map(i => segLength * i / n)
    val flags: Seq[Boolean] =
      sampleIdxs.map { idx =>
        val samplePoint = geometryFactory.createPoint(
          indexedLine.extractPoint(idx)
        )
        neighbourLineDistanceMeters(
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

  /** Maps a list of PSDM layer inputs to cable layers.
    */
  private def layersList(jl: java.util.List[JLayerInput]): List[Layer] =
    jl.asScala.map(mapLayer).toList

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

  /** Validates that a line has at most one cable deployment and returns the
    * single deployment if present.
    *
    * @param lineInput
    *   The candidate line.
    * @param deploymentListOpt
    *   The cable deployments assigned to the line, if any.
    * @return
    *   The single deployment, or `None` if the line has no deployment.
    */
  private[ampacity] def singleDeploymentFor(
      lineInput: LineInput,
      deploymentListOpt: Option[java.util.List[CableDeploymentInput]],
  ): Option[CableDeploymentInput] =
    deploymentListOpt match {
      case Some(deployments) =>
        val deploymentList = deployments.asScala.toSeq
        if deploymentList.size > 1 then
          throw new GridAgentInitializationException(
            s"Ampacity build failed: line ${lineInput.getUuid} (id: ${lineInput.getId}) " +
              s"has ${deploymentList.size} cable deployments. Only one cable deployment per line " +
              s"is supported. Please remove the surplus entries in cable_deployment_input.csv."
          )
        deploymentList.headOption
      case None => None
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
  private[ampacity] def writeThermalSegmentsToOutput(
      segments: Iterable[BuiltSegment],
      simonaConfig: SimonaConfig,
  ): Unit = {

    // If nothing to write, nothing to do
    if segments.isEmpty then return
    val baseOutputDir = Paths.get(simonaConfig.output.base.dir)
    val simulationName = simonaConfig.simulationName
    val runDirOpt: Option[Path] =
      try {
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
      } catch {
        case _: Exception => None
      }
    val runDir = runDirOpt.getOrElse(baseOutputDir.resolve(simulationName))
    val rawOutputDir = runDir.resolve("rawOutputData")
    Files.createDirectories(rawOutputDir)
    val outPath = rawOutputDir.resolve("thermal_line_segments.csv")
    val header =
      "segmentUuid,lineUuid,startX,startY,endX,endY,limitTemperature,hasParallelInfluence,neighbourLineUuids,minLateralDistanceMeters"
    val rows = segments.map { case BuiltSegment(segment, start, end) =>
      val influence = segment.parallelInfluence
      val hasInfluence = influence.nonEmpty
      val neighbourUuids = influence.map(_._1.toString).mkString(";")
      val distances = influence.map(_._2.toString).mkString(";")
      Seq(
        segment.uuid.toString,
        segment.lineUuid.toString,
        start.longitude.toString,
        start.latitude.toString,
        end.longitude.toString,
        end.latitude.toString,
        segment.upperBoundaryTemperature.value.toString,
        hasInfluence.toString,
        neighbourUuids,
        distances,
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

}
