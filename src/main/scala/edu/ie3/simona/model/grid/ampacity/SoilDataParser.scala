/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvDataSource
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.util.scala.quantities.*
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import play.api.libs.json.*
import squants.Meters
import squants.space.Length
import squants.thermal.Celsius

import java.nio.file.{Path}
import java.util.UUID
import scala.util.{Failure, Success, Try}

/** Utilities to parse soil related data from simple CSV files and provide
  * helpers to further process the parsed data.
  */
object SoilDataParser {

  private val expectedDepthRange: (Double, Double) = (-2.0, 0.0)

  /** Reads and validates both soil CSV files, then runs the full validation
    * suite against the parsed layers.
    *
    * @param args
    *   the command line arguments. The first argument must be the path to the
    *   soil types CSV, the second the path to the soil layers CSV.
    */
  def parseSoilData(args: Array[String]): Unit = {
    if args.length < 2 then
      throw new RuntimeException(
        "Usage: SoilDataParser <soilTypes.csv> <soilLayers.csv>"
      )

    val typesPath = Path.of(args(0))
    val layersPath = Path.of(args(1))

    val typesSource = csvDataSourceFor(typesPath)
    val layersSource = csvDataSourceFor(layersPath)

    val types = readOrFail(
      readSoilTypes(typesSource, typesPath.getFileName),
      "soil types",
    )
    val layers = readOrFail(
      readSoilLayers(layersSource, layersPath.getFileName),
      "soil layers",
    )

    val expectedRanges = layers
      .map(l => l.geometry)
      .distinct
      .map(g => g -> expectedDepthRange)
      .toMap

    validateAll(layers, expectedRanges, tolerance = 1e-6, types = types)
  }

  /** Builds a [[CsvDataSource]] rooted at the directory containing `path`.
    *
    * @param path
    *   the path of the CSV file to read.
    * @return
    *   a [[CsvDataSource]] whose base directory is the parent of `path`.
    */
  private def csvDataSourceFor(path: Path): CsvDataSource =
    new CsvDataSource(
      ",",
      Option(path.getParent).getOrElse(Paths.get(".")),
      new FileNamingStrategy(),
    )

  private def readOrFail[A](
      result: Try[A],
      label: String,
  ): A = result match {
    case Failure(e) =>
      throw new RuntimeException(s"Failed to read $label: ${e.getMessage}.", e)
    case Success(value) => value
  }

  /** Parse a CSV of soil types. Returns [[Try]] containing the parsed
    * [[SoilType]]s, or a [[Failure]] if the file cannot be read, the header is
    * invalid, or any data row is malformed (a [[Failure]] is produced by
    * letting the underlying exception propagate).
    */
  def readSoilTypes(source: CsvDataSource, fileName: Path): Try[Seq[SoilType]] =
    Try {
      source
        .getSourceData(fileName)
        .toList
        .asScala
        .map(m => m.asScala.toMap)
        .map(mapSoilType)
        .toSeq
    }

  /** Maps a single soil type CSV row to a [[SoilType]]. */
  private def mapSoilType(data: Map[String, String]): SoilType =
    SoilType(
      UUID.fromString(data("uuid")),
      data("id"),
      KelvinMetersPerWatt(data("thermalResistivityWet").toDouble),
      KelvinMetersPerWatt(data("thermalResistivityDry").toDouble),
      KilowattHoursPerCubicMeterKelvin(
        data("specificHeatCapacity").toDouble
      ),
      Celsius(data("criticalTemperatureDifference").toDouble),
    )

  /** Parse a CSV of soil layers. Returns [[Try]] containing the parsed
    * [[SoilLayer]]s, or a [[Failure]] if the file cannot be read, the header is
    * invalid, or any data row is malformed (a [[Failure]] is produced by
    * letting the underlying exception propagate).
    */
  def readSoilLayers(
      source: CsvDataSource,
      fileName: Path,
  ): Try[Seq[SoilLayer]] =
    Try {
      source
        .getSourceData(fileName)
        .toList
        .asScala
        .map(m => m.asScala.toMap)
        .map(mapSoilLayer)
        .toSeq
    }

  /** Maps a single soil layer CSV row to a [[SoilLayer]]. */
  private def mapSoilLayer(data: Map[String, String]): SoilLayer =
    SoilLayer(
      UUID.fromString(data("uuid")),
      parseGeoJsonToGeometry(data("geometry")),
      Meters(data("zFrom").toDouble),
      Meters(data("zTo").toDouble),
      UUID.fromString(data("soilType")),
    )

    process(0, 0, false)
    fields += sb.toString
    fields.result().toArray

  private def parseGeoJsonToGeometry(s: String): Geometry =
    try
      val js = Json.parse(s)
      (js \ "type").asOpt[String].map(_.toLowerCase) match
        case Some("polygon") =>
          val outerRing = (js \ "coordinates" \ 0).as[JsArray].value
          val pts = outerRing.map { p =>
            val arr = p.as[JsArray].value
            new Coordinate(arr(0).as[Double], arr(1).as[Double])
          }.toArray
          if pts.head != pts.last then
            throw new IllegalArgumentException(
              s"Expected closed polygon of soil layer: ${pts
                  .mkString("Array(", ", ", ")")}."
            )
          geometryFactory.createPolygon(pts)
        case Some(other) =>
          throw new IllegalArgumentException(
            s"Unsupported GeoJSON type: $other"
          )
        case None =>
          throw new IllegalArgumentException(
            s"Invalid GeoJSON: missing type: $s"
          )
    catch
      case e: Exception =>
        throw new IllegalArgumentException(
          s"Unable to parse geometry from geojson: $s",
          e,
        )

  /** Map each layer to its soil type (if available). Returns a sequence of
    * tuples `(layer, Option[SoilType])` where a missing type is represented as
    * `None`.
    */
  def associateLayersWithTypes(
      layers: Seq[SoilLayer],
      types: Seq[SoilType],
  ): Seq[(SoilLayer, Option[SoilType])] =
    val typesById: Map[UUID, SoilType] = types.map(t => t.uuid -> t).toMap
    layers.map(l => l -> typesById.get(l.soilType))

  /** Computes the total thickness per soil type UUID. */
  def totalThicknessBySoilType(layers: Seq[SoilLayer]): Map[UUID, Length] =
    layers
      .groupBy(_.soilType)
      .view
      .mapValues(_.map(_.thickness).reduce(_ + _))
      .toMap

  /** Runs the available validation routines.
    *
    * @param layers
    *   the soil layers to validate.
    * @param expectedRanges
    *   optional expected coverage ranges per geometry. If empty, no coverage
    *   validation is performed.
    * @param tolerance
    *   tolerance in meters for coverage / gap comparisons.
    * @param types
    *   optional sequence of known soil types. If provided, missing type
    *   references are reported.
    */
  def validateAll(
      layers: Seq[SoilLayer],
      expectedRanges: Map[Geometry, (Double, Double)] = Map.empty,
      tolerance: Double = 1e-6,
      types: Seq[SoilType] = Seq.empty,
  ): Unit = {
    validateNonOverlappingPerCoordinate(layers)
    validateNoGapsPerCoordinate(layers, tolerance)
    if expectedRanges.nonEmpty then
      validateCoverageAgainstRanges(layers, expectedRanges, tolerance)

    if types.nonEmpty then
      val missing = associateLayersWithTypes(layers, types).collect {
        case (l, None) => l
      }
      if missing.nonEmpty then
        throw new GridInconsistencyException(
          s"${missing.length} layers reference missing soil types: ${missing
              .map(_.uuid)
              .mkString(", ")}."
        )

    totalThicknessBySoilType(layers)
  }

  /** Ensures that for intersecting horizontal footprints the vertical intervals
    * `[zFrom, zTo]` of the layers do not overlap.
    */
  def validateNonOverlappingPerCoordinate(
      layers: Seq[SoilLayer]
  ): Unit = {
    val errors = for {
      i <- layers.indices
      j <- i + 1 until layers.length
      a = layers(i)
      b = layers(j)
      if a.geometry.intersects(b.geometry)
      aMin = math.min(a.zFrom.toMeters, a.zTo.toMeters)
      aMax = math.max(a.zFrom.toMeters, a.zTo.toMeters)
      bMin = math.min(b.zFrom.toMeters, b.zTo.toMeters)
      bMax = math.max(b.zFrom.toMeters, b.zTo.toMeters)
      if !(aMax <= bMin || bMax <= aMin)
    } yield s"Overlap between layers ${a.uuid} and ${b.uuid}"

    if errors.nonEmpty then
      throw new GridInconsistencyException(
        s"Soil validation failed for non overlapping layers: ${errors.mkString(", ")}"
      )
  }

  /** Validates that there are no gaps between adjacent layers per coordinate. A
    * gap is reported when the distance between the maximum depth of the
    * previous layer and the minimum depth of the next layer exceeds
    * `tolerance`.
    */
  def validateNoGapsPerCoordinate(
      layers: Seq[SoilLayer],
      tolerance: Double = 1e-6,
  ): Unit = {
    val errors = layers
      .groupBy(_.geometry)
      .values
      .flatMap { grp =>
        val intervals = grp
          .map(l =>
            (
              math.min(l.zFrom.toMeters, l.zTo.toMeters),
              math.max(l.zFrom.toMeters, l.zTo.toMeters),
            )
          )
          .sortBy(_._1)

        intervals
          .foldLeft((Option.empty[Double], List.empty[String])) {
            case ((None, acc), (_, max)) => (Some(max), acc)
            case ((Some(prevEnd), acc), (curStart, curEnd)) =>
              val newAcc = if curStart - prevEnd > tolerance then
                acc :+ f"Gap detected between depth ${prevEnd}%f and ${curStart}%f (size: ${curStart - prevEnd}%f)"
              else acc
              (Some(math.max(prevEnd, curEnd)), newAcc)
          }
          ._2
      }

    if errors.nonEmpty then
      throw new GridInconsistencyException(
        s"Validation of soil layers for gaps failed. ${errors.mkString(", ")}"
      )
  }

  /** Validates that the layers at each region fully cover the expected depth
    * ranges given in `expectedRanges`.
    */
  def validateCoverageAgainstRanges(
      layers: Seq[SoilLayer],
      expectedRanges: Map[Geometry, (Double, Double)],
      tolerance: Double = 1e-6,
  ): Unit = {
    val errors = expectedRanges.flatMap { case (region, (expMin, expMax)) =>
      val intervals = layers
        .filter(l => l.geometry.intersects(region))
        .map(l =>
          (
            math.min(l.zFrom.toMeters, l.zTo.toMeters),
            math.max(l.zFrom.toMeters, l.zTo.toMeters),
          )
        )
        .sortBy(_._1)

      val (current, coverageErrors) =
        intervals.foldLeft((expMin, List.empty[String])) {
          case ((cur, acc), (start, end)) =>
            val newAcc = if start - cur > tolerance then
              acc :+ f"Missing coverage between ${cur}%f and ${start}%f (size: ${start - cur}%f)"
            else acc
            (math.max(cur, end), newAcc)
        }

      val topError = if expMax - current > tolerance then
        List(
          f"Missing coverage at top between ${current}%f and ${expMax}%f (size: ${expMax - current}%f)"
        )
      else List.empty[String]

      val boundaryErrors = intervals.flatMap { case (s, e) =>
        (if s < expMin - tolerance then
           List(f"Layer starts below expected min ${s}%f < ${expMin}%f")
         else List.empty[String]) ++
          (if e > expMax + tolerance then
             List(f"Layer ends above expected max ${e}%f > ${expMax}%f")
           else List.empty[String])
      }

      coverageErrors ++ topError ++ boundaryErrors
    }

    if errors.nonEmpty then
      throw new GridInconsistencyException(
        s"Validation of soil layers for coverage against expected ranges failed. ${errors.mkString(", ")}"
      )
  }

}
