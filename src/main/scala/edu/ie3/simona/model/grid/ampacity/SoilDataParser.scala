/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.util.scala.quantities.*
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import play.api.libs.json.*
import squants.Meters
import squants.space.Length
import squants.thermal.Celsius

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.util.{Failure, Success, Try}

/** Utilities to parse soil related data from simple CSV files and provide
  * helpers to further process the parsed data.
  */
object SoilDataParser extends LazyLogging {

  private val soilTypeHeader = List(
    "uuid",
    "id",
    "thermal_resistivity_wet",
    "thermal_resistivity_dry",
    "specific_heat_capacity",
    "critical_temperature_difference",
  )

  private val soilLayerHeader = List(
    "uuid",
    "geometry",
    "z_from",
    "z_to",
    "soil_type",
  )

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

    val types = readOrFail(readSoilTypes(typesPath), "soil types")
    val layers = readOrFail(readSoilLayers(layersPath), "soil layers")

    val missing =
      associateLayersWithTypes(layers, types).collect { case (l, None) =>
        l
      }.length
    if missing > 0 then
      throw new RuntimeException(
        s"Warning: $missing layers reference missing soil types."
      )

    val expectedRanges = layers
      .map(l => l.geometry)
      .distinct
      .map(g => g -> expectedDepthRange)
      .toMap

    try validateAll(layers, expectedRanges, tolerance = 1e-6, types = types)
    catch case e: IllegalArgumentException => logger.error(e.getMessage)
  }

  private def readOrFail[A](
      result: Try[A],
      label: String,
  ): A = result match {
    case Failure(e) =>
      throw new RuntimeException(s"Failed to read $label: ${e.getMessage}.", e)
    case Success(value) => value
  }

  private def readAllLines(path: Path): Try[List[String]] = Try {
    Files.readAllLines(path).toArray(new Array[String](0)).toList.map(_.trim)
  }

  /** Verifies that the given header matches `expected` exactly (case-sensitive
    * and in the same order) and returns the remaining data rows. Throws an
    * [[IllegalArgumentException]] on any mismatch, an empty file, or a
    * header-only file.
    */
  private def splitHeader(
      content: List[String],
      expected: List[String],
      label: String,
  ): List[String] =
    if content.isEmpty then
      throw new IllegalArgumentException(
        s"Cannot parse $label: file contains no data rows."
      )

    val header = content.head.split(',').map(_.trim).toList
    if !header.sameElements(expected) then
      throw new IllegalArgumentException(
        s"Unexpected $label header: ${header.mkString(",")}. " +
          s"Expected exact header: ${expected.mkString(",")}."
      )

    content.tail

  /** Parse a CSV of soil types. Returns [[Try]] containing the parsed
    * [[SoilType]]s, or a [[Failure]] if the file cannot be read, the header is
    * invalid, or any data row is malformed (a [[Failure]] is produced by
    * letting the underlying exception propagate).
    */
  def readSoilTypes(path: Path): Try[Seq[SoilType]] =
    readAllLines(path).map { lines =>
      val content = lines.filterNot(l => l.isEmpty || l.startsWith("#"))
      val rows = splitHeader(content, soilTypeHeader, "soil types")

      rows.zipWithIndex.map { case (line, idx) =>
        val cols = line.split(',').map(_.trim)
        if cols.length != soilTypeHeader.length then
          throw new IllegalArgumentException(
            s"Invalid soil type line ${idx + 1}: '$line'"
          )
        SoilType(
          UUID.fromString(cols(0)),
          cols(1),
          KelvinMetersPerWatt(cols(2).toDouble),
          KelvinMetersPerWatt(cols(3).toDouble),
          KilowattHoursPerCubicMeterKelvin(cols(4).toDouble),
          Celsius(cols(5).toDouble),
        )
      }
    }

  /** Parse a CSV of soil layers. Returns [[Try]] containing the parsed
    * [[SoilLayer]]s, or a [[Failure]] if the file cannot be read, the header is
    * invalid, or any data row is malformed (a [[Failure]] is produced by
    * letting the underlying exception propagate).
    */
  def readSoilLayers(path: Path): Try[Seq[SoilLayer]] =
    readAllLines(path).map { lines =>
      val content = lines.filterNot(l => l.isEmpty || l.startsWith("#"))
      val rows = splitHeader(content, soilLayerHeader, "soil layers")

      rows.zipWithIndex.map { case (line, idx) =>
        val cols = splitCsvLine(line).map(_.trim)
        if cols.length != soilLayerHeader.length then
          throw new IllegalArgumentException(
            s"Invalid soil layer line ${idx + 1}: '$line'"
          )
        SoilLayer(
          UUID.fromString(cols(0)),
          parseGeoJsonToGeometry(unquoteCsvField(cols(1))),
          Meters(cols(2).toDouble),
          Meters(cols(3).toDouble),
          UUID.fromString(cols(4)),
        )
      }
    }

  /** Splits a CSV line on commas while ignoring commas that appear inside
    * braces or quotes, so that a quoted GeoJSON geometry survives as a single
    * field.
    */
  private def splitCsvLine(line: String): Array[String] =
    val fields = List.newBuilder[String]
    val sb = new StringBuilder

    def process(i: Int, depth: Int, inQuotes: Boolean): Unit =
      if i < line.length then
        val c = line.charAt(i)
        c match
          case '"' =>
            if inQuotes && i + 1 < line.length && line.charAt(i + 1) == '"' then
              sb.append('"')
              process(i + 2, depth, inQuotes)
            else
              sb.append(c)
              process(i + 1, depth, !inQuotes)
          case '{' if !inQuotes =>
            sb.append(c)
            process(i + 1, depth + 1, inQuotes)
          case '}' if !inQuotes =>
            sb.append(c)
            process(i + 1, math.max(0, depth - 1), inQuotes)
          case ',' if depth == 0 && !inQuotes =>
            fields += sb.toString
            sb.clear()
            process(i + 1, depth, inQuotes)
          case _ =>
            sb.append(c)
            process(i + 1, depth, inQuotes)

    process(0, 0, false)
    fields += sb.toString
    fields.result().toArray

  private def unquoteCsvField(field: String): String =
    val t = field.trim
    if t.length >= 2 && t.startsWith("\"") && t.endsWith("\"") then
      t.substring(1, t.length - 1).replace("\"\"", "\"")
    else t

  private val geometryFactory = new GeometryFactory()

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
            throw new RuntimeException(
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
      associateLayersWithTypes(layers, types).collect { case (l, None) => l }

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
      throw new RuntimeException(
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
      throw new RuntimeException(
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
      throw new RuntimeException(
        s"Validation of soil layers for coverage against expected ranges failed. ${errors.mkString(", ")}"
      )
  }

}
