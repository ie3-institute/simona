/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.util.scala.quantities.*
import squants.Meters
import squants.space.Length
import squants.thermal.Celsius

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.util.{Failure, Success, Try}
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import play.api.libs.json.*

/** Utilities to parse soil related data from simple CSV files and provide
  * helpers to further process the parsed data.
  */
object SoilDataParser extends LazyLogging {
  def parseSoilData(args: Array[String]): Unit = {
    if args.length < 2 then {
      throw new RuntimeException(
        "Usage: SoilDataParser <soilTypes.csv> <soilLayers.csv>"
      )
    }

    val typesPath = Path.of(args(0))
    val layersPath = Path.of(args(1))

    SoilDataParser.readSoilTypes(typesPath) match {
      case Failure(e) =>
        throw new RuntimeException(
          s"Failed to read soil types: ${e.getMessage}."
        )

      case Success(types) => logger.debug(s"Read ${types.length} soil types.")
    }

    SoilDataParser.readSoilLayers(layersPath) match {
      case Failure(e) =>
        throw new RuntimeException(
          s"Failed to read soil layers: ${e.getMessage}."
        )
      case Success(layers) =>
        logger.debug(s"Read ${layers.length} soil layers")
        val assoc = SoilDataParser.associateLayersWithTypes(
          layers,
          SoilDataParser.readSoilTypes(typesPath).getOrElse(Seq.empty),
        )
        val missing = assoc.collect { case (l, None) => l }.length
        if missing > 0 then
          throw new RuntimeException(
            s"Warning: $missing layers reference missing soil types."
          )

        try {
          val expected = layers
            .map(l => l.geometry)
            .distinct
            .map(g => g -> (-2.0, 0.0))
            .toMap
          val typesSeq =
            SoilDataParser.readSoilTypes(typesPath).getOrElse(Seq.empty)
          SoilDataParser.validateAll(
            layers,
            expectedRanges = expected,
            tolerance = 1e-6,
            types = typesSeq,
          )
        } catch {
          case e: IllegalArgumentException =>
            logger.error(e.getMessage)
        }
    }
  }

  private def readAllLines(path: Path): Try[List[String]] = Try {
    val lines = Files.readAllLines(path).toArray(new Array[String](0)).toList
    lines.map(_.trim)
  }

  /** Parse a CSV of soil types. Returns Try[Seq[SoilType]] with parsing errors
    * bubbled up as Failure.
    */
  def readSoilTypes(path: Path): Try[Seq[SoilType]] =
    readAllLines(path).flatMap { lines =>
      val content = lines.filterNot(l => l.isEmpty || l.startsWith("#"))
      val rows =
        if content.nonEmpty && content.head.toLowerCase.contains("uuid") then
          content.tail
        else content

      val parsed = rows.zipWithIndex.map { case (line, idx) =>
        val cols = line.split(',').map(_.trim)
        if cols.length != 6 then
          Failure(
            new IllegalArgumentException(
              s"Invalid soil type line ${idx + 1}: '$line'"
            )
          )
        else
          Try {
            val uuid = UUID.fromString(cols(0))
            val trWet = KelvinMetersPerWatt(cols(2).toDouble)
            val trDry = KelvinMetersPerWatt(cols(3).toDouble)
            val shc = KilowattHoursPerKelvinCubicMeters(cols(4).toDouble)
            val critTemp = Celsius(cols(5).toDouble)

            SoilType(uuid, cols(1), trWet, trDry, shc, critTemp)
          }
      }

      val failures = parsed.collect { case Failure(e) => e }
      if failures.nonEmpty then
        Failure(
          new Exception(
            s"Errors parsing soil types: ${failures.map(_.getMessage).mkString(", ")}"
          )
        )
      else Success(parsed.collect { case Success(v) => v })
    }

  /** Parse a CSV of soil layers. */
  def readSoilLayers(path: Path): Try[Seq[SoilLayer]] =
    readAllLines(path).flatMap { lines =>
      val content = lines.filterNot(l => l.isEmpty || l.startsWith("#"))
      val rows =
        if content.nonEmpty && content.head.toLowerCase.contains("uuid") then
          content.tail
        else content

      val parsed = rows.zipWithIndex.map { case (line, idx) =>
        val cols = splitCsvLine(line).map(_.trim)
        if cols.length != 5 then
          Failure(
            new IllegalArgumentException(
              s"Invalid soil layer line ${idx + 1}: '$line'"
            )
          )
        else
          Try {
            val uuid = UUID.fromString(cols(0))
            val geoColRaw = cols(1)
            val geoCol = unquoteCsvField(geoColRaw)
            val geometry = parseGeoJsonToGeometry(geoCol)
            val zFrom = Meters(cols(2).toDouble)
            val zTo = Meters(cols(3).toDouble)
            val soilType = UUID.fromString(cols(4))

            SoilLayer(uuid, geometry, zFrom, zTo, soilType)
          }
      }

      val failures = parsed.collect { case Failure(e) => e }
      if failures.nonEmpty then
        throw new RuntimeException(
          s"Errors parsing soil layers: ${failures.map(_.getMessage).mkString(", ")}."
        )
      else Success(parsed.collect { case Success(v) => v })
    }

  /** Split a CSV line on commas but ignore commas that are inside braces or
    * quotes.
    */
  private def splitCsvLine(line: String): Array[String] = {
    val buf = new scala.collection.mutable.ArrayBuffer[String]
    val sb = new StringBuilder
    var braceDepth = 0
    var inQuotes = false
    var i = 0
    while i < line.length do
      val c = line.charAt(i)
      c match
        case '"' =>
          // handle escaped double quotes inside quoted field: "" -> append a single '"' and do not toggle state
          if inQuotes && i + 1 < line.length && line.charAt(i + 1) == '"' then
            sb.append('"')
            i += 1 // skip the escaped quote
          else
            inQuotes = !inQuotes
            sb.append(c)
        case '{' if !inQuotes =>
          braceDepth += 1
          sb.append(c)
        case '}' if !inQuotes =>
          braceDepth = Math.max(0, braceDepth - 1)
          sb.append(c)
        case ',' if braceDepth == 0 && !inQuotes =>
          buf += sb.toString
          sb.clear()
        case _ => sb.append(c)
      i += 1
    buf += sb.toString
    buf.toArray
  }

  private val geometryFactory = new GeometryFactory()

  private def unquoteCsvField(field: String): String =
    val t = field.trim
    if t.length >= 2 && t.startsWith("\"") && t.endsWith("\"") then
      // remove surrounding quotes and unescape doubled quotes
      t.substring(1, t.length - 1).replace("\"\"", "\"")
    else t

  private def parseGeoJsonToGeometry(s: String): Geometry =
    try
      val js = Json.parse(s)
      (js \ "type").asOpt[String] match
        case Some(tpe) =>
          tpe.toLowerCase match
            case "polygon" =>
              val rings = (js \ "coordinates").as[JsArray].value
              val outerRing = rings.head.as[JsArray].value
              val pts = outerRing.map { p =>
                val arr = p.as[JsArray].value
                new Coordinate(arr(0).as[Double], arr(1).as[Double])
              }.toArray
              // check for closed ring
              val closed =
                if pts.head == pts.last then pts
                else
                  throw new RuntimeException(
                    s"Expected closed polygon of soil layer: ${pts
                        .mkString("Array(", ", ", ")")}."
                  )
              geometryFactory.createPolygon(closed)
            case other =>
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
    * tuples (layer, Option[SoilType]) where missing types are represented as
    * None.
    */
  def associateLayersWithTypes(
      layers: Seq[SoilLayer],
      types: Seq[SoilType],
  ): Seq[(SoilLayer, Option[SoilType])] = {
    val typesById: Map[UUID, SoilType] = types.map(t => t.uuid -> t).toMap
    layers.map(l => l -> typesById.get(l.soilType))
  }

  /** Compute total thickness per soil type UUID. */
  def totalThicknessBySoilType(layers: Seq[SoilLayer]): Map[UUID, Length] = {
    layers
      .groupBy(_.soilType)
      .view
      .mapValues(_.map(_.thickness).reduce(_ + _))
      .toMap
  }

  /** Combined wrapper that runs the available validation routines and returns a
    * `ValidationReport` summarising findings.
    *
    * Parameters:
    *   - `expectedRanges`: optional expected coverage ranges per coordinate. If
    *     empty no coverage validation is performed.
    *   - `types`: optional sequence of known soil types. If provided the
    *     association is checked and missing type references are reported.
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

  /** Simple validation: ensure that for each (x,y) the layers do not overlap
    * (i.e. intervals [zFrom, zTo] are disjoint). Returns a map from (x,y) to
    * list of detected overlap errors (empty list means no overlaps).
    */
  def validateNonOverlappingPerCoordinate(
      layers: Seq[SoilLayer]
  ): Unit = {
    val errors = scala.collection.mutable.ListBuffer.empty[String]
    for i <- layers.indices do {
      val a = layers(i)
      for j <- i + 1 until layers.length do {
        val b = layers(j)
        // if horizontal footprints intersect and vertical intervals overlap -> overlap
        if a.geometry.intersects(b.geometry) then {
          val aMin = math.min(a.zFrom.toMeters, a.zTo.toMeters)
          val aMax = math.max(a.zFrom.toMeters, a.zTo.toMeters)
          val bMin = math.min(b.zFrom.toMeters, b.zTo.toMeters)
          val bMax = math.max(b.zFrom.toMeters, b.zTo.toMeters)
          if !(aMax <= bMin || bMax <= aMin) then
            errors += s"Overlap between layers ${a.uuid} and ${b.uuid}"
        }
      }
    }

    if errors.nonEmpty then
      throw new RuntimeException(
        s"Soil validation failed for non overlapping layers: ${errors.mkString(", ")}"
      )
  }

  /** Validate that there are no gaps between adjacent layers for each
    * coordinate (x,y). A gap is reported if the difference between the previous
    * layer's maximum depth and the next layer's minimum depth is larger than
    * `tolerance`.
    *
    * Returns a map from (x,y) to a list gap descriptions.
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
        val errs = scala.collection.mutable.ListBuffer.empty[String]
        if intervals.nonEmpty then {
          var prevEnd = intervals.head._2
          for i <- 1 until intervals.length do {
            val (curStart, curEnd) = intervals(i)
            if curStart - prevEnd > tolerance then
              errs += f"Gap detected between depth ${prevEnd}%f and ${curStart}%f (size: ${curStart - prevEnd}%f)"
            prevEnd = math.max(prevEnd, curEnd)
          }
        }
        errs.toList
      }

    if errors.nonEmpty then
      throw new RuntimeException(
        s"Validation of soil layers for gaps failed. ${errors.mkString(", ")}"
      )
  }

  /** Validate that layers at coordinates fully cover the expected depth ranges
    * provided in `expectedRanges`. The map keys are (x,y) coordinates and
    * values are (minDepth, maxDepth) of expected coverage. Returns for each
    * coordinate a list of missing coverage segments or boundary violations.
    */
  def validateCoverageAgainstRanges(
      layers: Seq[SoilLayer],
      expectedRanges: Map[Geometry, (Double, Double)],
      tolerance: Double = 1e-6,
  ): Unit = {
    val errors = expectedRanges
      .map { case (region, (expMin, expMax)) =>
        val grp = layers.filter(l => l.geometry.intersects(region))
        val intervals = grp
          .map(l =>
            (
              math.min(l.zFrom.toMeters, l.zTo.toMeters),
              math.max(l.zFrom.toMeters, l.zTo.toMeters),
            )
          )
          .sortBy(_._1)
        val errors = scala.collection.mutable.ListBuffer.empty[String]

        // check for coverage from expMin to expMax
        var current = expMin
        for (start, end) <- intervals do {
          if start - current > tolerance then
            // missing segment
            errors += f"Missing coverage between ${current}%f and ${start}%f (size: ${start - current}%f)"
          current = math.max(current, end)
        }

        if expMax - current > tolerance then
          errors += f"Missing coverage at top between ${current}%f and ${expMax}%f (size: ${expMax - current}%f)"

        // check for layers exceeding expected boundaries
        intervals.foreach { case (s, e) =>
          if s < expMin - tolerance then
            errors += f"Layer starts below expected min ${s}%f < ${expMin}%f"
          if e > expMax + tolerance then
            errors += f"Layer ends above expected max ${e}%f > ${expMax}%f"
        }

        region -> errors.toList
      }
      .values
      .flatten

    if errors.nonEmpty then
      throw new RuntimeException(
        s"Validation of soil layers for coverage against expected ranges failed. ${errors.mkString(", ")}"
      )
  }

}
