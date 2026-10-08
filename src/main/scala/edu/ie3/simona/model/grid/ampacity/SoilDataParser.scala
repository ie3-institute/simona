/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvDataSource
import edu.ie3.util.scala.quantities.*
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import play.api.libs.json.*
import squants.Meters
import squants.space.Length
import squants.thermal.Celsius

import java.nio.file.{Files, Path, Paths}
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

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
    *
    * Expected header names (case-sensitive): `uuid`, `id`,
    * `thermal_resistivity_wet`, `thermal_resistivity_dry`,
    * `specific_heat_capacity`, `critical_temperature_difference`.
    */
  def readSoilTypes(path: Path): Try[Seq[SoilType]] = Try {
    val baseDir =
      if path.getParent != null then path.getParent else Paths.get(".")
    val csvDs = new CsvDataSource(",", baseDir, new FileNamingStrategy())

    val headersOpt = csvDs.getSourceFields(path)
    if !headersOpt.isPresent then
      throw new IllegalArgumentException(
        s"Unable to determine headers for file: $path"
      )
    val headers = headersOpt.get().asScala.toSeq

    val required = Seq(
      "uuid",
      "id",
      "thermal_resistivity_wet",
      "thermal_resistivity_dry",
      "specific_heat_capacity",
      "critical_temperature_difference",
    )
    val missing = required.filterNot(h => headers.contains(h))
    if missing.nonEmpty then
      throw new IllegalArgumentException(
        s"Missing required columns in $path: ${missing
            .mkString(", ")}. Available: ${headers.mkString(", ")}"
      )

    val stream = csvDs.getSourceData(path)
    val rows = stream.iterator().asScala.map(_.asScala.toMap).toList
    if rows.isEmpty then
      throw new IllegalArgumentException(s"Empty file: $path")

    val parsed = rows.zipWithIndex.map { case (row, idx) =>
      Try {
        def getVal(key: String): String =
          row.getOrElse(
            key,
            throw new IllegalArgumentException(
              s"Missing value for column '$key' in row ${idx + 1} (file: $path)"
            ),
          )

        val uuid = UUID.fromString(getVal("uuid"))
        val name = getVal("id")
        val trWet =
          KelvinMetersPerWatt(getVal("thermalResistivityWet").trim.toDouble)
        val trDry =
          KelvinMetersPerWatt(getVal("thermalResistivityDry").trim.toDouble)
        val shc = KilowattHoursPerCubicMeterKelvin(
          getVal("specificHeatCapacity").trim.toDouble
        )
        val critTempDiff =
          Celsius(getVal("criticalTemperatureDifference").trim.toDouble)

        SoilType(uuid, name, trWet, trDry, shc, critTempDiff)
      }
    }

    val failures = parsed.collect { case Failure(e) => e }
    if failures.nonEmpty then
      throw new RuntimeException(
        s"Errors parsing soil types: ${failures.map(_.getMessage).mkString(", ")}"
      )

    parsed.collect { case Success(v) => v }
  }

  /** Parse a CSV of soil layers. Returns Try[Seq[SoilLayer]] with parsing
    * errors.
    *
    * Expected header names (case-sensitive): `uuid`, `geometry`, `z_from`,
    * `z_to`, `soil_type`.
    *
    * The `geometry` field is expected to contain a GeoJSON Polygon or
    * MultiPolygon as a single CSV cell.
    */
  def readSoilLayers(path: Path): Try[Seq[SoilLayer]] = Try {
    val baseDir =
      if path.getParent != null then path.getParent else Paths.get(".")
    val csvDs = new CsvDataSource(",", baseDir, new FileNamingStrategy())

    val headersOpt = csvDs.getSourceFields(path)
    if !headersOpt.isPresent then
      throw new IllegalArgumentException(
        s"Unable to determine headers for file: $path"
      )
    val headers = headersOpt.get().asScala.toSeq

    val required = Seq("uuid", "geometry", "z_from", "z_to", "soil_type")
    val missing = required.filterNot(h => headers.contains(h))
    if missing.nonEmpty then
      throw new IllegalArgumentException(
        s"Missing required columns in $path: ${missing
            .mkString(", ")}. Available: ${headers.mkString(", ")}"
      )

    val stream = csvDs.getSourceData(path)
    val rows = stream.iterator().asScala.map(_.asScala.toMap).toList
    if rows.isEmpty then
      throw new IllegalArgumentException(s"Empty file: $path")

    val parsed = rows.zipWithIndex.map { case (row, idx) =>
      Try {
        def getVal(key: String): String =
          row.getOrElse(
            key,
            throw new IllegalArgumentException(
              s"Missing value for column '$key' in row ${idx + 1} (file: $path)"
            ),
          )

        val uuid = UUID.fromString(getVal("uuid"))
        val geoCol = getVal("geometry")
        val geometry = parseGeoJsonToGeometry(geoCol)
        val zFrom = Meters(getVal("zFrom").trim.toDouble)
        val zTo = Meters(getVal("zTo").trim.toDouble)
        val soilType = UUID.fromString(getVal("soilType"))

        SoilLayer(uuid, geometry, zFrom, zTo, soilType)
      }
    }

    val failures = parsed.collect { case Failure(e) => e }
    if failures.nonEmpty then
      throw new RuntimeException(
        s"Errors parsing soil layers: ${failures.map(_.getMessage).mkString(", ")}."
      )

    parsed.collect { case Success(v) => v }
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
