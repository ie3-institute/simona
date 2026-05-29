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

/** Utilities to parse soil related data from simple CSV files and provide
  * helpers to further process the parsed data.
  */
object SoilDataParser extends LazyLogging {
  def parseSoilData(args: Array[String]): Unit = {
    if args.length < 2 then {
      logger.warn("Usage: SoilDataParser <soilTypes.csv> <soilLayers.csv>")
      System.exit(1)
    }

    val typesPath = Path.of(args(0))
    val layersPath = Path.of(args(1))

    SoilDataParser.readSoilTypes(typesPath) match {
      case Failure(e) =>
        logger.warn(s"Failed to read soil types: ${e.getMessage}")
        System.exit(2)
      case Success(types) => logger.debug(s"Read ${types.length} soil types")
    }

    SoilDataParser.readSoilLayers(layersPath) match {
      case Failure(e) =>
        logger.warn(s"Failed to read soil layers: ${e.getMessage}")
        System.exit(3)
      case Success(layers) =>
        logger.debug(s"Read ${layers.length} soil layers")

        val assoc = SoilDataParser.associateLayersWithTypes(
          layers,
          SoilDataParser.readSoilTypes(typesPath).getOrElse(Seq.empty),
        )
        val missing = assoc.collect { case (l, None) => l }.length
        if missing > 0 then
          logger.warn(s"Warning: $missing layers reference missing soil types")

        try {
          val expected = layers
            .map(l => (l.x, l.y))
            .distinct
            .map(c => c -> (-2.0, 0.0))
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
        if cols.length < 6 then
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

// Aggregate failures
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
        if content.nonEmpty && content.head.toLowerCase.contains("x") then
          content.tail
        else content

      val parsed = rows.zipWithIndex.map { case (line, idx) =>
        val cols = line.split(',').map(_.trim)
        if cols.length < 5 then
          Failure(
            new IllegalArgumentException(
              s"Invalid soil layer line ${idx + 1}: '$line'"
            )
          )
        else
          Try {
            val x = cols(0).toDouble
            val y = cols(1).toDouble
            val zFrom = Meters(cols(2).toDouble)
            val zTo = Meters(cols(3).toDouble)
            val uuid = UUID.fromString(cols(4))
            SoilLayer(x, y, zFrom, zTo, uuid)
          }
      }

      val failures = parsed.collect { case Failure(e) => e }
      if failures.nonEmpty then
        Failure(
          new Exception(
            s"Errors parsing soil layers: ${failures.map(_.getMessage).mkString(", ")}"
          )
        )
      else Success(parsed.collect { case Success(v) => v })
    }

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
      expectedRanges: Map[(Double, Double), (Double, Double)] = Map.empty,
      tolerance: Double = 1e-6,
      types: Seq[SoilType] = Seq.empty,
  ): Unit = {
    val overlaps = validateNonOverlappingPerCoordinate(layers)
    val gaps = validateNoGapsPerCoordinate(layers, tolerance)
    val coverage =
      if expectedRanges.nonEmpty then
        validateCoverageAgainstRanges(layers, expectedRanges, tolerance)
      else Map.empty[(Double, Double), List[String]]

    val missingTypes =
      if types.nonEmpty then
        associateLayersWithTypes(layers, types).collect { case (l, None) => l }
      else Seq.empty

    val totals = totalThicknessBySoilType(layers)

    val layerDepthViolations: Seq[String] = Seq.empty
  }

  /** Simple validation: ensure that for each (x,y) the layers do not overlap
    * (i.e. intervals [zFrom, zTo] are disjoint). Returns a map from (x,y) to
    * list of detected overlap errors (empty list means no overlaps).
    */
  def validateNonOverlappingPerCoordinate(
      layers: Seq[SoilLayer]
  ): Unit = {
    val errors = layers
      .groupBy(l => (l.x, l.y))
      .view
      .mapValues { grp =>
        val sorted = grp.sortBy(_.zFrom)
        val errors = scala.collection.mutable.ListBuffer.empty[String]
        for i <- sorted.indices do {
          val a = sorted(i)
          for j <- i + 1 until sorted.length do {
            val b = sorted(j)
            val aMin = math.min(a.zFrom.toMeters, a.zTo.toMeters)
            val aMax = math.max(a.zFrom.toMeters, a.zTo.toMeters)
            val bMin = math.min(b.zFrom.toMeters, b.zTo.toMeters)
            val bMax = math.max(b.zFrom.toMeters, b.zTo.toMeters)
            if !(aMax <= bMin || bMax <= aMin) then
              errors += s"Overlap between layers at index ${i} and ${j} for coordinate (${a.x},${a.y})"
          }
        }
        errors.toList
      }
      .toMap
      .values
      .flatten

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
      .groupBy(l => (l.x, l.y))
      .view
      .mapValues { grp =>
        val intervals = grp
          .map(l =>
            (
              math.min(l.zFrom.toMeters, l.zTo.toMeters),
              math.max(l.zFrom.toMeters, l.zTo.toMeters),
            )
          )
          .sortBy(_._1)
        val errors = scala.collection.mutable.ListBuffer.empty[String]
        if intervals.nonEmpty then {
          var prevEnd = intervals.head._2
          for i <- 1 until intervals.length do {
            val (curStart, curEnd) = intervals(i)
            if curStart - prevEnd > tolerance then
              errors += f"Gap detected between depth ${prevEnd}%f and ${curStart}%f (size: ${curStart - prevEnd}%f)"
            prevEnd = math.max(prevEnd, curEnd)
          }
        }
        errors.toList
      }
      .toMap
      .values
      .flatten

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
      expectedRanges: Map[(Double, Double), (Double, Double)],
      tolerance: Double = 1e-6,
  ): Unit = {
    val errors = expectedRanges
      .map { case (coord, (expMin, expMax)) =>
        val grp = layers.filter(l => (l.x, l.y) == coord)
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

        coord -> errors.toList
      }
      .values
      .flatten

    if errors.nonEmpty then
      throw new RuntimeException(
        s"Validation of soil layers for coverage against expected ranges failed. ${errors.mkString(", ")}"
      )
  }

}
