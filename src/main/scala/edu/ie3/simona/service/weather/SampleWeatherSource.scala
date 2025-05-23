/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.source.IdCoordinateSource
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.util.TickUtil
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.geo.CoordinateDistance
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.locationtech.jts.geom.Point
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units
import squants.Kelvin
import squants.motion.MetersPerSecond
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.time.temporal.ChronoField.{HOUR_OF_DAY, MONTH_OF_YEAR, YEAR}
import java.util
import java.util.{Collections, Optional}
import javax.measure.quantity.Length
import scala.jdk.CollectionConverters._

final class SampleWeatherSource(
    private implicit val simulationStart: ZonedDateTime
) extends WeatherSource {
  private val resolution = 3600L
  override protected val idCoordinateSource: IdCoordinateSource = {
    SampleWeatherSource.SampleIdCoordinateSource
  }
  override val maxCoordinateDistance: ComparableQuantity[Length] =
    Quantities.getQuantity(50000d, Units.METRE)

  /** Get the weather data for the given tick as a weighted average taking into
    * account the given weighting of weather coordinates.
    *
    * @param tick
    *   Simulation date in question
    * @param weightedCoordinates
    *   The coordinate in question
    * @return
    *   Matching weather data
    */
  override def getWeather(
      tick: Long,
      weightedCoordinates: WeatherSource.WeightedCoordinates,
  ): WeatherData = getWeather(tick)

  /** Get the weather data for the given tick and coordinate. Here, the weather
    * data is taken repeatedly from a store The coordinate is not considered at
    * all.
    *
    * @param tick
    *   Simulation date in question
    * @return
    *   Matching weather data
    */
  private def getWeather(
      tick: Long
  ): WeatherData = {
    val simulationTime = tick.toDateTime
    val month = simulationTime.get(MONTH_OF_YEAR) - 1
    val hour = simulationTime.get(HOUR_OF_DAY)
    val year =
      if (
        simulationTime.get(YEAR) != 2011 && !(simulationTime
          .get(YEAR) == 2012 && month == 0)
      ) 2011
      else simulationTime.get(YEAR)
    val index = (((year - 2011) * 288) + (month * 24) + hour) + 1
    WeatherData(
      WattsPerSquareMeter(
        SampleWeatherSource
          .diffuseRadiation(index)
          .doubleValue
      ),
      WattsPerSquareMeter(
        SampleWeatherSource
          .directRadiation(index)
          .doubleValue
      ),
      Celsius(
        Kelvin(
          SampleWeatherSource
            .temperature(index)
            .doubleValue
        ).toCelsiusScale
      ),
      MetersPerSecond(
        SampleWeatherSource
          .windVelocity(index)
          .doubleValue
      ),
    )
  }

  /** Determine an Array with all ticks between the request frame's start and
    * end on which new data is available
    *
    * @param requestFrameStart
    *   Beginning of the announced request frame
    * @param requestFrameEnd
    *   End of the announced request frame
    * @return
    *   Array with data ticks
    */
  override def getDataTicks(
      requestFrameStart: Long,
      requestFrameEnd: Long,
  ): Array[Long] =
    TickUtil.getTicksInBetween(requestFrameStart, requestFrameEnd, resolution)
}

/** This model chooses weather data sampled arbitrarily from one weather station
  * (coordinate id = 213089, year 2011, the first of each month + January 2012)
  */
object SampleWeatherSource {
  object SampleIdCoordinateSource extends IdCoordinateSource {
    override def getSourceFields: Optional[util.Set[String]] =
      // only required for validation
      Optional.empty

    override def getCoordinate(id: Int): Optional[Point] =
      Optional.of(NodeInput.DEFAULT_GEO_POSITION)

    override def getCoordinates(ids: Int*): util.Collection[Point] =
      Collections.singletonList(NodeInput.DEFAULT_GEO_POSITION)

    override def getId(coordinate: Point): Optional[Integer] = Optional.of(1)

    override def getAllCoordinates: util.Collection[Point] =
      Collections.singletonList(NodeInput.DEFAULT_GEO_POSITION)

    override def getClosestCoordinates(
        coordinate: Point,
        n: Int,
        distance: ComparableQuantity[Length],
    ): util.List[CoordinateDistance] = {
      if (coordinate.getY.abs <= 90 && coordinate.getX.abs <= 180)
        Vector(
          new CoordinateDistance(
            coordinate,
            coordinate,
          )
        ).asJava
      else
        Vector.empty[CoordinateDistance].asJava
    }

    override def getNearestCoordinates(
        coordinate: Point,
        i: Int,
    ): util.List[CoordinateDistance] = {
      if (coordinate.getY.abs <= 90 && coordinate.getX.abs <= 180)
        Vector(
          new CoordinateDistance(
            coordinate,
            coordinate,
          )
        ).asJava
      else
        Vector.empty[CoordinateDistance].asJava
    }

    override def findCornerPoints(
        coordinate: Point,
        distance: ComparableQuantity[Length],
    ): util.List[CoordinateDistance] =
      findCornerPoints(
        coordinate,
        getClosestCoordinates(coordinate, 9, distance),
      )

    override def validate(): Unit = {
      /* nothing to do here */
    }
  }

  // these lists contain the hourly weather values for each first of the month of 2011 + january
  // 2012 at coordinate id 213089
  private val diffuseRadiation: Vector[Double] = Vector(0, 0, 0, 0, 0,
    1.18179e-12, 4.42315e-11, 0.0585938, 1.94141, 15.1172, 74.8438, 89.0469,
    104.062, 131.211, 98.3984, 74.5664, 9.52576e-10, 5.61295e-09, 3.74196e-09,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 4.3031e-12, 1.26833e-10, 0.123047, 21.7734,
    44.8906, 62.5, 116.304, 121.279, 124.103, 127.578, 126.474, 122.366,
    72.7656, 50.4609, 33.6406, 1.95692e-12, 9.96252e-13, 6.64168e-13, 0, 0, 0,
    0, 0, 0, 4.6561e-11, 0.179688, 7.00781, 70.375, 85.0805, 96.5268, 131.642,
    134.801, 136.495, 137.11, 133.496, 127.942, 95.6875, 79.7031, 57.375,
    2.50246e-10, 1.27398e-10, 8.49321e-11, 0, 0, 0, 0, 0, 0, 4.6561e-11,
    0.179688, 7.00781, 70.375, 85.0805, 96.5268, 131.642, 134.801, 136.495,
    137.11, 133.496, 127.942, 95.6875, 79.7031, 57.375, 2.50246e-10,
    1.27398e-10, 8.49321e-11, 0, 0, 0, 0, 0, 3.01617e-13, 0.320312, 9.46875,
    31.536, 137.816, 144.761, 148.295, 159.991, 179.943, 161.786, 167.365,
    187.983, 186.71, 190.492, 153.12, 109.372, 8.79227e-11, 4.47606e-11,
    2.98404e-11, 0, 0, 0, 0, 0, 0.0214844, 5.28906, 34.5625, 64.5749, 145.311,
    158.575, 177.106, 203.842, 208.607, 192.496, 153.726, 150.415, 146.707,
    122.146, 112.221, 99.3091, 32.2031, 16.3906, 10.9297, 0, 0, 0, 0, 0,
    0.046875, 9.05859, 30.9102, 49.6208, 111.284, 119.403, 126.256, 148.362,
    150.649, 151.829, 151.844, 149.184, 145.261, 123.728, 114.725, 103.147,
    43.0547, 21.9219, 14.6133, 0, 0, 0, 0, 0, 1.2934e-11, 0.460938, 13.4727,
    40.0492, 144.359, 178.505, 200.116, 217.874, 243.675, 270.285, 324.789,
    288.28, 246.589, 213.373, 161.242, 131.508, 27.2734, 13.8828, 9.25781, 0, 0,
    0, 0, 0, 2.57911e-13, 1.14858e-09, 0.28125, 10.9336, 98.3691, 119.471,
    122.866, 142.176, 228.197, 257.473, 168.816, 187.572, 185.407, 173.508,
    128.359, 98.2266, 1.78996e-10, 6.29864e-10, 1.1817e-09, 0, 0, 0, 0, 0, 0,
    5.39019e-11, 0.0664062, 2.85742, 73.9453, 94.8518, 100.687, 124.047,
    126.782, 127.841, 125.49, 120.944, 114.361, 73.6172, 52.5547, 35.0352, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 3.2974e-13, 4.56214e-11, 0.0996094, 18.0625, 45.7656,
    73.1016, 143.159, 158.414, 171.997, 164.156, 149.156, 124.133, 17.2539,
    8.78516, 5.85547, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4.99714e-11, 6.15368e-10,
    0.464844, 13.3242, 29.8594, 83.7578, 88.2578, 90.0703, 86.3281, 78.0234,
    64.9609, 7.0161e-09, 4.42477e-09, 2.94985e-09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 6.71801e-10, 0.0546875, 1.91406, 17.5312, 69.7578, 86.0391, 87.2578,
    102.062, 78.9297, 64.0508, 4.10171e-09, 7.74083e-09, 1.05629e-08, 0, 0, 0,
    0, 0)

  private val directRadiation: Vector[Double] = Vector(0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0.03125, 0.34375, 0.171875, 0.117188, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0.0136719, 3.625, 36.7266, 83.5, 278.094, 312.328, 333.422,
    350.391, 314.328, 266.484, 80.1172, 43.6875, 29.125, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0.0234375, 1.3125, 83.7109, 146.523, 211.234, 448.781, 481.25,
    499.109, 505.359, 469.375, 420.406, 187.695, 126.977, 84.8438, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0.0234375, 1.3125, 83.7109, 146.523, 211.234, 448.781,
    481.25, 499.109, 505.359, 469.375, 420.406, 187.695, 126.977, 84.8438, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0.0234375, 0.898438, 12.6953, 110.062, 183.016,
    262.141, 527.344, 517.719, 426.094, 559.203, 326.797, 230.422, 43.25,
    62.9062, 41.9453, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.210938, 6.19531, 23.0156,
    219.234, 278.797, 324.406, 536.797, 559.438, 600.734, 657.719, 622.281,
    573.391, 346.172, 279.25, 215.422, 9.6875, 4.92969, 3.28906, 0, 0, 0, 0, 0,
    0, 0.773438, 19.5391, 59.8203, 275.453, 342.297, 406.078, 630.578, 658.984,
    674.172, 675.344, 641.859, 596.781, 376.188, 309.078, 243.672, 21.9141,
    11.1562, 7.4375, 0, 0, 0, 0, 0, 0, 0, 0.0703125, 2.64062, 5.46875, 19.1094,
    49.7969, 217.547, 295.953, 307.719, 316.812, 332.766, 294.828, 131.562,
    158.281, 108.922, 2.04688, 1.04297, 0.695312, 0, 0, 0, 0, 0, 0, 0, 0,
    0.71875, 122.75, 167.562, 237.688, 489.781, 327.438, 254.609, 500.5,
    416.875, 331.672, 7.92188, 11.0703, 8.66406, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0.0820312, 13.0312, 58.9688, 129.969, 363.094, 388.562, 400.188, 386.297,
    348.484, 299.562, 86.7422, 48.3047, 32.1992, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0.00488281, 1.09375, 7.29688, 17.8672, 111.031, 120.531, 98.1484,
    25.0312, 28.1094, 25.5156, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0.078125, 2.65625, 19.6328, 114.727, 133.883, 142.406, 129.156, 101.867,
    73.3125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0546875,
    1.26562, 35.5156, 61.3516, 0.554688, 0.484375, 4.26562, 0, 0, 0, 0, 0, 0, 0,
    0)

  val windVelocity: Vector[Double] = Vector(4.155759, 4.16474, 4.918092, 5.3033,
    5.553537, 6.414151, 5.808621, 6.406849, 7.592254, 8.612377, 8.016528,
    8.526294, 9.143455, 8.801936, 9.673161, 9.93463, 9.342644, 8.159859,
    8.220303, 7.944492, 6.772916, 6.254803, 6.326622, 5.85534, 7.726576,
    8.03692, 7.89431, 7.553808, 7.619158, 8.442457, 8.407602, 8.513214,
    8.742652, 8.905468, 7.272316, 8.041487, 9.331848, 9.688264, 10.19103,
    10.81021, 11.11602, 11.16842, 10.64001, 10.20334, 9.241937, 9.55727,
    9.347568, 10.42712, 4.43063, 3.845876, 3.6041, 2.936424, 2.226154, 2.042998,
    1.750593, 1.785067, 1.878755, 1.61919, 1.621811, 1.687847, 1.368342,
    1.481512, 1.924683, 1.973318, 2.021676, 1.879806, 1.593722, 2.313831,
    2.270009, 2.379954, 2.45481, 2.360958, 4.43063, 3.845876, 3.6041, 2.936424,
    2.226154, 2.042998, 1.750593, 1.785067, 1.878755, 1.61919, 1.621811,
    1.687847, 1.368342, 1.481512, 1.924683, 1.973318, 2.021676, 1.879806,
    1.593722, 2.313831, 2.270009, 2.379954, 2.45481, 2.360958, 3.542508,
    3.727805, 4.127579, 4.152031, 3.607763, 2.661435, 2.74836, 2.798988,
    3.423525, 3.523311, 3.49244, 4.03484, 3.596967, 3.206293, 2.835315,
    3.757234, 3.352892, 4.570135, 5.133514, 6.24511, 6.236793, 6.814177,
    7.22784, 7.566368, 6.881512, 6.689921, 7.255632, 7.504581, 7.743653,
    8.21737, 8.119877, 8.245744, 8.99432, 9.017037, 8.362158, 7.968274,
    7.436713, 7.124018, 6.719967, 6.38225, 5.810755, 5.388939, 5.068966,
    4.575522, 4.03903, 3.297998, 2.934726, 2.769524, 4.378804, 4.056524,
    3.808471, 3.173417, 3.327576, 5.036839, 5.156245, 3.419165, 3.407172, 4.045,
    4.412901, 4.399883, 3.475394, 3.088157, 3.678887, 2.820831, 3.260706,
    4.082227, 4.382616, 4.050036, 5.237008, 4.211788, 4.624809, 4.601047,
    3.096792, 2.620996, 1.66859, 1.478464, 1.741077, 2.357662, 2.471333,
    2.041556, 1.148631, 0.9083933, 1.565658, 2.229594, 4.226667, 5.305191,
    6.421716, 7.51709, 6.695159, 8.08908, 8.756971, 8.72017, 7.099617, 5.175229,
    4.20865, 3.969436, 3.305677, 4.074029, 4.88233, 5.909486, 5.664249, 5.73041,
    5.025874, 5.082469, 7.486319, 7.48981, 9.145868, 10.1439, 9.447639,
    8.962342, 8.678671, 8.79894, 8.996054, 9.026779, 8.154106, 6.946157,
    5.300584, 3.597597, 3.562931, 2.406476, 5.547493, 5.597592, 5.313337,
    5.523605, 5.722849, 5.500268, 5.683947, 5.389264, 4.405135, 4.206182,
    4.046393, 4.183651, 4.176117, 4.067772, 3.554344, 3.721058, 4.145534,
    4.769235, 5.624875, 5.5847, 5.934193, 5.730165, 4.761456, 4.634291,
    3.053934, 3.230623, 2.273523, 2.144042, 3.035843, 3.618587, 3.96354,
    4.263217, 4.705784, 4.25851, 3.306112, 2.269481, 2.960408, 3.363742,
    3.322413, 4.076366, 5.078184, 4.248629, 3.532717, 4.028238, 4.128935,
    4.134281, 4.286064, 4.191655, 7.428819, 5.970714, 5.145741, 4.672889,
    3.676172, 2.838317, 2.238852, 1.64738, 1.028744, 1.589901, 1.719261,
    2.643034, 2.77937, 2.541408, 2.632023, 2.61833, 2.951166, 3.276715,
    3.134577, 3.424191, 3.723192, 3.534005, 3.880045, 3.735514, 8.580473,
    9.849699, 10.64576, 9.945789, 9.470883, 9.44867, 10.10593, 10.40204,
    9.776756, 10.57668, 10.40494, 10.58485, 9.949297, 11.54384, 13.34579,
    11.42129, 11.68356, 11.53926, 13.18045, 16.20823, 15.81948, 14.9177,
    16.75237, 16.75548)

  val temperature: Vector[Double] = Vector(270.573, 270.778, 270.624, 270.659,
    271.131, 271.532, 272.197, 271.717, 271.622, 271.87, 272.312, 272.592,
    272.991, 273.378, 273.545, 273.569, 273.611, 273.625, 273.572, 273.735,
    273.483, 272.44, 272.251, 272.936, 274.965, 274.27, 274.063, 273.852,
    273.848, 273.678, 273.523, 273.372, 273.541, 274.708, 277.153, 278.946,
    280.008, 280.96, 281.495, 280.903, 279.609, 277.607, 276.07, 275.078,
    274.071, 273.444, 273.498, 273.443, 277.302, 276.712, 276.062, 275.62,
    275.385, 275.051, 274.85, 275.869, 277.711, 280.118, 282.884, 285.318,
    287.311, 288.378, 288.818, 288.861, 288.457, 287.667, 286.063, 284.935,
    284.182, 283.358, 282.655, 281.229, 277.302, 276.712, 276.062, 275.62,
    275.385, 275.051, 274.85, 275.869, 277.711, 280.118, 282.884, 285.318,
    287.311, 288.378, 288.818, 288.861, 288.457, 287.667, 286.063, 284.935,
    284.182, 283.358, 282.655, 281.229, 284.764, 284.355, 284.241, 283.655,
    283.615, 283.388, 283.956, 285.123, 286.513, 288.29, 289.938, 290.835,
    290.251, 291.919, 291.06, 290.094, 290.991, 290.975, 289.719, 289.052,
    288.218, 287.195, 286.412, 285.68, 287.09, 286.991, 286.535, 286.342,
    285.985, 286.178, 287.4, 289.275, 290.541, 291.855, 293.042, 293.848,
    294.888, 295.596, 295.85, 296.005, 296.014, 295.466, 294.206, 292.974,
    291.944, 291.144, 290.695, 290.291, 292.156, 291.569, 290.984, 290.408,
    290.116, 290.271, 291.515, 293.363, 295.227, 297.166, 298.982, 300.292,
    301.453, 302.185, 302.549, 302.635, 302.37, 301.691, 300.348, 298.965,
    297.333, 296.385, 295.275, 294.214, 290.656, 290.319, 290.172, 289.626,
    289.539, 289.39, 289.917, 290.534, 291.621, 292.767, 293.977, 295.717,
    294.701, 295.327, 295.112, 294.958, 295.113, 294.886, 293.288, 291.533,
    289.841, 288.778, 287.832, 287.313, 288.815, 288.589, 287.977, 287.896,
    287.92, 287.276, 287.303, 287.741, 289.035, 290.122, 291.239, 291.153,
    291.424, 292.348, 292.084, 291.761, 291.365, 290.652, 289.744, 288.208,
    287.216, 286.511, 286.545, 286.77, 289.892, 289.289, 288.894, 288.688,
    288.516, 288.198, 288.003, 288.994, 290.367, 292.098, 293.951, 295.093,
    295.988, 296.57, 296.677, 296.394, 295.816, 294.329, 293.063, 291.924,
    290.958, 289.807, 289.126, 288.839, 282.31, 281.518, 281.236, 280.892,
    280.81, 280.538, 280.546, 280.664, 281.346, 282.618, 283.971, 285.159,
    286.135, 286.73, 287.166, 286.921, 286.347, 285.732, 284.903, 284.344,
    283.938, 283.599, 282.94, 282.558, 278.078, 277.519, 276.95, 276.604,
    276.458, 276.196, 276.118, 276.023, 275.984, 276.208, 277.389, 278.204,
    278.397, 278.31, 278.143, 277.495, 276.959, 276.495, 275.674, 275.399,
    274.841, 274.285, 274.135, 273.734, 277.064, 277.331, 277.268, 277.069,
    277.223, 276.519, 276.033, 276.501, 276.63, 276.993, 277.384, 277.604,
    277.661, 277.913, 277.535, 276.538, 276.14, 276.052, 276.441, 276.738,
    276.529, 276.68, 276.68, 276.121)
}
