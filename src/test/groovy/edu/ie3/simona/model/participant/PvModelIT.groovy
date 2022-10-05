/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.io.grid.CsvGridSource
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.interfaces.Irradiance
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVRecord
import spock.lang.Shared
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import javax.measure.quantity.Power
import java.time.ZonedDateTime
import java.util.zip.GZIPInputStream

import static edu.ie3.util.quantities.PowerSystemUnits.MEGAWATT
import static edu.ie3.util.quantities.PowerSystemUnits.PU
import static java.util.Locale.US
import static java.util.Locale.setDefault
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.*

/**
 * A simple integration test that uses pre-calculated data to check if the pv model works as expected.
 * It uses 8 pv models located in GER.
 *
 */
class PvModelIT extends Specification implements PvModelITHelper {

	@Shared
	HashMap<String, PvModel> pvModels

	@Shared
	HashMap<ZonedDateTime, HashMap<String, WeatherMessage.WeatherData>> weatherMap

	@Shared
	HashMap<ZonedDateTime, HashMap<String, Quantity<Power>>> resultsMap


	def setupSpec() {
		// input weather data values are in us format
		// if locale is not set hard coded to US, quantity parsing will return invalid values
		setDefault(US)

		pvModels = getPvModels()
		weatherMap = getWeatherData()
		resultsMap = getResultsData()
	}

	def "8 pv panels full year test"() {

		given: "an empty array to collect the results"
		ArrayList<Double> testRes = new ArrayList<>()

		when: "we calculate the photovoltaic in-feed for each unit for the whole year"
		List<String> modelIds = new ArrayList<>(pvModels.keySet())
		// sort models: east, south, west
		Collections.sort(modelIds)
		final int modelCount = modelIds.size()

		// sort, in case dates & times have not been sorted before
		List<ZonedDateTime> keyList = new ArrayList<>(weatherMap.keySet())
		Collections.sort(keyList)

		for (ZonedDateTime dateTime : keyList) {

			HashMap modelToWeatherMap = weatherMap.get(dateTime)

			String[] row = new String[2*modelCount+1]
			row[0] = dateTime.toString()

			int modelI = 0
			for (String modelId : modelIds) {
				PvModel model = pvModels.get(modelId)

				"build the needed data"
				WeatherMessage.WeatherData weather = modelToWeatherMap.get(modelId)
				PvModel.PvRelevantData neededData = new PvModel.PvRelevantData(dateTime,3600L, weather.diffIrr() as ComparableQuantity<Irradiance>, weather.dirIrr() as ComparableQuantity<Irradiance>)
				ComparableQuantity<Dimensionless> voltage = getQuantity(1.414213562, PU)

				"collect the results and calculate the difference between the provided results and the calculated ones"
				double calc = model.calculatePower(0L, voltage, neededData).p().getValue().doubleValue()
				double sol = resultsMap.get(dateTime).get(modelId).getValue().doubleValue()

				testRes.add(Math.abs(calc - sol))

				row[1 + modelI] = calc.toString()
				row[1 + modelCount + modelI] = sol.toString()

				modelI++
			}

		}

		then: "we expect the calculated result to be quasi equal the provided results data"
		testRes.every {
			(it < 0.00000000000001) // floating point operation
		}
	}
}

trait PvModelITHelper {
	private static final CSV_FORMAT = CSVFormat.DEFAULT.builder().setHeader().build()

	Iterable<CSVRecord> getCsvRecords(String fileName) {
		def resultsInputData = new File(this.getClass().getResource(fileName).file)
		def fileStream = new FileInputStream(resultsInputData)
		def gzipStream = new GZIPInputStream(fileStream)
		def decoder = new InputStreamReader(gzipStream, "UTF-8")
		def br = new BufferedReader(decoder)
		return CSV_FORMAT.parse(br)
	}

	HashMap<String, PvModel> getPvModels() {
		"load the grid input data from the corresponding resources folder"

		def csvGridSource = CsvGridSource.readGrid("it_grid", ";",
				this.getClass().getResource("_pv/it/grid_data").file,
				new FileNamingStrategy())

		def simulationStartDate = TimeUtil.withDefaults.toZonedDateTime("2011-01-01 00:00:00")
		def simulationEndDate = TimeUtil.withDefaults.toZonedDateTime("2012-01-01 00:00:00")

		HashMap<String, PvModel> pvModels = new HashMap<>()
		for (PvInput inputModel : csvGridSource.get().getSystemParticipants().getPvPlants()) {
			PvModel model = PvModel.apply(
					inputModel.getUuid(),
					inputModel.getId(),
					SystemComponent.determineOperationInterval(
					simulationStartDate,
					simulationEndDate,
					inputModel.getOperationTime()
					),
					1d,
					QControl.apply(inputModel.getqCharacteristics()),
					inputModel.getsRated(),
					inputModel.getCosPhiRated(),
					inputModel.getNode().getGeoPosition().getY(),
					inputModel.getNode().getGeoPosition().getX(),
					inputModel.getAlbedo(),
					inputModel.getEtaConv(),
					inputModel.getAzimuth(),
					inputModel.getElevationAngle(),
					getQuantity(1d, SQUARE_METRE)
					)

			pvModels.put(inputModel.getId(), model)
		}

		return pvModels
	}

	HashMap<ZonedDateTime, HashMap<String, WeatherMessage.WeatherData>> getWeatherData() {
		"read the weather data from the provided weather data file"
		final String fileName = "_pv/it/weather.tar.gz"
		final def csvRecords = getCsvRecords(fileName)

		HashMap<ZonedDateTime, HashMap<String, WeatherMessage.WeatherData>> weatherMap = new HashMap<>()
		for (row in csvRecords) {
			ZonedDateTime time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
			HashMap modelToWeatherMap
			if (weatherMap.containsKey(time)) {
				modelToWeatherMap = weatherMap.get(time)
			}

			if (modelToWeatherMap == null) {
				modelToWeatherMap = new HashMap<String, WeatherMessage.WeatherData>()
				weatherMap.put(time, modelToWeatherMap)
			}

			String modelId = row.get(1)

			double temp = 0
			double windVel = 0

			WeatherMessage.WeatherData weather = new WeatherMessage.WeatherData(
					(ComparableQuantity<Irradiance>) getQuantity(row.get(22)
					.replace("Wh/m²", "W/m²")
					.split("\u0000")[0]),
					(ComparableQuantity<Irradiance>) getQuantity(row.get(21)
					.replace("Wh/m²", "W/m²")),
					getQuantity(temp, KELVIN),
					getQuantity(windVel, METRE_PER_SECOND))

			modelToWeatherMap.put(modelId, weather)
		}

		return weatherMap
	}

	HashMap<ZonedDateTime, HashMap<String, Quantity<Power>>> getResultsData() {
		"read the results data from the provided file"
		final String fileName = "_pv/it/results2.tar.gz"
		def csvRecords = getCsvRecords(fileName)

		// we skip the first line and use hardcoded headers, because the first line is garbled
		String[] headers = [
			"Datetime",
			"pv_east_1",
			"pv_east_2",
			"pv_south_1",
			"pv_south_2",
			"pv_south_3",
			"pv_south_4",
			"pv_west_1",
			"pv_west_2"
		]

		HashMap<ZonedDateTime, HashMap<String, Quantity<Power>>> resultsMap = new HashMap<>()
		for(row in csvRecords) {
			// last line is trash
			if (row.get(0).startsWith('\u0000'))
				break

			ZonedDateTime time = ZonedDateTime.parse(row.get(0))
			HashMap<String, Quantity<Power>> modelToPowerMap = new HashMap<String, Quantity<Power>>()
			for (int i = 1; i < headers.length; i++) {
				String modelId = headers[i]
				String rawValue = row[i]
				Quantity<Power> power = getQuantity(Double.parseDouble(rawValue), MEGAWATT)
				modelToPowerMap.put(modelId, power)
			}
			resultsMap.put(time, modelToPowerMap)
		}
		return resultsMap
	}
}