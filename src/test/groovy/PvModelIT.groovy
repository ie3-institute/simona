/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */



import edu.ie3.datamodel.io.source.csv.CsvJointGridContainerSource
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.model.participant.PvModel
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.Sq
import edu.ie3.util.scala.quantities.WattsPerSquareMeter$
import org.apache.commons.csv.CSVFormat
import org.apache.commons.csv.CSVRecord
import spock.lang.Shared
import spock.lang.Specification
import squants.Dimensionless
import squants.Each$
import squants.energy.Megawatts$
import squants.energy.Power
import squants.motion.MetersPerSecond$
import squants.thermal.Kelvin$

import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.zip.GZIPInputStream

import static java.util.Locale.US
import static java.util.Locale.setDefault

/**
 * A simple integration test that uses pre-calculated data to check if the pv model works as expected.
 * It uses 8 pv models located in GER.
 *
 */
class PvModelIT extends Specification implements PvModelITHelperDummy {

    @Shared
    Map<String, PvModel> pvModels

    @Shared
    Map<ZonedDateTime, Map<String, WeatherMessage.WeatherData>> weatherMap

    @Shared
    Map<ZonedDateTime, Map<String, Power>> resultsMap


    def setupSpec() {
        // input weather data values are in us format
        // if locale is not set hard coded to US, quantity parsing will return invalid values
        setDefault(US)

        pvModels = createPvModels()
        weatherMap = weatherData
        resultsMap = resultsData
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

            Map<String, WeatherMessage.WeatherData> modelToWeatherMap = weatherMap.get(dateTime)

            String[] row = new String[2*modelCount+1]
            row[0] = dateTime.toString()

            int modelI = 0
            for (String modelId : modelIds) {
                PvModel model = pvModels.get(modelId)

                "build the needed data"
                WeatherMessage.WeatherData weather = modelToWeatherMap.get(modelId)
                PvModel.PvRelevantData neededData = new PvModel.PvRelevantData(
                        dateTime,
                        3600L,
                        weather.diffIrr(),
                        weather.dirIrr()
                )
                Dimensionless voltage = Sq.create(1.414213562d, Each$.MODULE$)

                "collect the results and calculate the difference between the provided results and the calculated ones"
                double calc = model.calculatePower(0L, voltage, ModelState.ConstantState$.MODULE$,  neededData).p().toMegawatts()
                double sol = resultsMap.get(dateTime).get(modelId).toMegawatts()

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

trait PvModelITHelperDummy {
    private static final CSV_FORMAT = CSVFormat.DEFAULT.builder().setHeader().build()

    Iterable<CSVRecord> getCsvRecords(String fileName) {
        def resultsInputData = new File(this.getClass().getResource(fileName).file)
        def fileStream = new FileInputStream(resultsInputData)
        def gzipStream = new GZIPInputStream(fileStream)
        def decoder = new InputStreamReader(gzipStream, "UTF-8")
        def br = new BufferedReader(decoder)
        return CSV_FORMAT.parse(br)
    }

    Map<String, PvModel> createPvModels() {
        "load the grid input data from the corresponding resources folder"

        def csvGridSource = CsvJointGridContainerSource.read("it_grid", ";",
                Path.of(this.getClass().getResource("_pv/it/grid_data").toURI()), false)

        def simulationStartDate = TimeUtil.withDefaults.toZonedDateTime("2011-01-01T00:00:00Z")
        def simulationEndDate = TimeUtil.withDefaults.toZonedDateTime("2012-01-01T00:00:00Z")

        Map<String, PvModel> pvModels = new HashMap<>()
        for (PvInput inputModel : csvGridSource.systemParticipants.pvPlants) {
            PvModel model = PvModel.apply(
                    inputModel,
                    1d,
                    simulationStartDate,
                    simulationEndDate
            )

            pvModels.put(inputModel.getId(), model)
        }

        return pvModels
    }

    Map<ZonedDateTime, Map<String, WeatherMessage.WeatherData>> getWeatherData() {
        "read the weather data from the provided weather data file"
        final String fileName = "_pv/it/weather.tar.gz"
        final def csvRecords = getCsvRecords(fileName)

        Map<ZonedDateTime, Map<String, WeatherMessage.WeatherData>> weatherMap = new HashMap<>()
        for (row in csvRecords) {
            ZonedDateTime time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
            Map<String, WeatherMessage.WeatherData> modelToWeatherMap
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
                    Sq.create(row.get(22).replace("Wh/m²", "").toDouble(), WattsPerSquareMeter$.MODULE$),
                    Sq.create(row.get(21).replace("Wh/m²", "").toDouble(), WattsPerSquareMeter$.MODULE$),
                    Sq.create(temp, Kelvin$.MODULE$),
                    Sq.create(windVel, MetersPerSecond$.MODULE$))

            modelToWeatherMap.put(modelId, weather)
        }

        return weatherMap
    }

    Map<ZonedDateTime, Map<String, Power>> getResultsData() {
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

        Map<ZonedDateTime, Map<String, Power>> resultsMap = new HashMap<>()
        for(row in csvRecords) {
            // last line is trash
            if (row.get(0).startsWith('\u0000'))
                break

            ZonedDateTime time = TimeUtil.withDefaults.toZonedDateTime(row.get(0))
            Map<String, Power> modelToPowerMap = new HashMap<>()
            for (int i = 1; i < headers.length; i++) {
                String modelId = headers[i]
                String rawValue = row[i]
                Power power = Sq.create(Double.parseDouble(rawValue), Megawatts$.MODULE$)
                modelToPowerMap.put(modelId, power)
            }
            resultsMap.put(time, modelToPowerMap)
        }
        return resultsMap
    }
}