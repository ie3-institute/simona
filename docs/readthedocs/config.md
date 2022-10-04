(config)=

# Configuration 

## Configuration Parameters 

### General simulation parameters
- To create the output directory name, the name of the simulation is used as a string variable 

  `simona.simulationName = "vn_simona"`

### Time parameters
- Starting date and time of the simulation
  - Format: "YYYY-MM-DD HH:MM:SS"

  `simona.time.startDateTime = "2011-01-01 00:00:00"`

- Ending date and time of the simulation
  - Format: "YYYY-MM-DD HH:MM:SS"

  `simona.time.endDateTime = "2011-01-01 02:00:00"`

- The preset ReadyCheckWindow should be maintained 

  `simona.time.schedulerReadyCheckWindow = 900`

- If the simulation is to be ended in case of a failed power flow, set: 

  `simona.time.stopOnFailedPowerFlow = true`

- If the simulation is to skip a failed power flow and continue to run, set:

  `simona.time.stopOnFailedPowerFlow = false`

### Input parameters
- Setting of the data source

  `simona.input.grid.datasource.id = "csv"`

- Specify the folder path containing the csv data of the grid components and the csv separator (e.g. "," or ";")

  `simona.input.primary.csvParams = {`
  
  `directoryPath: "input/samples/vn_simona/fullGrid"`
  
  `csvSep: ","`
  
  `isHierarchic: false`
  
  `}`

- Insert weather data source via:

  `simona.input.weather.datasource = {`

  `scheme = "icon"`

  `sampleParams.use = true`

  `coordinateSource.sampleParams.use = true`

  `}`

  - Schemes of weather data
    - icon
    - cosmo 
  - Use of sample values
  
    `sampleParams.use = true`

    `coordinateSource.sampleParams.use = true`
  
    - The sample values should only be used to test the functionality. The performance of a reasonable simulation with sensitive results should be based on real weather data.
    - Supported weather data sources are: influxdb1x, csv, sql, couchbase, sample

### Output parameters

- Specify the output directory

  `simona.output.base.dir = "output/vn_simona"`

- A timestamp can be added to the output data through: 

  `simona.output.base.addTimestampToOutputDir = true` 

- Specification of the result type:

  In order for the output to be processed as a file, the data sink must be specified accordingly by "sink.datatype" between prefix and suffix.

  `simona.output.sink.csv {`

  `fileFormat = ".csv"`

  `filePrefix = ""`

  `fileSuffix = ""`

  `}`

#### Output configuration of the grid

- The grid output configuration defines for which grid components simulation values are to be output.

  `simona.output.grid = {`

  `notifier = "grid"`

  `nodes = false`

  `lines = false`

  `switches = false`

  `transformers2w = false`

  `transformers3w = false`

  `}`

#### Output configuration of system participants

- To use the default configuration the default notifier has to be used. By setting "simulationResult" to true, the participant is enabled to return its results. 

  `simona.output.participant.defaultConfig = {`

  `notifier = "default"`

  `powerRequestReply = false`

  `simulationResult = true`

  `}`

- The default configuration applies to all models except the ones with individual configurations assigned.
If individual configurations have to be performed for certain system participants, these must be listed with the corresponding notifier as in the following example.

  `simona.output.participant.individualConfigs = [`

  `{`

  `notifier = "pv"`

  `powerRequestReply = false`

  `simulationResult = true`

  `},`

  `{`

  `notifier = "evcs"`

  `powerRequestReply = false`

  `simulationResult = true`

  `}`

  `]`

## Runtime configuration 

- Specification of the runtime of subgrids and voltage levels:

  `simona.runtime.selected_subgrids = []`

  `simona.runtime.selected_volt_lvls = []`

- The participant runtime can be either based on default configuration or individual configurations can be assigned. 

  `simona.runtime.participant.load = {`

  `defaultConfig = {`

  `calculateMissingReactivePowerWithModel = false`

  `uuids = ["default"]`

  `scaling = 1.0`

  `modelBehaviour = "fix"`

  `reference = "power"`

  `}`

  `individualConfigs = []`

  `}`

  - The reactive power is determined based on the chosen Q-control of the participant, when: 

    `calculateMissingReactivePowerWithModel = true`

  - Using the default configuration the universally unique identifier can be set to "default". 

    `uuids = ["default"]`

  - Choosing the scaling factor of the power output: 

    `scaling = 1.0`

  - The model behaviour specifies which type of load model is used. The load model can equal a fixed set point ("fix"), a predefined profile ("profile") or a randomly created profile ("random"). 

    `modelBehaviour = "fix"`

  - The load reference can scale the load model behaviour to reach the given annual energy consumption ("energyConsumption") or the foreseen active power ("power"). 

    `reference = "power"`

- If an individual configuration is to be assigned, the default configuration parameters must be adjusted accordingly.
Runtime configurations of other system participants are done similarly, except that model behavior and reference are not defined.

## Event configuration 

- Tba:

  `simona.event.listener = []`

## Grid configuration 

- The reference system can be configured as follows: 

  `simona.gridConfig.refSystems = [`

  `{sNom = "100 kVA", vNom = "0.4 kV", voltLvls = [{id = "NS", vNom = "0.4 kV"}]},`

  `{sNom = "60 MVA", vNom = "20 kV", voltLvls = [{id = "MS", vNom = "20 kV"}]},`

  `]`

- The reference system contains a list of voltage levels. Each element includes the nominal apparent power, the nominal voltage and the separate configuration of each voltage level. The voltage level configuration is composed of the identifier and the nominal voltage.

- Further typical voltage levels which can be used in the simulation are presented in docs/readthedocs/models/voltage_levels.md.

## Power flow configuration 

- Maximum allowed deviation in power between two sweeps, before overall convergence is assumed:

  `simona.powerflow.maxSweepPowerDeviation = 1E-5 `

- Maximum deviation as convergence criterion for the power flow calculation: 

  `simona.powerflow.newtonraphson.epsilon = [1E-12]`

- Secondary convergence criterion for the power flow calculation is the number of iterations:

  `simona.powerflow.newtonraphson.iterations = 50`

- Resolution of the power flow calculation: 

  `simona.powerflow.resolution = "3600s"`
