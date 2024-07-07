(config)=

# Configuration 

To configure a SIMONA simulation, several parameters must be specified. Each simulation is performed within a reference system for a given runtime and is based on a power flow calculation using the Newton-Raphson algorithm. Individual steps for configuring the simulation are performed below.

## Configuration Parameters 

### General simulation parameters
To create the output directory name, the name of the simulation is used as a string variable 

  `simona.simulationName = "vn_simona"`

### Time parameters
Starting date and time of the simulation
  - Format: "YYYY-MM-DD HH:MM:SS"

  `simona.time.startDateTime = "2011-01-01 00:00:00"`

Ending date and time of the simulation
  - Format: "YYYY-MM-DD HH:MM:SS"

  `simona.time.endDateTime = "2011-01-01 02:00:00"`

The preset ReadyCheckWindow should be maintained 

  `simona.time.schedulerReadyCheckWindow = 900`

If the simulation is to be ended in case of a failed power flow, set: 

  `simona.time.stopOnFailedPowerFlow = true`

If the simulation is to skip a failed power flow and continue to run, set:

  `simona.time.stopOnFailedPowerFlow = false`

### Input parameters
Setting of the data source

  `simona.input.grid.datasource.id = "csv"`

Specify the folder path containing the csv data of the grid components and the csv separator (e.g. "," or ";")

```
simona.input.primary.csvParams = {  
  directoryPath: "input/samples/vn_simona/fullGrid"  
  csvSep: ","  
  isHierarchic: false  
}
```

Insert weather data source via:
```
simona.input.weather.datasource = {
  scheme = "icon"
  sampleParams.use = true
  coordinateSource.sampleParams.use = true
  maxCoordinateDistance = 50000
}
```

  - Schemes of weather data
    - [icon](https://www.dwd.de/EN/research/weatherforecasting/num_modelling/01_num_weather_prediction_modells/icon_description.html)
    - [cosmo](https://www.dwd.de/EN/research/weatherforecasting/num_modelling/01_num_weather_prediction_modells/regional_model_cosmo_de.html;jsessionid=57BC71602B46DDF067666F57C1889B5E.live11042?nn=484268)
  - Use of sample values
  
    `sampleParams.use = true`

    `coordinateSource.sampleParams.use = true`
  
    - The sample values should only be used to test the functionality. The performance of a reasonable simulation with sensitive results should be based on real weather data.
    - Supported weather data sources are: influxdb1x, csv, sql, couchbase, sample
  - The parameter `maxCoordinateDistance` is used to specify the radius in which weather data should be searched in. The given distance should be in meter.

Further model classes which can be used to parse a data set as input to power system simulations are described in [PSDM](https://powersystemdatamodel.readthedocs.io/en/latest/models/models.html#time-series). 
Data sources and data sinks are explained in the [I/O-capabilities](https://powersystemdatamodel.readthedocs.io/en/latest/io/basiciousage.html) section of the PSDM. 

### Output parameters

Specify the output directory

  `simona.output.base.dir = "output/vn_simona"`

A timestamp can be added to the output data through: 

  `simona.output.base.addTimestampToOutputDir = true` 

Specification of the result type:

  In order for the output to be processed as a file, the data sink must be specified accordingly by "sink.datatype" between prefix and suffix.

```
simona.output.sink.csv {
  fileFormat = ".csv"
  filePrefix = ""
  fileSuffix = ""
}
```

#### Output configuration of the grid

The grid output configuration defines for which grid components simulation values are to be output.

```
simona.output.grid = {
  notifier = "grid"
  nodes = false
  lines = false
  switches = false
  transformers2w = false
  transformers3w = false
}
```

#### Output configuration of system participants

To use the default configuration the default notifier has to be used. By setting "simulationResult" to true, the participant is enabled to return its results. 

```
simona.output.participant.defaultConfig = {
  notifier = "default"
  powerRequestReply = false
  simulationResult = true
}
```

The default configuration applies to all models except the ones with individual configurations assigned.
If individual configurations have to be performed for certain system participants, these must be listed with the corresponding notifier as in the following example.

```
simona.output.participant.individualConfigs = [
  {
    notifier = "pv"
    powerRequestReply = false
    simulationResult = true
  },
  {
    notifier = "wec"
    powerRequestReply = false
    simulationResult = true
  },
  {
    notifier = "evcs"
    powerRequestReply = false
    simulationResult = true
  }
]
```

#### Output configuration of thermal elements

To use the default configuration the default notifier has to be used. By setting "simulationResult" to true, the thermal elements is enabled to return its results.

```
simona.output.thermal.defaultConfig = {
  notifier = "default",
  simulationResult = true
}
```

The default configuration applies to all models except the ones with individual configurations assigned.
If individual configurations have to be performed for certain thermal elements, these must be listed with the corresponding notifier as in the following example.

```
simona.output.thermal.individualConfigs = [
  {
    notifier = "house",
    simulationResult = true
  },
  {
    notifier = "cylindricalstorage",
    simulationResult = true
  }
]
```

Further model classes which can be used to load the outcome of a system simulation are described in [PSDM](https://powersystemdatamodel.readthedocs.io/en/latest/models/models.html#result).
Data sources and data sinks are explained in the [I/O-capabilities](https://powersystemdatamodel.readthedocs.io/en/latest/io/basiciousage.html) section of the PSDM.

## Runtime configuration 

Specification of the runtime of subgrids and voltage levels:

  `simona.runtime.selected_subgrids = []`

  `simona.runtime.selected_volt_lvls = []`

The participant runtime can be either based on default configuration or individual configurations can be assigned. 

```
simona.runtime.participant.load = {
  defaultConfig = {
    calculateMissingReactivePowerWithModel = false
    uuids = ["default"]
    scaling = 1.0
    modelBehaviour = "fix"
    reference = "power"
  }
  individualConfigs = []
}
```

The reactive power is determined based on the chosen Q-control of the participant, when: 

    calculateMissingReactivePowerWithModel = true

Using the default configuration the universally unique identifier can be set to "default". 

    uuids = ["default"]

Choosing the scaling factor of relevant participant parameters such as rated power or annual power consumption: 

    scaling = 1.0

The model behaviour specifies which type of load model is used. The load model can equal a fixed set point ("fix"), a predefined profile ("profile") or a randomly created profile ("random"). 

    modelBehaviour = "fix"

The load reference can scale the load model behaviour to reach the given annual energy consumption ("energyConsumption") or the foreseen active power ("power"). 

    reference = "power"

If an individual configuration is to be assigned, the default configuration parameters must be adjusted accordingly.
Runtime configurations of other system participants are done similarly, except that model behavior and reference are not defined.

### Storage runtime configuration

The storage model takes parameters for the initial state of charge (SOC) and the target SOC for electrical energy storages, with 0.0 <= SOC <= 1.0.
The initial SOC defaults to 0%, while the target SOC is optional. When no target SOC is set, the reference behavior (see flexibility messages) of storages is 0 kW. 

    initialSoc = "0.0"
    targetSoc = "1.0"

Individual configuration can be assigned accordingly.

## Event configuration 

Tba:

  `simona.event.listener = []`

## Grid configuration 

The reference system contains a list of voltage levels. Each element includes the nominal apparent power, the nominal 
voltage and the separate configuration of each voltage level. The voltage level configuration is composed of the identifier 
and the nominal voltage.

The configuration of a reference system is optional. If no configuration is provided by the user, the default
[reference system](models/reference_system) that includes all common german voltage levels is used. For those users 
who need other voltage levels than the common german voltage levels or different nominal apparent powers, they can configure
their reference systems as shown below.

The reference system can be configured as follows: 

```
simona.gridConfig.refSystems = [
  {sNom = "100 kVA", vNom = "0.4 kV", voltLvls = [{id = "LV", vNom = "0.4 kV"}]},
  {sNom = "60 MVA", vNom = "20 kV", voltLvls = [{id = "MV", vNom = "20 kV"}]},
  {sNom = "600 MVA", vNom = "110 kV", voltLvls = [{id = "HV", vNom = "110 kV"}]},
  {sNom = "1000 MVA", vNom = "380 kV", voltLvls = [{id = "EHV", vNom = "380 kV"}]}
]
```

Further typical voltage levels which can be used in the simulation and the configuration of individual reference systems
are described in the documentation of [reference system](models/reference_system).

## Power flow configuration 

Maximum allowed deviation in power between two sweeps, before overall convergence is assumed:

  `simona.powerflow.maxSweepPowerDeviation = 1E-5 `

Maximum deviation as convergence criterion for the power flow calculation: 

  `simona.powerflow.newtonraphson.epsilon = [1E-12]`

Secondary convergence criterion for the power flow calculation is the number of iterations:

  `simona.powerflow.newtonraphson.iterations = 50`

Resolution of the power flow calculation: 

  `simona.powerflow.resolution = "3600s"`

## Transformer Control Group configuration

It's possible to add a voltage control function to a transformer or group of transformers. This requires measurements within the network to be under voltage control and at least one corresponding transformer.
The voltage control will attempt to adjust the voltage by changing the tap position of the corresponding transformer. If changing the tap position would cause a voltage limit to be exceeded, the initial voltage deviation cannot be reduced by the voltage control system.

Transformer control groups must contain at least one transformer and one measurement. And can be configured as shown in this example for two transformer control groups:
```
simona.control.transformer = [
{
transformers = ["31a2b9bf-e785-4475-aa44-1c34646e8c79"],
measurements = ["923f2d69-3093-4198-86e4-13d2d1c220f8"],
vMin = 0.98,
vMax = 1.02
}
, {
transformers = ["1132dbf4-e8a1-44ae-8415-f42d4497aa1d"],
measurements = ["7686b818-a0ba-465c-8e4e-f7d3c4e171fc"],
vMin = 0.98,
vMax = 1.02
}
]
```

UUID of transformer in control group:

`transformers = ["31a2b9bf-e785-4475-aa44-1c34646e8c79"]`

UUID of measurement in control group:

`measurements = ["923f2d69-3093-4198-86e4-13d2d1c220f8"]`

Minimum Voltage Limit in p.u.:

`vMin = 0.98`

Maximum Voltage Limit in p.u.:

`vMax = 1.02`
