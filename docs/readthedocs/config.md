(config)=

# Configuration 

To configure a SIMONA simulation, several parameters must be specified. Each simulation is performed within a reference
system for a given runtime and is based on a power flow calculation using the Newton-Raphson algorithm. Individual steps
for configuring the simulation are performed below.

An overview of all default values can be found at the bottom of the page. These values are used when no other value is set in the configuration
file.

## General simulation parameters
To create the output directory name, the name of the simulation is used as a string variable 

  `simona.simulationName = "vn_simona"`

## Time parameters
Starting date and time of the simulation in ISO-8601 date and time format with offset

  `simona.time.startDateTime = "2011-01-01T00:00:00Z"`

Ending date and time of the simulation in ISO-8601 date and time format with offset

  `simona.time.endDateTime = "2011-01-01T02:00:00Z"`

The preset ReadyCheckWindow should be maintained 

  `simona.time.schedulerReadyCheckWindow = 900`

If the simulation is to be ended in case of a failed power flow, set: 

  `simona.time.stopOnFailedPowerFlow = true`

If the simulation is to skip a failed power flow and continue to run, set:

  `simona.time.stopOnFailedPowerFlow = false`

## Input parameters
Setting of the data source

  `simona.input.grid.datasource.id = "csv"`

Specify the folder path containing the csv data of the grid components and the csv separator (e.g. "," or ";").
The directory structure is determined by the boolean `isHierarchic`. 
If files are placed within [a specific set of subdirectories](https://powersystemdatamodel.readthedocs.io/en/latest/io/csvfiles.html#default-directory-hierarchy), `isHierarchic: true` needs to be set.

```
simona.input.primary.csvParams = {  
  directoryPath: "input/samples/vn_simona/fullGrid"  
  csvSep: ","  
  isHierarchic: false  
}
```

### Weather data

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
  - Other supported weather data sources are: influxdb1x, csv, sql, couchbase
  - The parameter `maxCoordinateDistance` is used to specify the radius in which weather data should be searched in. The given distance should be in meter.

Further model classes which can be used to parse a data set as input to power system simulations are described in [PSDM](https://powersystemdatamodel.readthedocs.io/en/latest/models/models.html#time-series). 
Data sources and data sinks are explained in the [I/O-capabilities](https://powersystemdatamodel.readthedocs.io/en/latest/io/basiciousage.html) section of the PSDM. 

### Price data

For the buying price, fees (in EUR/MWh) are added to the wholesale price and the tax is added on the total cost.
Conversely, for the selling price, fees (in EUR/MWh) are subtracted from the wholesale price, and the tax is subtracted from the remaining amount.

Find below an exemplary configuration for households in Dortmund, 2025.
An explanation of the exemplary price configuration below can be found [here](models/price_service.md#example-scenario).
The UUID of the price time series to use as data source. 
An individual time series with given UUID and column scheme ENERGY_PRICE needs to be provided.

```
simona.input.prices.datasource = {
  buyingPrice = {
    fees: 187.11
    tax: 0.19
  }
  sellingPrice = {
    fees: 5.0
    tax: 0.0
  }
  timeseriesUuid = <UUID>
  csvParams = {
    directoryPath: "input/samples/vn_simona/fullGrid"
    csvSep: ","
    isHierarchic: false
  }
}
```

## Output parameters

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
  compressOutputs = false
}
```

While using a csv sink, the raw data output files can be zipped directly when `compressOutputs = true` is used.


### Output configuration of the grid

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

### Output configuration of system participants

To use the default configuration the default notifier has to be used. By setting "simulationResult" to true, the participant is enabled to return its results. 

```
simona.output.participant.defaultConfig = {
  notifier = "default"
  powerRequestReply = false
  simulationResult = true
  flexResult = false
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
    flexResult = false
  },
  {
    notifier = "wec"
    powerRequestReply = false
    simulationResult = true
    flexResult = false
  },
  {
    notifier = "evcs"
    powerRequestReply = false
    simulationResult = true
    flexResult = false
  }
]
```

### Output configuration of thermal elements

To use the default configuration the default notifier has to be used. By setting "simulationResult" to true, the thermal elements is enabled to return its results.

```
simona.output.thermal.defaultConfig = {
  notifier = "default",
  simulationResult = true
  flexResult = false
}
```

The default configuration applies to all models except the ones with individual configurations assigned.
If individual configurations have to be performed for certain thermal elements, these must be listed with the corresponding notifier as in the following example.

```
simona.output.thermal.individualConfigs = [
  {
    notifier = "house",
    simulationResult = true
    flexResult = false
  },
  {
    notifier = "cylindricalstorage",
    simulationResult = true
    flexResult = false
  }
]
```

Further model classes which can be used to load the outcome of a system simulation are described in [PSDM](https://powersystemdatamodel.readthedocs.io/en/latest/models/models.html#result).
Data sources and data sinks are explained in the [I/O-capabilities](https://powersystemdatamodel.readthedocs.io/en/latest/io/basiciousage.html) section of the PSDM.

### Output configuration for flexibility options

The output of flexibility options either globally or for system participant groups or individual participants can also be applied. By setting "flexResult" to true, the participant is enabled to return the flexibility options results.

```
simona.output.participant.defaultConfig = {
  notifier = "default"
  powerRequestReply = false
  simulationResult = true
  flexResult = true
}
```

### Logging level configuration

To specify which log statements should be logged in the `simona.log` file and which log statements should be printed to
the console, SIMONA offers the user two configuration options.

The first options sets the default log level for the log file as well as the console.

```
simona.output.log.level = "INFO"
```

If the console should use a different log level, this level must be selected in the following example.

```
simona.output.log.consoleLevel = "DEBUG"
```


## Runtime configuration 

Specification of the runtime of subgrids and voltage levels:

  `simona.runtime.selected_subgrids = []`

  `simona.runtime.selected_volt_lvls = []`

The participant runtime can be either based on default configuration or individual configurations can be assigned. For the
individual configuration one need to add a column with the parameter name to the corresponding input files and fill in the
values for those input that differ from the default configuration. As for the other models, no input needs to be provided.

```
simona.runtime.participant.load = {
    calculateMissingReactivePowerWithModel = false
    scaling = 1.0
    modelBehaviour = "fix"
    reference = "power"
}
```

An example for individual configuration of loads:

```
uuid,cos_phi_rated,e_cons_annual,id,load_profile,node,operates_from,operates_until,operator,q_characteristics,s_rated,controlling_em,modelBehaviour,reference
8221a6b1-eff3-48fe-88ab-0685a9f59cce,0.9700000286102295,4000.0,NS_NET116_L_S1_2(8),h0,3e21f3a1-2c9c-4138-bcc9-466b004609ed,,,,"cosPhiFixed:{(0.0,1.0)}",4.1237101554870605,,,
5d4b96bf-a6ad-4026-b97a-4e6d77896480,0.9700000286102295,4000.0,NS_NET116_L_S2_2(4),h0,d53ff076-dadd-44f8-85d4-68f48991f7d0,,,,"cosPhiFixed:{(0.0,1.0)}",4.1237101554870605,,,
a964d9b8-a035-41df-86c0-4c5306af2158,0.9700000286102295,4000.0,NS_NET116_L_S2_4(7),h0,e05c68b1-11cd-43fd-a4b2-31e4db380c78,,,,"cosPhiFixed:{(0.0,1.0)}",4.1237101554870605,,,
50c89980-8da2-4e98-8602-e2f0b560e7c4,0.949999988079071,4000.0,NS_NET146_L_F1_(8),h0,d5489e1b-0e7e-4ca9-a362-09c23576a622,,,,"cosPhiFixed:{(0.0,1.0)}",2.3157899379730225,,fix,energy
```



The reactive power is determined based on the chosen Q-control of the participant, when: 

    calculateMissingReactivePowerWithModel = true

Using the default configuration, the universally unique identifier should be empty. 
Alternatively, the parameter can be omitted to use the default empty list.

    uuids = []

Choosing the scaling factor of relevant participant parameters such as rated power or annual power consumption: 

    scaling = 1.0

The model behaviour specifies which type of load model is used. The load model can equal a fixed set point ("fix") or a predefined profile ("profile"). When using primary data for a load model, the model behaviour will be ignored. 

    modelBehaviour = "fix"

The load reference can scale the load model behaviour to reach the given annual energy consumption ("energyConsumption") or the foreseen active power ("power"). When using primary data for a load model, the load reference will be ignored.

    reference = "power"

If an individual configuration is to be assigned, the default configuration parameters must be adjusted accordingly.
Runtime configurations of other system participants are done similarly, except that model behavior and reference are not defined.

### Storage runtime configuration

The storage model takes parameters for the initial state of charge (SOC) and the target SOC for electrical energy storages, with 0.0 <= SOC <= 1.0.
The initial SOC defaults to 0%, while the target SOC is optional. When no target SOC is set, the reference behavior (see flexibility messages) of storages is 0 kW. 

    initialSoc = "0.0"
    targetSoc = "1.0"

Individual configuration can be assigned accordingly.

## Grid configuration 

### Reference System
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

### Voltage limits

The voltage limits contains a list of voltage levels. Each element includes the minimal and maximal allowed voltage and
the separate configuration of each voltage level. The voltage level configuration is composed of the identifier and the
nominal voltage. 

The configuration of a voltage limits is optional. If no configuration is provided by the user, the default
[voltage limits](models/voltage_limits) that includes all common german voltage levels is used. For those users
who need other voltage levels than the common german voltage levels or different voltage limits, they can configure
their limits as shown below.

The voltage limits can be configured as follows:

```
simona.gridConfig.voltageLimits = [
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Lv", vNom = "0.4 kV"}]},
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Mv", vNom = "20 kV"}]},
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Hv", vNom = "110 kV"}]},
  {vMin = 0.9, vMax = 1.05, voltLvls = [{id = "EHV", vNom = "380 kV"}]},
]
```

Further typical voltage levels which can be used in the simulation and the configuration of individual voltage limits
are described in the documentation of [voltage limits](models/voltage_limits).


## Power flow configuration 

The powerflow configuration in SIMONA is an optional configuration. If no configuration is present, SIMONA will skip all
powerflow calculations. This can be useful, if you only want to simulate system participants and/or energy management systems.

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


## Default configuration values

### Time
```
simona.time.schedulerReadyCheckWindow = None
```

### Input

```
simona.input = {
    baseInputDir = ./input
    extSimDir = None
    
    loadProfile = {
        csvParams = None
        sqlParams = None
    }
    
    primary = {
      couchbaseParams = None
      csvParams = None
      influxDb1xParams = None
      sqlParams = None
    }
    
    weather.datasource = {
        coordinateSource = {
          csvParams = None
          gridModel = "icon"
          sampleParams = None
          sqlParams = None
        }
        couchbaseParams = None
        csvParams = None
        influxDb1xParams = None
        maxCoordinateDistance = 50000
        resolution = 3600
        sampleParams = None
        scheme = "icon"
        sqlParams = None
        timestampPattern = None
    }

    prices.datasource = {
        buyingPrice = {
            fees: 0
            tax: 0
        }
        sellingPrice = {
            fees: 0
            tax: 0
        }
        csvParams = None
    }
}
```

### Output

```
simona.output = {
    base.addTimestampToOutputDir = true

    grid = {
      congestions = false
      lines = false
      nodes = false
      switches = false
      transformers2w = false
      transformers3w = false
    }
    
    sink = {
      csv = None
      influxDb1x = None
      kafka = None
    }    
    
    log = {
        level = "INFO"
        consoleLevel = None
    }
    
    participant = {
        defaultConfig = {
            notifier = "default"
            simulationResult = false
            flexResult = false
            powerRequestReply = false
        }
        indvidualConfigs = [] 
    }
    
    thermal = {
        defaultConfig = {
            notifier = "default"
            simulationResult = false
        }
        indvidualConfigs = []
    }
}
```

### Runtime

```
simona.runtime = {
    selectedSubgrids = []
    selectedVoltLvls = []

    listener = {
      eventsToProcess = None
      kafka = None
    }
    
    em = {
        calculateMissingReactivePowerWithModel = false
        scaling = 1.0
        aggregateFlex = "SELF_OPT_EXCL_REG"
        curtailRegenerative = false
    }
    
    participant = {
        requestVoltageDeviationThreshold = 1e-14
        bm = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
        }
        evcs = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
            chargingStrategy = "maxPower"
            lowestEvSoc = 0.2
        }
        fixedFeedIn = {
             calculateMissingReactivePowerWithModel = false
             scaling = 1.0
        }
        hp = {
             calculateMissingReactivePowerWithModel = false
             scaling = 1.0
        }
        load = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
            modelBehaviour = "fix"
            reference = "power"
        }
        pv = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
        }
        storage = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
            initialSoc = 0.0
            targetSoc = None
        }
        wec = {
            calculateMissingReactivePowerWithModel = false
            scaling = 1.0
        }
    }
}
```

### PowerFlow

```
simona.powerflow = {
    maxSweepPowerDeviation = 1e-5
    newtonraphson.iterations = 50
    resolution = 3600s
    stopOnFailure = false
}
```

### Congestion management

```
simona.congestionManagement = {
    enableDetection = false
    enableTransformerTapChange = false
}
```

### Control

```
simona.control = {
    transformer = []
}
```
