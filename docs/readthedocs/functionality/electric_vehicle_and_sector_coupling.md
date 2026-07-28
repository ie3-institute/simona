# Electric Vehicle and Sector Coupling Analysis

SIMONA can be used to analyse the impact of electric vehicle charging stations and sector coupling technologies such as
heat pumps on distribution grids.

## Using electric vehicle charging stations

The runtime configuration of electric vehicle charging stations is located under:

```
simona.runtime.participant.evcs
```

The following snippets can be added to the SIMONA configuration file. Single parameter paths are equivalent to setting the
corresponding value inside a nested configuration block.

The charging strategy can be configured with:

```
simona.runtime.participant.evcs.chargingStrategy = "maxPower"
```

The charging strategy defaults to `"maxPower"`.

The lowest EV state of charge can be configured with:

```
simona.runtime.participant.evcs.lowestEvSoc = 0.2
```

The default value is `0.2`.

The scaling factor can be configured with:

```
simona.runtime.participant.evcs.scaling = 1.0
```

The default value is `1.0`.

## Using heat pumps

Heat pump runtime settings are located under:

```
simona.runtime.participant.hp
```

The scaling factor can be configured with:

```
simona.runtime.participant.hp.scaling = 1.0
```

The default value is `1.0`.

## Output

Participant results are disabled by default. To write participant time series, enable:

```
simona.output.participant.defaultConfig.simulationResult = true
```

If flexibility results should be written as well, enable:

```
simona.output.participant.defaultConfig.flexResult = true
```

To analyse the grid impact, enable grid output:

```
simona.output.grid.nodes = true
simona.output.grid.lines = true
simona.output.grid.transformers2w = true
```

A power flow configuration is required for node voltages, line currents and transformer loading.

```
simona.powerflow = {
  maxSweepPowerDeviation = 1e-5
  newtonraphson = {
    epsilon = [1E-12]
    iterations = 50
  }
  resolution = 3600s
  stopOnFailure = false
}
```

## Typical application

This functionality can be used to analyse voltage impacts, line loading and transformer loading caused by electric
vehicles, heat pumps and other flexible loads.