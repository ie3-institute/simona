# Synthetic Data Generation for Data-Driven Methods

SIMONA can be used to generate physically consistent synthetic time series for distribution grids.

## Using synthetic data generation

Synthetic data generation requires a simulation time range, input data, output configuration and, if grid states should be
included, a power flow configuration.

The following snippets can be added to the SIMONA configuration file. Single parameter paths are equivalent to setting the
corresponding value inside a nested configuration block.

The simulation time range is configured with:

```
simona.time.startDateTime = "2011-01-01T00:00:00Z"
simona.time.endDateTime = "2011-01-01T02:00:00Z"
```

## Power flow

If node voltages, line currents or transformer loading should be part of the generated data, `simona.powerflow` has to be
configured. Otherwise, SIMONA skips the power flow calculation.

A basic power flow configuration can look as follows:

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

The power flow resolution defaults to `3600s`.

## Output

Grid outputs are disabled by default. Enable the required grid result types:

```
simona.output.grid.nodes = true
simona.output.grid.lines = true
simona.output.grid.transformers2w = true
```

If congestion results should be written as well, enable:

```
simona.output.grid.congestions = true
```

Participant results are disabled by default. To write them, enable participant simulation results:

```
simona.output.participant.defaultConfig.simulationResult = true
```

The output directory is configured with:

```
simona.output.base.dir = "output/synthetic_data"
```

A timestamp is added to the output directory by default. To disable this, set:

```
simona.output.base.addTimestampToOutputDir = false
```

## Typical application

This functionality can be used to create reproducible data sets for testing, validation, benchmarking and data-driven
methods.