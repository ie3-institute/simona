# Integration of Renewable Energy Sources

SIMONA can be used to analyse the grid impact of renewable generation such as photovoltaic systems and wind energy
converters.

## Using renewable generation

Renewable generators are simulated as system participants. For weather-dependent models, weather data has to be configured
under:

```
simona.input.weather.datasource
```

The following snippets can be added to the SIMONA configuration file. Single parameter paths are equivalent to setting the
corresponding value inside a nested configuration block.

For testing purposes, sample weather data can be enabled with:

```
simona.input.weather.datasource.sampleParams.use = true
simona.input.weather.datasource.coordinateSource.sampleParams.use = true
```

The weather scheme defaults to `"icon"`:

```
simona.input.weather.datasource.scheme = "icon"
```

The maximum coordinate distance defaults to `50000` meters:

```
simona.input.weather.datasource.maxCoordinateDistance = 50000
```

The sample values should only be used to test the functionality. Meaningful studies should be based on real weather data.

## Output

Participant results are disabled by default. To write participant time series, enable:

```
simona.output.participant.defaultConfig.simulationResult = true
```

To analyse the grid impact, enable the required grid outputs:

```
simona.output.grid.nodes = true
simona.output.grid.lines = true
simona.output.grid.transformers2w = true
```

If congestion results should be written as well, enable:

```
simona.output.grid.congestions = true
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