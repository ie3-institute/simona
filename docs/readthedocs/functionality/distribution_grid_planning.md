# Distribution Grid Planning

SIMONA can be used to generate time series of grid utilisation for distribution grid planning. The main results are node
voltages, line loading and transformer loading.

## Using distribution grid planning

For grid planning studies, a power flow configuration has to be provided. If no power flow configuration is present,
SIMONA skips all power flow calculations.

The following snippets can be added to the SIMONA configuration file. Single parameter paths such as
`simona.output.grid.nodes = true` are equivalent to setting the corresponding value inside a nested configuration block.

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

Grid outputs are disabled by default. To analyse the grid state, enable the required outputs:

```
simona.output.grid.nodes = true
simona.output.grid.lines = true
simona.output.grid.transformers2w = true
```

If congestion results should be written as well, enable:

```
simona.output.grid.congestions = true
```

Participant results are disabled by default. To write participant time series, enable:

```
simona.output.participant.defaultConfig.simulationResult = true
```