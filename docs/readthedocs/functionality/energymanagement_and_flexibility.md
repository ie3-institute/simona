# Energy Management and Flexibility Utilisation

SIMONA can be used to analyse flexibility options of system participants such as storages, electric vehicle charging
stations or other controllable assets.

## Using flexibility output

Flexibility results are disabled by default. To enable flexibility results for all system participants, set:

```
simona.output.participant.defaultConfig.flexResult = true
```

The regular participant simulation result output is also disabled by default. To write participant time series, enable:

```
simona.output.participant.defaultConfig.simulationResult = true
```

The snippets shown here can be added directly to the SIMONA configuration file. Single parameter paths are equivalent to
setting the corresponding value inside a nested configuration block.

## Storage configuration

The initial state of charge of storages can be configured with:

```
simona.runtime.participant.storage.initialSoc = 0.0
```

The default value is `0.0`.

A target state of charge can be configured with:

```
simona.runtime.participant.storage.targetSoc = 1.0
```

By default, no target state of charge is set. The state of charge values are given in the range from `0.0` to `1.0`.

The storage scaling factor defaults to `1.0` and only has to be changed for scenario scaling:

```
simona.runtime.participant.storage.scaling = 1.0
```

## Energy management configuration

The runtime configuration for energy management is located under:

```
simona.runtime.em
```

Curtailment of regenerative generation is disabled by default. It can be enabled with:

```
simona.runtime.em.curtailRegenerative = true
```

The energy management scaling factor defaults to `1.0`:

```
simona.runtime.em.scaling = 1.0
```

## Grid impact

If the impact of flexibility on the grid should be analysed, a power flow configuration and grid output should be enabled.

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

For example, grid output can be enabled with:

```
simona.output.grid.nodes = true
simona.output.grid.lines = true
simona.output.grid.transformers2w = true
```

## Typical application

This functionality can be used to analyse available flexibility and its influence on participant behaviour and grid
loading.