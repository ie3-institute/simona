# Congestion management

SIMONA is able to perform a basic congestion management that includes:
1. Congestion detection
2. Mitigation measures

If enabled, the congestion management is run after the power flow calculation. This page will introduce the congestion
management and how to configure it briefly. For more information please refer to {cite:p}`11443104`.


## Using the congestion management

To use the congestion management, the congestion detection needs to be enabled first:

```
simona.congestionManagement.enableDetection = true
```

By enabling the detection, SIMONA uses the power flow results for node voltage magnitudes, line currents and transformer
loading to check for congestions. If no mitigation measure is enabled, these results are written out together with the
other SIMONA results. In case at least one measure is available by enabling it, SIMONA will try to apply it to solve the
congestion {cite:p}`11443104`.

## Mitigation measures

Currently, only transformer tap changes are included as mitigation measures. This measure can be enabled by:

```
simona.congestionManagement.enableTransformerTapChange = true
```

The primary objective of this measure is to resolve over- and undervoltage issues and will be applied for all transformers
that are configured with automatic tapping (`autoTap=true`). In addition to voltage management, it can also be used to
mitigate line overloading by using an equivalent voltage change delta, that is calculated for each line (see: {cite:p}`11443104`).
This allows the line current to be considered.

### Mitigation via Transformer tap change

One way to solve violations of the voltage band or line overloading is the transformer tap change. This mitigation measure
performs the following steps:
1. Calculating the possible voltage increase and decrease with the given voltage band and maximal and minimal voltage magnitude.
2. Determine limitations imposed by inferior subgrids. Here the ability of the connecting transformers to change their tap position is also considered.
3. Determine limitations or needed voltage increase imposed by the maximal line current.

To consider the line currents, the current of the line with the highest loading is converted into an equivalent voltage
delta. To derive a formula, the assumption is made that the power flow is constant. This formula can be found in {cite:p}`11443104`.
This delta is then used to define, if possible, either a minimal needed voltage increase to reduce the current or a maximal
possible increase to prevent line overloading.
