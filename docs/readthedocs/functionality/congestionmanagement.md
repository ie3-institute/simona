# Congestion management

SIMONA is able to perform a basic congestion management that includes:
1. Congestion detection
2. Mitigation measures

If enabled, the congestion management is run after the normal power flow calculation. This page will introduce the congestion
management and how to configure it briefly. For more information please refer to {cite:p}`11443104`.


## Using the congestion management

To use the congestion management, the congestion detection needs to be enabled first:

```
simona.congestionManagement.enableDetection = true
```

By enabling the detection, SIMONA uses the power flow results for node voltage magnitudes, line currents and transformer
loading to check for congestions {cite:p}`11443104`. If no mitigation measure is enabled, these results are written out
together with the other SIMONA results. In case at least one measure is available, SIMONA will try to apply it to solve
the congestion {cite:p}`11443104`.

## Mitigation measures

Currently, only transformer tap changes are included as mitigation measures. This measure can be enabled by:

```
simona.congestionManagement.enableTransformerTapChange = true
```

The measure will consider all transformers that are configured to support automatic tapping (`autoTap=true`) for the
mitigation {cite:p}`11443104`. The main focus of this measure is to fix over- and undervoltages, but it can also be used
to reduce line overloading. To do this, for each line an equivalent voltage change delta is calculated (see: {cite:p}`11443104`).
With this, the line current can be considered.
