# Ampacity

*SIMONA* provides a thermal model for underground power cable line segments. Instead of using a static ampacity value 
from a cable data sheet, the thermal model tracks the temperature of the individual cable layers during the simulation.

## Structure of the ampacity package

The ampacity functionality is located in `src/main/scala/edu/ie3/simona/model/grid/ampacity/`
and consists of the following components:

| Component                      | Role                                                                                       |
| ------------------------------ | ------------------------------------------------------------------------------------------ |
| `CableSetup`                   | Data model of the cable (layers, temperatures, electrical and loss parameters).            |
| `LayoutFormation`              | Geometric arrangement of the cable axes; selects the loss and resistance formulas.         |
| `LineSegmentThermalModel`      | Thermal model of one line segment (resistances, capacities, soil, depth, neighbours).      |
| `LineThermalModelCalculations` | Shared constants and helpers, e.g. the lay factor of the screen layer.                     |
| `SegmentCalculationPoints`     | Determines the calculation point (geometric midpoint) of a thermal segment for the weather registration. |
| `SoilThermalResistanceCalculations` | Calculations of the external thermal resistance (T4) to soil, thermal capacitances, and cable bed related quantities (e.g. the equivalent radius of a cable bed). |
| `ThermalSegmentBuilder`        | Builds [[LineSegmentThermalModel]] instances from a subgrid, including validation of soil layers, cable types, and cable deployments. |


## LineSegmentThermalModel

`LineSegmentThermalModel` is the thermal model of a single line segment. It is
created by the [[ThermalSegmentBuilder]] and carries everything the thermal
simulation of the segment needs:

- **Cable geometry and setup**: the cable setup ([[CableSetup]]), the 3D start
  and end coordinates of the segment, the laying depth below ground level, and
  the distance between the cables of a multi-cable deployment.
- **Thermal parameters**: the thermal resistances T1–T4 and the thermal
  capacities of the four cable elements (conductor, dielectric, screen, armor
  and jacket), the cable bed, and the surrounding soil, all per unit of length.
- **Soil**: the soil type surrounding the cable, an optional rectangular cable
  bed (`CableBed`), and the ordered sequence of horizontal soil layers
  (`SoilLayerInfo`) from the ground surface downwards.
- **Parallel influence**: the influence of neighbouring cables running
  parallel and close to the segment as a sequence of (uuid, influence factor)
  pairs.

The simulation state of a segment is captured in `LineState`, which holds the
current tick, the cable setup, the current [[LineSegmentThermalModel]], the
ground temperature, the temperatures of the cable layers
(`LineTemperatures`), the current voltage at the cable, and a flag indicating
whether a dry zone is active around the cable. The class also defines the
`LineModelTemperatureUpperBoundaryReached` thermal threshold, which is reached
when the upper boundary temperature of the cable is exceeded.

## ThermalSegmentBuilder

`ThermalSegmentBuilder` is responsible for constructing
[[LineSegmentThermalModel]] instances from a [[SubGridContainer]]. Its main
entry point is `build`, which:

1. extracts the soil layers and soil types from the subgrid container,
2. generates thermal segments for all lines that have both a cable type and a
   cable deployment, and
3. writes the generated segments to the simulation output folder
   (`rawOutputData/thermal_line_segments.csv`).

When generating the segments, the builder splits each line along its
centerline into sub-segments at

- **soil layer boundaries**, so that every segment lies in a homogeneous soil
  layer (at most three covering soil layers are supported), and
- **parallel-proximity stretch boundaries**, when this feature is activated.
  Neighbour lines running within the configured threshold distance cause the
  segment to be split so that the parallel influence can be assigned per
  segment. Stretches shorter than the minimum overlap length are skipped.

For each resulting sub-segment the builder assigns the covering soil layer and
soil type at the segment midpoint, resolves the parallel influence of
neighbouring lines, determines the calculation point for the weather
registration, and computes the per-unit-length thermal capacities of the four
cable elements.

The builder also validates the input data required for the ampacity
calculation and fails the simulation with a
`GridAgentInitializationException` if, for example, no cable types or no cable
deployments are available, a line references more than one deployment, the
cable depth is not covered by any soil layer, or the cable bed geometry does
not enclose the cable formation.