# Ampacity

*SIMONA* provides a thermal model for underground power cable line segments. Instead of using a static ampacity value 
from a cable data sheet, the thermal model tracks the temperature of the individual cable layers during the simulation.
The losses per unit cable length, which act as the heat sources of the thermal model, are calculated by the
`CableLossCalculations` object in the package `edu.ie3.simona.model.grid.ampacity`.

## Structure of the ampacity package

The ampacity functionality is located in `src/main/scala/edu/ie3/simona/model/grid/ampacity/`
and consists of the following components:

| Component                      | Role                                                                                       |
| ------------------------------ | ------------------------------------------------------------------------------------------ |
| `CableSetup`                   | Data model of the cable (layers, temperatures, electrical and loss parameters).            |
| `LayoutFormation`              | Geometric arrangement of the cable axes; selects the loss and resistance formulas.         |
| `LineSegmentThermalModel`      | Thermal model of one line segment (resistances, capacities, soil, depth, neighbours).      |
| `LineThermalModelCalculations` | Shared constants and helpers, e.g. the lay factor of the screen layer.                     |
| `CableLossCalculations`        | Loss engine: AC resistances and conductor, screen, armor and dielectric losses.            |

## Methods of `CableLossCalculations`

The public methods of `CableLossCalculations` calculate the heat sources that drive
the thermal model. All losses are returned as a power per unit of cable length
(`PowerPerLength`).

### `calcAcResistance`

Calculates the **AC resistance per unit length of the conductor** at the operating
temperature, accounting for high-frequency effects:

- The DC resistance at the reference temperature (20 °C), taken from the cable data
  sheet, is corrected to the limit temperature of the cable using the temperature
  correction factor for resistance.
- The **skin effect factor** `y_s` is calculated from the conductor's skin effect
  coefficient `k_s`, the frequency and the DC resistance.
- The **proximity effect factor** `y_p` is calculated for three single-core cables
  in trefoil or three-core cables from the proximity effect coefficient `k_p`, the
  conductor diameter and the distance between the cable axes.

The result is the DC resistance multiplied by `(1 + y_s + y_p)`.

### `calcAcResistanceSheath`

Calculates the **AC resistance per unit length of the screen (sheath)** at the
operating temperature. The screen's DC resistance is derived from the material
resistivity and the cross-sectional area (or, if not given, from the number and
diameter of the screen wires), corrected by the **lay factor** of the screen
winding. The operating temperature of the screen is estimated from the limit
temperature minus the temperature drop over the internal thermal resistance T1,
caused by the conductor and half of the dielectric losses. The result is then
corrected to this operating temperature using the material's temperature
coefficient of electrical resistivity.

### `calcLossesConductor`

Calculates the **conductor (I²R) losses** per unit cable length: the square of the
line current multiplied by the AC resistance of the conductor.

### `calcLossesSheath`

Calculates the **losses in the screen (sheath)** per unit cable length as a product
of the conductor losses and the screen loss factor `lambda_1`, which is split into
a part from circulating currents `lambda_1'` and a part from eddy currents
`lambda_1''` (given in the cable setup):

- In **flat formation** (touching or with distance), `lambda_1'` is calculated
  phase-by-phase (leading, middle, lagging) from the screen reactance, the mutual
  reactance between the screens and the resistance ratio of screen and conductor.
- In **trefoil (touching) formation**, `lambda_1'` is calculated from the
  resistance ratio of screen and conductor and the screen reactance.
- The *trefoil not touching* formation is currently not supported and results in
  an `IllegalArgumentException`.

The screen reactance `x` and the mutual reactance `x_m` are calculated internally
from the system frequency, the axial distance between the cables and the average
diameter of the sheath.

### `calcLossesArmor`

Calculates the **losses in the armor** per unit cable length as a product of the
conductor losses and the armor loss factor `lambda_2`, which is the sum of a part
from circulating currents `lambda_2'` and a part from eddy currents `lambda_2''`,
both of which are given as parameters (e.g. from the cable data sheet).

### Dielectric losses

Calculates the **voltage-dependent dielectric losses** per unit cable length from
the phase-to-ground voltage, the system frequency, the dielectric loss tangent
`tan delta` and the specific capacitance of the cable dielectric. Unlike the
conductor, screen and armor losses, these losses are not current-dependent. This
calculation is provided by the `RichSpecificCapacitance` extension in
`edu.ie3.util.scala.quantities.SquantsUtils` (`calculateSpecificDielectricLosses`).

## Notes

- The loss calculations are the physical core of the thermal model: they provide
  the heat sources (power per unit length) for the internal and external thermal
  circuit of the `LineSegmentThermalModel`.
- The integration of the ampacity package into the line models and the load flow
  (e.g. for dynamic ampacity-based congestion detection) is currently still in
  progress.
