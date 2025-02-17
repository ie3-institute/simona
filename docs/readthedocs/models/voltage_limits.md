(voltage_limits)=

# Voltage Limits

The voltage limits are built up by specifying the included voltage levels. The following table describes typical network
levels and their parameterization. They are primarily used for the optional congestion management.

## Default voltage limits

```{list-table}
:widths: auto
:header-rows: 1

* - Voltage level (id)
  - Minimal voltage (vMin)
  - Maximal voltage (vMax)

* - LV
  - 0.9 p.u.
  - 1.1 p.u.

* - MV (10 kV)
  - 0.9 p.u.
  - 1.1 p.u.

* - MV (20 kV)
  - 0.9 p.u.
  - 1.1 p.u.
  
* - MV (30 kV)
  - 0.9 p.u.
  - 1.1 p.u.
  
* - HV
  - 0.9 p.u.
  - 1.1 p.u.
  
* - EHV (220 kV)
  - 0.9 p.u.
  - 1.118 p.u.
  
* - EHV (380 kV)
  - 0.9 p.u.
  - 1.05 p.u.
```

**References:**

* {cite:cts}`EN_50160`
* {cite:cts}`EU_2017/1485`

## Configuration of the voltage limits

To configure the voltage limits, the voltage levels listed in the table above must be selected and integrated into the
config. The individual voltage level configuration (voltLvls) according to the example below. Each voltage level consists
of an identifier and the nominal voltage.

```
simona.gridConfig.voltageLimits = [
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Lv", vNom = "0.4 kV"}]},
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Mv", vNom = "20 kV"}]},
  {vMin = 0.9, vMax = 1.1, voltLvls = [{id = "Hv", vNom = "110 kV"}]},
  {vMin = 0.9, vMax = 1.05, voltLvls = [{id = "EHV", vNom = "380 kV"}]},
]
```
