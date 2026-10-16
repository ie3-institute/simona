# Soil types CSV

This document describes the expected CSV format for soil types used by SoilDataParser.

## Attributes, Units and Remarks

### Type Model

```{list-table}
   :widths: auto
   :class: wrapping
   :header-rows: 1

   * - Attribute
     - Unit
     - Remarks

   * - uuid
     - –
     - UUID of the soil type record

   * - id
     - –
     - Human readable name of the soil type

   * - thermal_resistivity_wet
     - K·m/W
     - Thermal resistivity when wet

   * - thermal_resistivity_dry
     - K·m/W
     - Thermal resistivity when dry

   * - specific_heat_capacity
     - kWh/m³K
     - Specific heat capacity

   * - critical_temperature_difference
     - °C
     - Critical temperature difference for ampacity calculations
```

## Notes

- Header names are checked strictly and must match the attribute names above.
- Numeric values use a dot as decimal separator.

## Example CSV

```text
uuid,id,thermal_resistivity_wet,thermal_resistivity_dry,specific_heat_capacity,critical_temperature_difference
78b72d1d-9d05-446b-a072-426eb4d3807e,loam,0.30,0.40,0.0015,15.0
```
