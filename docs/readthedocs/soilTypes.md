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

   * - name
     - –
     - Human readable name of the soil type

   * - tr_wet
     - K·m/W
     - Thermal resistivity when wet

   * - tr_dry
     - K·m/W
     - Thermal resistivity when dry

   * - shc
     - kWh/m³K
     - Specific heat capacity

   * - crit_temp_diff
     - °C
     - Critical temperature difference for ampacity calculations
```

## Notes

- Header names are checked strictly and must match the attribute names above.
- Numeric values use a dot as decimal separator.

## Example CSV

```text
uuid,name,tr_wet,tr_dry,shc,crit_temp_diff
78b72d1d-9d05-446b-a072-426eb4d3807e,loam,0.30,0.40,0.0015,15.0
```
