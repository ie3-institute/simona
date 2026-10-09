# Soil layers CSV

This document describes the expected CSV format for soil layers used by SoilDataParser.

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
     - UUID of the soil layer record

   * - geometry
     - –
     - GeoJSON geometry (Polygon) encoded as a JSON string; must be double-quoted in the CSV so internal commas do not break the CSV

   * - z_from
     - m
     - Start depth (upper boundary)

   * - z_to
     - m
     - End depth (lower boundary)

   * - soil_type
     - UUID
     - Reference to a soil type uuid defined in the soil types CSV
```

## Notes

- Header names are checked strictly and must match the attribute names above.
- Depths (z_from, z_to) are in meters. Typical convention: z_from >= z_to for layers below surface (e.g., 0.0 to -2.0).
- The geometry content must be valid GeoJSON (usually a Polygon) encoded as a JSON string and double-quoted in the CSV.

## Example CSV

```text
uuid,geometry,z_from,z_to,soil_type
f07aa67c-43f5-4706-967a-5d0613a94701,"{""type"":""Polygon"",""coordinates"":[[[7.40383,51.49129],[7.40562,51.49130],[7.40560,51.49106],[7.40377,51.49105],[7.40383,51.49129]]]]}" ,0.0,-2.0,32b43a78-7721-431d-b1c2-56975a123670
```
