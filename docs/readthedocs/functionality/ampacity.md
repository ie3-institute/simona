# Ampacity

This section documents CSV formats related to ampacity calculations.

## Cable-bed thermal model

When a cable bed is configured, the short-duration thermal network uses ten nodes: five cable nodes followed by 
two cells for the bedding material and three cells for the surrounding soil. Each environmental cell uses the 
material's thermal resistivity and volumetric heat capacity; the equivalent-radius geometry and resistance-matched 
outer soil radius determine the cell boundaries.

The external thermal resistance is divided between the bed and outer soil so their series resistance. Cable beds 
must enclose the cables, use one surrounding soil layer, and have a rectangular aspect ratio below 3. Layered 
outer soil and unsupported cable formations are rejected during initialization. Long-duration cable-bed 
transients are not modelled by this network.

```{toctree}
---
maxdepth: 1
---
../soilTypes
../soilLayers
```
