(reference_system)=

# Reference System

The reference system is built up by specifying the included voltage levels. The following table describes typical network levels and the corresponding parameterization.

## Default reference system

```{list-table}
:widths: auto
:header-rows: 1

* - Voltage level (id)
  - Nominal voltage (vNom)
  - Apparent power (sNom)

* - LV
  - 0.4 kV
  - 100 kVA

* - MV
  - 10 kV
  - 40 MVA
  
* - MV
  - 20 kV
  - 60 MVA
  
* - MV
  - 30 kV
  - 150 MVA
  
* - HV
  - 110 kV
  - 600 MVA
  
* - EHV
  - 220 kV
  - 800 MVA
  
* - EHV
  - 380 kV
  - 1000 MVA
```


## Configuration of the reference system 

To configure the reference system, the voltage levels listed in the table above must be selected and integrated into the reference system with the nominal apparent power (sNom), the nominal voltage (vNom), and the individual voltage level configuration (voltLvls) according to the example below. Each voltage level is composed of the identifier and the nominal voltage.

```
simona.gridConfig.refSystems = [
    {sNom = "100 kVA", vNom = "0.4 kV", voltLvls = [{id = "LV", vNom = "0.4 kV"}]},
    {sNom = "60 MVA", vNom = "20 kV", voltLvls = [{id = "MV", vNom = "20 kV"}]},
    {sNom = "600 MVA", vNom = "110 kV", voltLvls = [{id = "HV", vNom = "110 kV"}]},
    {sNom = "1000 MVA", vNom = "380 kV", voltLvls = [{id = "EHV", vNom = "380 kV"}]}
]
```
