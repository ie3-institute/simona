(reference_system)=

## Reference System

The reference system is built up by specifying the included voltage levels. The following table describes typical network levels and the corresponding parameterization.

### Typical voltage levels 

| voltage level (id) | nominal voltage (vNom) | apparent power (sNom) |
|--------------------|------------------------|-----------------------|
| NS                 | 0.4 kV                 | 100 kVA               |                           
| MS                 | 20 kV                  | 60 MVA                |
| HS                 | 110 kV                 | 600 MVA               |
| HoeS               | 380 kV                 | 1000 MVA              |

### Configuration of the reference system 

To configure the reference system, the voltage levels listed in the table above must be selected and integrated into the reference system with the nominal apparent power (sNom), the nominal voltage (vNom), and the individual voltage level configuration (voltLvls) according to the example below. Each voltage level is composed of the identifier and the nominal voltage.

```
simona.gridConfig.refSystems = [
    {sNom = "100 kVA", vNom = "0.4 kV", voltLvls = [{id = "NS", vNom = "0.4 kV"}]},
    {sNom = "60 MVA", vNom = "20 kV", voltLvls = [{id = "MS", vNom = "20 kV"}]},
    {sNom = "600 MVA", vNom = "110 kV", voltLvls = [{id = "HS", vNom = "110 kV"}]},
    {sNom = "1000 MVA", vNom = "380 kV", voltLvls = [{id = "HoeS", vNom = "380 kV"}]}
]
```
