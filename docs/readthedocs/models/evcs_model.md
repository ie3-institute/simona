(evcs_model)=
# EvcsModel


The currently connected EvModels are saved within the state data of EvCsAgent and passed to EvCsModel with each trigger. The model then calculates the active load of the charging station.

## Attributes, Units and Remarks

| Attribute          | Unit | Remarks                                         |
|--------------------|------|-------------------------------------------------|
| uuid               |  --  | universally unique identifier                   |
| id                 |  --  | human readable identifier                       |
| operationInterval  |  --  | time interval, in which the system is operating |
| scalingFactor      |  --  | scaling the output of the system                |
| qControl           |  --  | type of reactive power control                  |
| sRated             |  kVA | rated apparent power                            |
| cosPhiRated        |  --  | rated power factor                              |
| chargingPoints     |  --  | number of charging points available             |
| locationType       |  --  | the charging station location type              |

## Calculation


The following arguments need to be provided for power calculation:


| Argument         | Unit    | Remarks                                       |
|------------------|---------|-----------------------------------------------|
| dataFrameLength  | --      | duration in ticks until next EV movement data |
| currentEVs       | --      | set of EVs connected to the CS at this moment |
