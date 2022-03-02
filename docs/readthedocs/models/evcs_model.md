(evcs_model)=
## Electric Vehicle Charging Station Model (EvCs Model)

The currently connected EvModels are saved within the state data of EvCsAgent and passed to EvCsModel with each trigger. The model then calculates the active load of the charging station.

### Attributes, Units and Remarks

Attributes and Units are defined at {doc}`PowerSystemDataModel - EvCs Model <psdm:models/input/participant/evcs>` please have a look.

### Calculation


The following arguments need to be provided for power calculation:


| Argument         | Unit    | Remarks                                       |
|------------------|---------|-----------------------------------------------|
| dataFrameLength  | --      | duration in ticks until next EV movement data |
| currentEVs       | --      | set of EVs connected to the CS at this moment |
