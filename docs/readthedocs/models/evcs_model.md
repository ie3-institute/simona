(evcs_model)=
## Electric Vehicle Charging Station Model

The currently connected EvModels are saved within the state data of EvcsAgent and passed to EvcsModel with each trigger. The model then calculates the active load of the charging station.

### Attributes, Units and Remarks

Please refer to {doc}`PowerSystemDataModel - Evcs Model <psdm:models/input/participant/evcs>` for Attributes and Units used in this Model.

### Calculation


The following arguments need to be provided for power calculation:


| Argument         | Unit    | Remarks                                       |
|------------------|---------|-----------------------------------------------|
| dataFrameLength  | --      | duration in ticks until next EV movement data |
| currentEVs       | --      | set of EVs connected to the CS at this moment |
