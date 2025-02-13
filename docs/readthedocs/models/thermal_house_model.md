(thermal_house_model)=

# Thermal House Model

This page documents the functionality of the thermal house available in SIMONA.


## Behaviour

This house model represents the thermal behaviour of a building. It represents a simple shoebox with thermal capacity and transmission losses.
The house can optionally be equipped with a thermal energy storage. Both are connected by the ({ref}`thermal_grid_model`).

There are different operating modes, depending on whether the heat source is em-controlled or not.

### Behaviour without EM control

If the heat source of this building is not under EM control, the internal temperature of the house should remain between the upper and lower temperature limits. If the temperature falls below the lower temperature limit, the available heat from the storage is used first. If the storage 
is empty, the heat pump will first heat the house up to the upper temperature limit and then refill the storage.
As the storage is initialised as empty, the heat source will start charging the storage first. Whenever the heat source is in operation (e.g. to charge the storage), it will continue to operate until the house has reached the upper temperature limit again.

### Behaviour under EM control

When EM is applied to the heat source of this building, the internal temperature of the house should remain between the target temperature and the lower temperature limit. For flexibility, the energy management system can additionally heat the house up to the upper temperature limit.

If the temperature falls below the lower temperature limit, the available heat in the storage is used first. If the storage is empty, the heat pump will first heat the house to the target temperature limit and then refill the storage.
As the storage is initialised as empty, the heat source will start charging the storage first. Whenever the heat source is in operation (e.g. to charge the storage), it will continue to operate until the house has reached the target temperature boundary again.

To give the system flexibility, the heat source can heat the house up to the upper temperature limit. Heating can also be stopped before the upper temperature limit is reached. The available capacity of the thermal storage can also be used for flexibility purposes by recharging the storage before it is empty.


## Attributes, Units and Remarks

Please refer to  {doc}`PowerSystemDataModel - Thermal House Model <psdm:models/input/thermal/thermalhouse>` for Attributes and Units used in this Model.
