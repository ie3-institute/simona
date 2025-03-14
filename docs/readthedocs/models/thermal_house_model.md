(thermal_house_model)=

# Thermal House Model

This page documents the functionality of the thermal house available in SIMONA.

## Behaviour

This house model represents the thermal behaviour of a building. It represents a simple shoebox with thermal capacity and  transmission losses for its heating demand. As well the hot water demand (tap water) based on the number of inhabitants and the housing type on hourly basis is considered. The house can optionally be equipped with a {ref}`cts_model` as thermal storage. Both are connected by the {ref}`thermal_grid_model`.

The thermal house provides two different energy demands. The required demand indicates that the inner temperature of the house is below the lower temperature boundary and thus, requires mandatory heating. An additional demand indicates the amount of energy necessary to reach the target temperature. Additional demand not necessarily requires to be covered but could, e.g. for flexibility purposes.

There are different operating modes, depending on whether the heat source is em-controlled or not.

### Behaviour without EM control

If the heat source of this building is not under {ref}`em` control, the internal temperature of the house should remain between the target temperature and lower temperature boundary. If the temperature falls below the lower temperature limit, the available energy from the storage is used first. If the storage 
is empty, the heat pump will first heat the house up to the target temperature and then refill the storage.
As the storage is initialised as empty, the heat source will start charging the storage first. Whenever the heat source is in operation (e.g. to charge the storage), it will continue to operate until the house has reached the target temperature again.

### Behaviour under EM control

Currently not fully supported

## Attributes, Units and Remarks

Please refer to  {doc}`PowerSystemDataModel - Thermal House Model <psdm:models/input/thermal/thermalhouse>` for Attributes and Units used in this Model.
