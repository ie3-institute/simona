(thermal_house_model)=

# Thermal House Model

This page documents the functionality of the thermal house available in SIMONA.


## Behaviour

This house model represents the thermal behaviour of a building. It represents a simple shoebox with thermal capacity and transmission losses.
The house can optionally be equipped with a {ref}`cts_model` as thermal storage. Both are connected by the {ref}`thermal_grid_model`.

The thermal house provides two different energy demands. The required demand indicates that the inner temperature of the house is below the lower temperature boundary and thus, requires mandatory heating. An additional demand indicates the amount of energy necessary to reach the target temperature. Additional demand not necessarily requires to be covered but could, e.g. for flexibility purposes.

There are different operating modes, depending on whether the heat source is em-controlled or not.

### Behaviour without EM control

If the heat source of this building is not under {ref}`em` control, the internal temperature of the house should remain between the target temperature and lower temperature boundary. If the temperature falls below the lower temperature limit, the available energy from the storage is used first. If the storage 
is empty, the heat pump will first heat the house up to the target temperature and then refill the storage.
As the storage is initialised as empty, the heat source will start charging the storage first. Whenever the heat source is in operation (e.g. to charge the storage), it will continue to operate until the house has reached the target temperature again.

### Behaviour under EM control

When {ref}`em` is applied to the heat source of this building, the thermal behaviour should be basically the same as without EM control, so internal temperature of the house should remain between the target temperature and the lower temperature limit. 

However, for flexibility usage, the energy management system can turn on the heat source whenever the thermal house has additional demand. Same for the thermal storage. When the heat source is EM controlled, the thermal storage will only be recharged when the flexibility strategy allows. E.g. when there is surplus energy from PV plant.


## Physical Model

### Inner Temperature Calculation

$$
\K1 = \frac{P}{m \cdot c} + \frac{\lambda \cdot A \cdot T_out}{d \cdot m \cdot c}
$$

$$
\K2 = \frac{\lambda \cdot A}{d \cdot m \cdot c}
$$

$$
\TDot = K_{1} - K_{2} \cdot T_{\theta} \frac{\lambda \cdot A}{d \cdot m \cdot c}
$$

*with*\
**T** = 


## Attributes, Units and Remarks

Please refer to  {doc}`PowerSystemDataModel - Thermal House Model <psdm:models/input/thermal/thermalhouse>` for Attributes and Units used in this Model.
