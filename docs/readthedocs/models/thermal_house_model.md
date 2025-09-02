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

### Thermal Calculations

With formula of the thermal flux for the heat losses by 

$$
\dot{Q} = \lambda \cdot A \cdot \frac{T_{inner} - T_{outer}}{d}
$$
*where*\
**$\dot{Q}$** = Thermal Flux in $W$\
**$\lambda$** = Thermal Conductivity in $\frac{W}{m \cdot K}$\
**A** = Surface in $m^2$\
**T** = Inner and Outer Temperature in $K$


and of temperature deviation when heating by

$$
\Delta{T} = \frac{\Delta{Q}}{m \cdot c}
$$
*where*\
**$\Delta{T}$** = Temperature difference $T_{inner}$ and $T_{outer}$ in $K$\
**$\Delta{Q}$** = Transferred heat in $J$\
**m** = Mass of the medium in $kg$\
**c** = Specific Heat Capacity of the medium in $\frac{J}{kg \cdot K}$

One derives

$$
T(t) - T(0) = \frac{ \int_{t_0}^{t}p(\tau) - loss(\tau) d\tau}{m \cdot c}
$$

This can be transformed to

$$
\dot{T}(\tau) = \frac{1}{m \cdot c} \left( p_\tau - \frac{\lambda \cdot A}{d} \left( T(\tau) - T_{out,\tau} \right) \right)
$$

By defining

$$
p_\tau = P
$$ 

$$
T_{out,\tau}=T_{ambient}
$$

$$
k_1 = \frac{P}{m \cdot c} + \frac{\lambda \cdot A \cdot T_{ambient}}{d \cdot m \cdot c}
$$

$$
k_2 = \frac{\lambda \cdot A}{d \cdot m \cdot c}
$$

this will be

$$
\dot{T} = k_1 - k_2 \cdot T(\tau)
$$

By integration one receives

$$
T(\tau) = C e^{-k_2 \cdot \tau} \cdot T(\tau)
$$


By replacing with the house parameters of thermal losses

$$
\frac{\lambda \cdot A}{d} = thermal losses
$$

and thermal capacity

$$
m \cdot c = thermal capacity
$$

we $k_1$ and $k_2$ transforms to

$$
k_1 = \frac{P}{ethCapa} + \frac{ethLosses \cdot T_{ambient}}{ethCapa}
$$

$$
k_2 = \frac{ethLosses}{ethCapa}
$$

### Inner Temperature Calculation
Thus, the inner temperature can be calculated by

$$
T_1 = \left(T_0 - \frac{k_1}{k_2}\right) \cdot e^{-k_2 \cdot t} + \frac{k_1}{k_2}
$$
*where*\
**$T_1$** = New Temperature in $K$\
**$T_0$** = Last Temperature in $K$

### Calculate next threshold

Since for $t \rightarrow \infty$ 

$$
T_1 = \left(T_0 - \frac{k_1}{k_2}\right) \cdot e^{-k_2 \cdot t} + \frac{k_1}{k_2}
$$

will be

$$
T_{\infty} = \frac{k_1}{k_2}
$$

This allows to determine which boundary will be reached next. The exact time step can be calculated by

$$
t = \frac{1}{-k_2} \cdot  \ln{ \left(\frac{T_1 - \frac{k_1}{k_2}}{T_0 - \frac{k_1}{k_2}} \right) } 
$$
*where*\
**$T_1$** = New Temperature in $K$\
**$T_0$** = Last Temperature in $K$


**References:**

* {cite:cts}`Meschede.2005`
* {cite:cts}`Becker.1991`

## Attributes, Units and Remarks

Please refer to  {doc}`PowerSystemDataModel - Thermal House Model <psdm:models/input/thermal/thermalhouse>` for Attributes and Units used in this Model.
