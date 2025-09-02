(cts_model)=
# Cylindrical Thermal Storage
This page documents the functionality of the cylindrical thermal storage available in SIMONA.

## Behaviour
This storage model operates on volumes, although the functions it provides for other models all operate with energy. Internally the storage model converts energy to volume and vice versa with formulas specified below. Furthermore, it is assumed that the storage medium is water. Also, the model holds a variable for the current storage level. At initialisation the storage will be empty.

## Attributes, Units and Remarks

Please refer to  {doc}`PowerSystemDataModel - CTS Model <psdm:models/input/thermal/cylindricalstorage>` for Attributes and Units used in this Model.

## Calculations
### Maximal storage capacity
As the name suggests this storage has a cylindric form, hence its maximum storage capacity $V_{st, max}$ can be given by:

$$
V_{st, max} = \pi \cdot r^2 \cdot h
$$

### Conversions of energy and volume

In this model the change of stored energy (heat) is represented by the change of the volume of the storage medium. This relation between stored heat and volume change is given by:

$$
V = (\frac{W}{c \cdot (tE - tA)})
$$

with
W = energy to be converted to volume
V = resulting volume
tE = return temperature
tA = inlet temperature
c = $1,15 \frac{kWh}{m^3 \cdot K}$

The original equation is given by:

$$
\Delta V_{st} = (\frac{\dot{Q_{th}} \cdot \Delta t}{c \cdot (tA - tE)})
$$

Reference:

* {cite:cts}`Quaschning.2013`


That is the mathematical description of loading and unloading processes concerning the buffer storage. Whenever heat is stored within the storage or removed from the storage this equation is used. This includes the case that the whole heat demand is satisfied by the storage.

The same relationship is used to determine the quantity of heat which is stored in the storage by converting the equation to:

$$
W = V \cdot c \cdot (tE - tA)
$$

## Store/Take energy

This calculation is performed as follows: An amount of energy is specified for storing or taking. The model then changes the storage level and returns if it exceeded the maximum (when storing energy) or the minimum (when taking energy).
