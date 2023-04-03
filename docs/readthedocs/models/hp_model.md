(hp_model)=

# Heat Pump Model

This page documents the functionality of the Heat Pump Model  (HP model) available in SIMONA. This model is part of the SIMONA simulation framework and represented by an agent. In the current model, it is feasible to simulate nano- and micro-CHP units. As it is a cross-sector agent heat/electricity) the usage of a house-heat-model is mandatory.  In the following, the CHP unit model and its controller unit is described. The controller unit is necessary to ensure, that different kinds of heat models can be applied.

## Assumptions

The HP unit is able to operate either at full load or not at all. Uncovered heat demand of former time-steps is not considered in the following steps, as the HP unit does not posses a memory.

## Parameters

## Attributes, Units and Remarks

Please refer to {doc}`PowerSystemDataModel - HP Model <psdm:models/input/participant/hp>` for Attributes and Units used in this Model.

## Implemented Behaviour

- Agent Objectives
  The objective of the HP agent is to cover the given heat demand of it's thermal bus in each time-step.
- Storage medium
  This model can be implemented with any mutable storage (meaning that the storage requires the trait MutableStorage).

The implemented behaviour is shown in the program sequence plan below. In general, the unit will turn on, if the thermal bus is demanding heat. During operation, the HP unit operates at full utilization.
