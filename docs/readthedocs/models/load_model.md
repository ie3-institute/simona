## Load Model Configuration

### General Information

SIMONA supports different load model behaviours, which need some parameterization made by this config entry. It can be found in the config tree at ```simona.runtime.participant.load```.

This sub configuration is two-fold:

Default Configuration
~ This is one configuration, that applies to all load models, except of those, that have a individual config assigned. To simplify code, no distinct model class is introduced for the single config. In order to highlight the fact, that this config is a default config, you may write ```uuids = ["default"]```. If you put anything else than this, you will get a warning, that those references are neglected, although the rest will work just fine.

Set of Individual Configurations
~ This part holds a set of configurations, that will apply to specific load models, denoted by their uuid. To simplify config generation, you are able to assign the same config to a list of uuids in batch. If one load has no individual configuration assigned, it will default to the above given config.

The following parameters are available (as well for default as for individual assignment):


| Parameter      | Type         | Permissible input            |
| ---------------- | -------------- | ------------------------------ |
| uuid           | List[String] | List of valid UUIDs          |
| scaling        | Double       | All positive Doubles         |
| modelBehaviour | String       | 'fix', 'profile' or 'random' |
| reference      | String       | 'power' or 'energy'          |

#### Configuration parameter ``uuid``

A list of valid UUIDs of load models, the following configuration should be applied for.

#### Configuration parameter ``scaling``

Universal multiplication factor, that is applied to the models calculation results. It may be a positive real number.

#### Configuration parameter ``modelBehaviour``

Defining the model implementation to use:

- *"profile"*: It will set up a standard load profile model according to the ``standardLoadProfile`` field of the input model.
- *"fix"*: The model will provide a fixed active power consumption
- *"random"*: The model will randomly draw the active power consumption from sampled probability functions of household load measurements

#### Configuration parameter ``reference``

Gives information, to what input information the model output will be scaled. Everything is meant with reference to the active power consumption:

- *"power"*: The maximum active power consumption throughout a year will meet the rated active power of the model. In the case of ``modelBehaviour = "random"``, the 95 % quantile will meet the rated apparent power.
- *"energy"*: The annual energy consumption of the model will meet ``eConsAnnual`` of the input model

Applying configuration
----------------------

Two examples of proper load model configuration is given beneath.

The first example applies the same configuration to all available load models:

simona.runtime.participant.load = {
defaultConfig = {
uuid = ["default"]
scaling = 1.3
modelBehaviour = "profile"
reference = "power"
}
individualConfigs = []
}
If you want to have specific behaviour for only a sub set of the models, you may have something like:

simona.runtime.participant.load = {
defaultConfig = {
uuid = ["default"]
scaling = 1.3
modelBehaviour = "profile"
reference = "power"
}
individualConfigs = [
{
uuid = [
"49f250fa-41ff-4434-a083-79c98d260a76",
"9f90aa5f-409f-4214-9336-faede4ace939",
"db2cf9bb-0ba3-405a-9f39-e386d097b39e"
]
scaling = 1.3
modelBehaviour = "fix"
reference = "power"
},
{
uuid = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
scaling = 1.5
modelBehaviour = "random"
reference = "energy"
}
]
}
All models behave the same, except for those with the given uuids.
