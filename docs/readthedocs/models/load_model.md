(load_model)=
# Load Model Configuration and Load Profiles

SIMONA includes a polymorph approach for load model configuration. Based on parameterization the load model can be a profile model or a random model.

## Load Model

### General Information

SIMONA supports different load model behaviours, which need some parameterization made by this config entry. It can be found in the config tree at ```simona.runtime.participant.load```.

This sub configuration is two-fold:

Default Configuration
This applies to all load models, except for those, which already have an individual config assigned. To simplify code, no distinct model class is introduced for the single config. In order to highlight the fact, that this config is a default config, you may write ```uuids = ["default"]```. If you put anything else than this, you will get a warning, that those references are neglected, although the rest will work just fine.

Set of Individual Configurations
This part holds a set of configurations, that will apply to specific load models, denoted by their uuid. To simplify config generation, you are able to assign the same config to a list of uuids in batch. If one load has no individual configuration assigned, it will default to the above given config.

## Attributes, Units and Remarks

Please refer to {doc}`PowerSystemDataModel - Load Model <psdm:models/input/participant/load>` for Attributes and Units used in this Model.

### Configuration parameters

#### ``uuid``

A list of valid UUIDs of load models, the following configuration should be applied for.

#### ``scaling``

Universal factor that is applied to the models' relevant parameters such as maximum power or energy consumption. It may be a positive real number.

#### ``modelBehaviour``

Defining the model implementation to use:

- *"profile"*: It will set up a standard load profile model according to the ``standardLoadProfile`` field of the input model.
- *"fix"*: The model will provide a fixed active power consumption
- *"random"*: The model will randomly draw the active power consumption from sampled probability functions of household load measurements

#### ``reference``

Gives information, to what input information the model output will be scaled. Everything is meant with reference to the active power consumption:

- *"power"*: The maximum active power consumption throughout a year will meet the rated active power of the model. In the case of ``modelBehaviour = "random"``, the 95 % quantile will meet the rated apparent power.
- *"energy"*: The annual energy consumption of the model will meet ``eConsAnnual`` of the input model

### Applying configuration

Two examples of proper load model configuration is given beneath.

The first example applies the same configuration to all available load models:

```{code-block}
    simona.runtime.participant.load = {
        defaultConfig = {
            uuid = ["default"]
            scaling = 1.3
            modelBehaviour = "profile"
            reference = "power"
        }
        individualConfigs = []
    }
```

If you want to have specific behaviour for only a sub set of the models, you may have something like:

 ```{code-block}
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
```
All models behave the same, except for those with the given uuids.

(load_profiles)=
## Load Profiles

Load profiles model the annual power consumption of various consumer types in quarter-hour steps.

### Model

Load profiles are determined by three factors: consumer type, day type and season.


| Factor                            | Possible values                                                                                             |
| ----------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| Consumer type (Kundengruppe)      | H0 (**H**ousehold),**H**aushalt),<br>*G0* (Industry/**G**ewerbe),<br> *L0* (Agriculture/**L**andwirtschaft) |
| Season (Zeitzone)                 | *W* (Winter/**W**inter),<br> *U* (Transition/**Ü**bergangszeit),<br> *S* (Summer/**S**ommer)               |
| Day type (Charakteristischer Tag) | *We* (Weekday/**We**rktag),<br> *Sa* (Saturday/**S**amstag),<br> *So* (Sunday/**S**onntag)                  |

In total, 27 combinations have to be considered. A load profile type is represented as a *LoadProfileKey* in SIMONA, consisting of specific types for all three factors.

Each load profile type is assigned 96 values (one for each quarter-hour of the day).

Assignment of season and day type are described at page 4 of the source PDF.

**Leap years** do not have any significant influence on load profiles and thus do not receive any special treatment.

**Holidays** should normally be treated as Sundays, Christmas Eve and New Year\'s Eve as Saturdays. Holidays are currently not implemented for profile types in SIMONA.

#### Dynamization factor

Household type *H0* requires special treatment by multiplying a dynamization factor, which is dependent on the day of year *t*.

$$
F_t = -3.92 \cdot 10^{-10} \cdot t^4 + 3.2 \cdot 10^{-7}
\cdot t^3 - 7.2 \cdot 10^{-5} \cdot t^2 + 2.1 \cdot 10^{-3}
\cdot t + 1.24
$$

The factor $F_t$, after calculation, shall be rounded to four decimal places. After multiplication with the profile value for given quarter-hour, the result should again be rounded to one decimal place.

#### Maximum value

For each consumer type, a maximum value can be retrieved by calling LoadProfileStore.getMaxParam. The maximum value of consumer type *H0* has to be calculated considering dynamization function. For sake of simplicity, in SIMONA it is searched for only in winter profiles of *H0*, as we assume the maximum value to be exclusively found there.

#### Units

Although the primary source declares the profile values to be power in W (p. 14 and 19), we consider them to represent energy in Wh.

#### Considering annual consumption

Load profile values are normalized for an annual consumption of $1000 \frac{kWh}{a}$. For a realistic prediction, the actual annual consumption has to be considered. Given an annual consumption of 4711 kWh/a, each load profile output has to be multiplied with 4.711.

### Sources

Both model and data stem from [BDEW](https://www.bdew.de/energie/standardlastprofile-strom/).

The model in its entirety is described here: [Application Remarks](https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf)

The data sources are taken from [Profile.zip](https://www.bdew.de/media/documents/Profile.zip), which includes file *Repräsentative Profile VDEW.xls*. The tabs H0, G0 and L0 contain the values that make up Lastprofile.csv in the SIMONA project.
