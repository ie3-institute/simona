(about)=
# About SIMONA

SIMONA is a simulation tool for generating time series in the context of distribution grids to be used in grid planning, 
operation and analysis. The simulation is executed using an agent-based, discrete-event model. SIMONA is being developed
at the [Institute of Energy Systems, Energy Efficiency and Energy Economics](https://ie3.etit.tu-dortmund.de) at
[TU Dortmund University](https://www.tu-dortmund.de), Germany.

## Power Grid Model
With SIMONA, you can model your power grid using a broad variety of different available asset types. The input data for
the grid must be provided in the PowerSystemDataModel format. (For more information on this data model, please visit the
{doc}`PowerSystemDataModel <psdm:index>` documentation.) The combination of implemented
physical participant models and your provided input data forms a power grid that can be further investigated and 
analysed using ancillary tools.

![SIMONA System Overview](images/usersguide/basic_simona_environment.png)

## Simulation
SIMONA is designed to simulate the assets' behaviour with each other and with the grid itself. In this process, SIMONA
uses an individuality approach, meaning that the simulation is focussed on the individual behaviour of the assets rather
than the entire grid. 

Since SIMONA uses an agent-based approach, the assets are modelled as agents. There are system participant agents (SPA),
which represent generators or loads commonly used in distribution grids (PV, Wind Turbine, EV, Storages, ...). In 
addition to that, a grid agent (GA) manages interactions between subgrids and with the subordinate SPAs.

![SIMONA Message Protocol Overview](images/usersguide/agent_messages.png)

## Use Cases
SIMONA can be applied to a range of research and development tasks related to electrical distribution systems. Typical application areas include:

- **Distribution Grid Planning:**  
  Generation of grid utilisation time series for future scenarios. These results can be used to analyse hosting capacity, evaluate grid reinforcement measures, and assess the impact of new technologies.

- **Grid Operation and Congestion Analysis:**  
  Investigation of operational challenges in distribution grids with high shares of distributed energy resources. Possible mitigation measures such as coordinated control strategies or transformer tap changes are   described in the {doc}`Functionality <functionality>` section.

- **Energy Management and Flexibility Utilisation:**  
  Evaluation of energy management strategies and the coordinated utilisation of flexibility from distributed energy resources such as batteries, controllable loads, and distributed generation.

- **Integration of Renewable Energy Sources:**  
  Analysis of the impact of increasing renewable generation on voltage levels, line loading, and transformer utilisation in distribution grids.

- **Electric Vehicle and Sector Coupling Analysis:**  
  Coupling with models from other domains, such as mobility simulations, enables the analysis of electric vehicle charging demand and its impact on grid operation.

- **Synthetic Data Generation for Data‑Driven Methods:**  
  Generation of physically consistent synthetic grid operation data that can be used to develop and validate data‑driven approaches, for example machine‑learning‑based state estimation.

## Further Information and Contact Details

Please, visit the SIMONA [website](https://simona.ie3.e-technik.tu-dortmund.de) for further Information. There you will
also find the current developers' contact information.
