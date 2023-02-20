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

![Basic Simona](images/usersguide/basic_simona_environment.png "SIMONA System Overview")

## Simulation
SIMONA is designed to simulate the assets' behaviour with each other and with the grid itself. In this process, SIMONA
uses an individuality approach, meaning that the simulation is focussed on the individual behaviour of the assets rather
than the entire grid. 

Since SIMONA uses an agent-based approach, the assets are modelled as agents. There are system participant agents (SPA),
which represent generators or loads commonly used in distribution grids (PV, Wind Turbine, EV, Storages, ...). In 
addition to that, a grid agent (GA) manages interactions between subgrids and with the subordinate SPAs.

![SIMONA Agent Message](images/usersguide/agent_messages.png)

## Further Information and Contact Details

Please, visit the SIMONA [website](https://simona.ie3.e-technik.tu-dortmund.de) for further Information. There you will
also find the current developers' contact information.
