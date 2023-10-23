(HomeEnergyManagementSystem)

# Home Energy Management System

The Home Energy Management System takes care to follow implemented Control Schemes of agents in households with prosumer behaviour. It manages the behaviour of load, storage (home storage and EVs) as well as PV production for a certain household.

## Assumptions of implemented control schemes
### Optimize Self Consumption

If there is more energy produced at the household then there is demand the HEMS acts as follows:
Load < Production at Household
* Feed-in into grid should be minimized
* If an EV is connected, this will be charged till SoC of 100% with the lower one of both values is used: 
  * the rated power of Evcs/EV
  * the difference between PV-Production and LoadDemand of the household
* If there is a home storage connected it will be charged till SoC of 100% the lower one of both values is used:
  * the rated power of the home storage
  * DemandHomestorage = PV-Production - PowerDemandLoadAgent - PowerDemandEvcs
* If there is still remaining power this will be feed into the grid 

Load > Production at household
* Demand from grid should be minimized
* If an EV is connected which has charging demand, this will be charged till demand is satisfied with max Power.
* If there is a home storage this will be uncharged till SoC = 0% with minimum (RatedPowerHomestorage | |(PVInput - PowerDemandLoadAgent - PowerDemandEvcs)|)
* If there is still remaining power demand this will be covered from the grid
* If there is NO charging demand of an EV, but one is connected, the Home storage will be uncharged first, if power is not enough EV will be uncharged as well (V2H) till SoC of 60%.
* If there is still remaining power demand this will be covered from the grid
* 
* 0 != PowerDemandGridAgent + PvInput - PowerDemandLoadAgent - PowerDemandHomeStorage - PowerDemandEvcs

### Price Signal

## Time Steps

Further developments could be done on implementing the possibility to choose between updating the value store at a fixed interval (or at every power flow calculation) and the actual implementation with updating value store when triggered by the agents

<!-- Sequence Diagramm einfügen, Aktivierung der Kinder des HEMS durch HEMS, Trigger für HEMS (bei fester Zeitscheibe) oder Trigger in Zeitintervallen der Kinder (oder konfigurierbar?)) Aktivierung / Trigger des LoadAgent aktualisierung des ValueStore -->
<!-- Todo: HEMS in EMS ändern, Generischer Beschreiben um auch EMS in Substation zu repräsentieren -->
