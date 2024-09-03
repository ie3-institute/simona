(em)=

# Energy Management

Energy Management Agents (EmAgents) control power feed-in and load of system participants or other EmAgents in a hierarchical fashion.

## Protocol

During simulation, EmAgents send `FlexActivation` and `IssueFlexControl` messages and receive `ProvideFlexOptions` and `FlexCompletion` messages.
After having been requested to calculate flex options via `FlexActivation`, controllable assets send back their flex options to the controlling unit using `ProvideFlexOptions`. 
Eventually the controlling EmAgent responds with some type of `IssueFlexControl` messages, setting a power set point for operation.
The asset then tries to realize the set power as best as it can and replies with a `FlexCompletion` messages.
If an EmAgent is itself controlled by another EmAgent, it also behaves like a system participant (sends `FlexActivation` and `IssueFlexControl` messages etc.).


Every EmAgent aggregates flex options and power of its connected assets and disaggregates flex control among the connected assets. 
It also functions as a scheduler for all connected assets by processing information on the ticks of the next desired activations and conveying such information to the controlling EmAgent or a central scheduler respectively.

Uncontrolled EmAgents answer to a scheduler with regards to their activation.

![](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/em/UncontrolledEm.puml)

Controlled EmAgents are scheduled and activated by another EmAgent, which is thus placed higher up in hierarchy.

![](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/em/ControlledEm.puml)

## Model strategies

When disaggregating flex control, a multitude of strategies is conceivable.
An exemplary implemented strategy is the prioritized flex strategy, which prioritizes flexibility usage by asset type.
For each asset type, flexibility can only be used if such an asset is actually connected and able to provide the requested flexibility.
Otherwise, the next asset type is automatically considered.

An excess feed-in should be reduced by either decreasing feed-in or increasing load, first the charging power of connected EVs is increased, then connected battery storage is charged, then the heat pump might be turned on and finally renewables (PV and WEC) are curtailed.
If an excess load is detected, first connected battery storage is discharged, after which the batteries of connected EVs might be discharged, while finally a running heat pump might be turned off.
As stated above, asset types that are not connected are skipped.

![](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/main/em/PrioritizedFlexStrat.puml)
