(features)=

# Features

This page gives an overview of some additional features in SIMONA.

## Congestion management

The congestion management is an optional feature of SIMONA, that is run after a power flow calculation. It can be used
for example to mitigate voltage congestions by tapping transformers.

The procedures of the congestion management is as follows:

![Procedure of the congestion management](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/main/CongestionManagement.puml)

The congestion management consists of the steps `Transformer tapping`, `Topology change` and `Using flexibilities`. By
default all steps are disabled. If the congestion management is enabled without enabling any step, only the congestion
detection is run. It can be configured, which steps the congestion management can use. After each step a simulation is
run to incorporate the changes.
