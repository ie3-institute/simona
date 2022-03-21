(protocols)=
### Initialization phase

![Initialization Phase Sequence Diagram](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/InitializationPhase.puml)

(protocolsTriggerParticipantAgent)=
### Triggering of participant agents


#### Participant triggered by primary data

This activation protocol is used, if the agent is foreseen to only play back externally given, precalculated model
results.

![Participant triggered by primary data Sequence Diagram](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/ParticipantTriggeredByPrimaryData.puml)

#### Participant triggered by secondary data

This is the usual activation protocol of a participant agent.
The participant agent asks the scheduler to trigger it (the participant agent) in the same tick in order to achieve
timely synchronization within the ready check window.

![Participant triggered by secondary data Diagram](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/ParticipantTriggeredBySecondaryData.puml)

#### Participant triggered by itself

This case only occurs, when the participant model itself does not rely on any other data.
This might be the case for a fixed feed in model as well as constant power load models.

![Participant triggered by itself Diagram](http://www.plantuml.com/plantuml/proxy?cache=no&src=https://raw.githubusercontent.com/ie3-institute/simona/dev/docs/uml/protocol/ParticipantTriggeredByItself.puml)
