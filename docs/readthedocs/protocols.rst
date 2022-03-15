Initialization phase
--------------------

.. image:: _static/figures/uml/InitializationPhase.png

Triggering of participant agents
--------------------------------

Participant triggered by primary data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This activation protocol is used, if the agent is foreseen to only play back externally given, precalculated model
results.

.. image:: _static/figures/uml/ParticipantTriggeredByPrimaryData.png

Participant triggered by secondary data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the usual activation protocol of a participant agent.
The participant agent asks the scheduler to trigger it (the participant agent) in the same tick in order to achieve
timely synchronization within the ready check window.

.. image:: _static/figures/uml/ParticipantTriggeredBySecondaryData.png

Participant triggered by itself
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This case only occurs, when the participant model itself does not rely on any other data.
This might be the case for a fixed feed in model as well as constant power load models.

.. image:: _static/figures/uml/ParticipantTriggeredByItself.png