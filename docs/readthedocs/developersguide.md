(developersguide)=

# Developer’s Guide

## Repositories

The SIMONA repository on github [is here](https://github.com/ie3-institute/simona).

The convention for merging into the developer branch is that it needs to be pass all tests and at least two other active SIMONA developer needs to review your changes before merging. Please do this by creating a pull request from any new feature branches into dev. We also encourage you to create pull requests early in your development cycle which gives other’s an opportunity to observe and/or provide feedback in real time. When you are ready for a review, invite one or more through the pull request.

Please use the following naming convention for feature branches, “&lt;initials-or-username&lt;/#&lt;issue-number&lt;-&lt;descriptive-feature-branch-name&lt;”. E.g.: ab/#issue123-update-docs


## Models

This page gives an overview of the available models in *SIMONA*.

### Grid Related Models

```{eval-rst}
.. toctree::
   :maxdepth: 1

   models/line_model
   models/switch_model
   models/two_winding_transformer_model
   models/three_winding_transformer_model
```

### System Participant Related Models

```{eval-rst}
.. toctree::
   :maxdepth: 1

   models/bm_model
   models/chp_model
   models/cts_model
   models/evcs_model
   models/load_model
   models/pv_model
   models/wec_model
```

## Protocols

```{eval-rst}
.. toctree::
   :maxdepth: 1

   protocols
   protocolsTriggerParticipantAgent
```