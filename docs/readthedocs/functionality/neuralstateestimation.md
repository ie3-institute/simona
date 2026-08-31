# Training data generation for neural state estimation

SIMONA facilitates the neural state estimation workflow for distribution grids by providing high-quality synthetic training data. This workflow enables the evaluation of neural models trained on simulation data and their transferability to real-world grid measurements {cite:p}`Oberließen.2025`.

SIMONA itself does not pursue the state estimation approach. It provides the simulated data basis that can be used in such workflows.

The approach addresses challenges such as missing or erroneous measurement signals by incorporating anomaly detection and reconstruction. Furthermore, the methodology extends to stochastic state estimation, allowing for the quantification of uncertainty and the provision of estimation intervals {cite:p}`Oberließen.2025`.

## Workflow & SIMONA Integration

To generate training data, e.g., for neural state estimation, SIMONA serves as the simulation environment to generate the necessary data foundation:

1. **Data Generation:** SIMONA creates synthetic grid states and corresponding measurement data under configurable operating conditions and measurement configurations.
2. **Downstream Use:** The generated supervised datasets can subsequently be used by external tools or workflows to train, test, and evaluate neural state estimation models.

By systematically varying operating conditions and measurement configurations, SIMONA can be used to provide a controlled, consistent basis for developing and analyzing robust neural estimators {cite:p}`Oberließen.2025`.
