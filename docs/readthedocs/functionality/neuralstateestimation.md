# Training data generation for neural state estimation

SIMONA facilitates the neural state estimation workflow for distribution grids by providing high-quality synthetic training data. This workflow enables the evaluation of neural models trained on simulation data and their transferability to real-world grid measurements {cite:p}`Oberließen.2025`.

The approach addresses challenges such as missing or erroneous measurement signals by incorporating anomaly detection and reconstruction. Furthermore, the methodology extends to stochastic state estimation, allowing for the quantification of uncertainty and the provision of estimation intervals {cite:p}`Oberließen.2025`.

## Workflow & SIMONA Integration

To perform neural state estimation, SIMONA serves as the simulation environment to generate the necessary data foundation:

1. **Data Generation:** Create synthetic grid states and corresponding measurement data via simulation.
2. **Training:** Train a neural model on the generated supervised dataset.
3. **Inference:** Apply the trained model to real measurement data to estimate non-observable grid states.
4. **Assessment:** Evaluate the model’s transferability, robustness against faulty measurements, and the quality of uncertainty-aware estimates {cite:p}`Oberließen.2025`.

By systematically varying operating conditions and measurement configurations, SIMONA provides a controlled, consistent basis for developing and analyzing robust neural estimators {cite:p}`Oberließen.2025`.
