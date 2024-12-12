(measurement_control)=

# Transformer Control Groups

Transformer control group can be used to implement control functionalities like long-range control for active voltage
stability. For this purpose, network areas and transformers can be logically linked to a control group via measuring
points. If a deviation from the target voltage magnitude is detected at one of the measuring points, the transformer is
switched to the appropriate tap position to solve the deviation, provided that no limit values are violated at other
measuring points. This requires that only measuring points are included in control groups that can also be influenced by
the associated transformer.
