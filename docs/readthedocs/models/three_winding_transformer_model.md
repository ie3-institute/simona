(three_winding_transformer_model)=
## Three winding transformer

### Transformer Calculations

#### Attributes, Units and Remarks

Input and Result Attributes and Units are defined at PowerSystemDataModel. Please refer to:

- Input: {doc}`PowerSystemDataModel - Three Winding Transformer Model <psdm:models/input/grid/transformer3w>`
- Result: {doc}`PowerSystemDataModel - Three Winding Transformer Model <psdm:models/result/grid/transformer3w>`

Equivalent circuit of the three winding transformer

![](../_static/figures/models/transformer_model/ModelTwt.png)

#### General Information

- In the data set, there is no need anymore to duplicate one of the nodes into subnets. Simply add all three nodes in their respective subnets.
- The temporary node is created automatically. The input model takes care of consistent naming.
- The temporary node can be included into database output. Refer the method addTransformers3W(LinkedList<Transformer3WInputModel> inputTrafos) of NetModel class.
- All three subnets contain a Transformer3WModel.
- Only the model of the highest voltage level (subnet A) creates an output model.
- The temporary transformer node is set as subnetGate, therefore only the respective branches are modeled in each subnet. Subnet A also includes the phase-to-ground elements mimeTeX and mimeTeX.
- All resistances, reactances, conductances and admittances are given in physical values with respect to the highest voltage level.
- Tap changer are assumed to be apparent only at the winding of the highest voltage level, therefore regulating both windings B and C equally.
