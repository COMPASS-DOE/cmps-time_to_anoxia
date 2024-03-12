# Time to Anoxia
[![DOI](https://zenodo.org/badge/513304849.svg)](https://zenodo.org/doi/10.5281/zenodo.10810574)

Kaizad F. Patel et al.

This repository contains data and code associated with Patel et al. 2024 _Time to anoxia: Observations and predictions of oxygen drawdown following coastal flood events_ (https://doi.org/10.1016/j.geoderma.2024.116854).

---

## Abstract
The coastal terrestrial-aquatic interface (TAI) is a highly dynamic system characterized by strong physical, chemical, and biological gradients. In particular, shifting soil redox conditions and consumption of terminal electron acceptors, due in part to dynamic hydrologic conditions, is a strong driver of carbon availability and transformations across TAIs. However, while redox dynamics are well described, our ability to quantitatively forecast rates of oxic to anoxic shifts in soils with different characteristics and inundation regimes is limited. We integrated field measurements, laboratory incubations, and model simulations to improve mechanistic understanding of oxygen consumption dynamics in coastal soils. Continuous in situ monitoring unexpectedly revealed that flooding caused temporary spikes in subsurface dissolved oxygen followed by rapid consumption in the wetlands. To further investigate these mechanisms in a controlled setting, we performed laboratory incubations using surface and subsurface soils from a TAI gradient (defined here as upland forest to transitional forest to wetland) in Western Lake Erie to measure oxygen consumption rates in TAI soils during flood events. In our experiments, wetland soils reached anoxia the fastest, in ∼ 9 h on average, whereas upland soils turned anoxic in ∼ 18 h. Subsurface upland soils did not turn anoxic even after two weeks of saturation in the lab, and their oxygen consumption patterns suggested carbon and/or nutrient limitation. These results are consistent with in-situ groundwater redox and oxygen measurements in the field, where wetland soils exhibited the highest rates of oxygen consumption along the TAI. Model simulations of oxygen consumption suggested that oxygen consumption had stronger abiotic controls in wetland soils but stronger biotic controls in upland soils, providing a useful framework for future incubation experiments. Microbial activity is a strong driver of oxygen consumption in TAI soils, although it is constrained by the availability of dissolved carbon in subsurface soils.

## Repository structure
We used the `{targets}` package for reproducible analytical workflows ([Landau 2021](https://doi.org/10.21105/joss.02959)). The workflow contains targets stored in `_targets.R`, which is run though `run.R`, both in the parent directory. The `_targets.R` file pulls data from `1-data/` and functions from `2-code/`, and the final graphs are rendered in the `3-reports/` subdirectory. 

## Funding
This work was supported through the Field, Measurements, and Experiments (FME) component of the Coastal Observations, Mechanisms, and Predictions Across Systems and Scales (COMPASS) program (https://compass.pnnl.gov/). COMPASS-FME is a multi-institutional project supported by the US Department of Energy, Office of Science, Biological and Environmental Research as part of the Environmental System Science Program.
