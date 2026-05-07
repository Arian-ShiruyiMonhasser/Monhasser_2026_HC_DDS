# HC_DDS

Hydraulic conductivity dynamic discrete states (HC_DDS) model implementation in R.

This repository contains the numerical implementation and post-processing workflow developed for:

> Hinz, C., Monhasser, A., and Wachsmuth, G.  
> "Event-based changes in hydraulic properties of surface soils in semi-arid regions may generate surface runoff regimes at decadal timescales"

## Repository structure

### `DDS.R`
Main numerical model and stochastic weather generator as described in the theory section of the paper. The stochastic weather input parameters involve trigonometric functions to impose the effect of seasonality over the exponential distribution of rainfall characteristics and potential evaporation. Karl Kästner implemented the stochastic rainfall input using a Markov chain framework.

The numerical solver uses explicit Euler integration for both soil moisture and hydraulic state evolution. The main outputs are therefore soil moisture (`s`) and hydraulic property (`α`). Fluxes, alongside the timestamps of threshold crossings, are also stored for consistency checks.

### `Plot_Generator.R`
Generates figures used for soil moisture dynamics, water balance, and flux balance analysis.

### `DTW_HierarchicalClustering.R`
Implements dynamic time warping and hierarchical clustering of runoff regimes. A brief evaluation of cluster validity indices (CVI) is also performed to determine suitable cluster numbers.

### `LDA_Parameterization.R`
Performs linear discriminant analysis (LDA) and generates the corresponding figures for parameter-space interpretation of clustered simulations.

## Main outputs

Simulation environments are stored as `.RData` files containing:
- soil moisture (`s`)
- hydraulic state (`α`)
- fluxes
- threshold crossing timestamps

## Dependencies

Required R packages:
- ggplot2
- patchwork
- dtwclust
- MASS
- scico

## References

[1] H. Wickham. *ggplot2: Elegant Graphics for Data Analysis*. Springer-Verlag New York, 2016.

[2] Pedersen T (2024). *patchwork: The Composer of Plots*. R package version 1.3.0, https://github.com/thomasp85/patchwork, <https://patchwork.data-imaginist.com>.

[3] Sarda-Espinosa A (2024). *dtwclust: Time Series Clustering Along with Optimizations for the Dynamic Time Warping Distance*. R package version 6.0.0, <https://github.com/asardaes/dtwclust>.

[4] Venables, W. N. & Ripley, B. D. (2002). *Modern Applied Statistics with S*. Fourth Edition. Springer, New York. ISBN 0-387-95457-0.

[5] Pedersen T, Crameri F (2023). *scico: Colour Palettes Based on the Scientific Colour-Maps*. R package version 1.5.0, <https://github.com/thomasp85/scico>.
