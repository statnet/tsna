# `tsna`: Tools for Temporal Social Network Analysis

[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/tsna?color=2ED968)](https://cranlogs.r-pkg.org/)
[![cran version](https://www.r-pkg.org/badges/version/ergm)](https://cran.r-project.org/package=tsna)
[![Coverage status](https://codecov.io/gh/statnet/ergm/branch/master/graph/badge.svg)](https://codecov.io/github/statnet/tsna?branch=master)
[![R build status](https://github.com/statnet/tsna/workflows/R-CMD-check/badge.svg)](https://github.com/statnet/tsna/actions)

This package provides tools for exploring and describing longitudinal network data.  It works with temporal network data that is stored as a `networkDynamic` object -- essentially a list in the `network` format in which elements also have an attached activity attribute, a matrix of spells indicating when vertex or edge is active. See the [networkDynamic](https://cran.r-project.org/web/packages/networkDynamic/index.html) package for more information on translating longitudinal network data from other formats (timed edge lists, lists of toggles, sets of matrices, etc).

The `tsna` package contains functions for calculating a range of descriptive statistics from longitudinal network data.  Some of these are simple temporal extensions of static network descriptors, others are unique to longitudinal network data.

* Timeseries of standard 'static' social network analysis metrics
  
  These functions operate by collapsing the dynamic network into a static network at a series of regular intervals and returning the results as a time series ts object. They can provide general description of trends in a network dataset.

    - `tSnaStats` – descriptive statistics from the [sna](https://cran.r-project.org/web/packages/ergm/index.html) package. Includes both graph- and vertex-level measures such as centralities, components, reciprocity, betweenness, triad-census, etc.
    - `tErgmStats` – descriptive statistics for the "terms" used in statistical Exponential-family Random Graph Models (ERGMs) from the [ergm package](https://cran.r-project.org/web/packages/ergm/index.html)

* Temporal path based metrics
  
  These functions compute and use temporal paths (network geodesics that are constrained by the activity times of edges) through a network.

  - `tPath`
  - `tReach`

* Rates and Duration metrics
  
  These functions can be used to compute distributions of (observed) activity durations in a data structure. Note that due to censoring (edges that begin before or end after the time observation window for the network) the observed durations may be biased away from the 'real' values (or model parameters). The duration estimate functions use various types of survival analysis to return estimates of these values.

  - `edgeDuration`
  - `vertexDuration`
  - `tiedDuration`
  - `tEdgeDissolution`
  - `tEdgeFormation`
  - `tiedDuration`

* Utility functions
  
  These provide tools for data manipulation and plotting

  - `as.network.tPath`
  - `plotPaths`
  - `timeProjectedNetwork`

The `tsna` package is part of the [statnet](https://statnet.org/) software suite for network analysis written in **R**, and it is designed to work seamlessly with the other [packages](https://statnet.org/packages/) in this suite.  

The development of this software was supported by grant R01HD68395 from the National Institute of Health.

## Docs and examples

The [package vignette](https://cran.r-project.org/web/packages/tsna/vignettes/tsna_vignette.html) (`browseVignettes(package='tsna')`) provides examples and illustrations of key concepts.  

Examples of how `tsna` tools might be used in a network data analysis workflow can be found in our [Workshop materials](https://statnet.org/workshops/), in particular the [Temporal network tools](https://statnet.org/workshop-ndtv/) and [tergm](https://statnet.org/workshop-tergm/) workshops.


## License and attribution

This software is distributed under the GPL-3 license. It is free, open source, and has the attribution requirements (GPL Section 7) at http://statnet.org/attribution

To cite package ‘tsna’ in publications please use:
Bender-deMoll S, Morris M (2021). _tsna: Tools for Temporal Social Network Analysis_. R package version  0.3.5, <https://CRAN.R-project.org/package=tsna>.

## Code of Conduct

Developers and Contributing Users to the Statnet Project should read https://statnet.github.io/private/ for information about the relationship between the public and the private repository and the workflows involved.

Please note that the tsna project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

