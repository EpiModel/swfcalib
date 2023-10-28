
<!-- README.md is generated from README.Rmd. Please edit that file -->

# swfcalib

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/slurmworkflow)](https://CRAN.R-project.org/package=slurmworkflow)
[![R-CMD-check](https://github.com/EpiModel/swfcalib/workflows/R-CMD-check/badge.svg)](https://github.com/EpiModel/swfcalib/actions)
[![codecov](https://codecov.io/gh/EpiModel/swfcalib/branch/main/graph/badge.svg?token=eo2r0HeP8Z)](https://codecov.io/gh/EpiModel/swfcalib)
<!-- badges: end -->

`swfcalib` is designed to automate the calibration of complex
multi-parameters multi-outputs models ran on High Performance Computing
systems (HPC) equipped with [Slurm](https://slurm.schedmd.com/).

## Specific problems it solves

`swfcalib` was build out of necessity to solve a specific set of
problems:

- expensive model runs
- very noisy outputs
- many parameters to calibrate
- many outputs to match
- knowledge about parameters and outputs relationship
- impossibility of running a pilot job (long running job)

By using [`slurmworkflow`](https://github.com/EpiModel/slurmworkflow),
`swfcalib` can implement a loop like behavior in
[Slurm](https://slurm.schedmd.com/) without the need of a pilot job
staying alive for the whole duration of the calibration process
(sometimes several weeks).

`swfcalib` was designed to calibrate epidemic models like [this
one](https://github.com/EpiModel/DoxyPEP). These models have around 20
parameters to calibrate to fit 20 outcomes to observed targets. However,
knowledge of the model allow us to define many conditional one to one
relationship between parameters and outcomes. `swfcalib` can use this
knowledge to split the calibration into multiple waves of simpler
parallel calibration jobs.

## Installation

You can install the development version of **swfcalib** like so:

``` r
remotes::install_github("EpiModel/swfcalib")
```

## Example

``` r
library(swfcalib)
```

## Design

Terminology:

- *model*: the model to be calibrated. It takes a *proposal* and return
  some *outcomes*
- *proposal*: a set of parameters values to be passed to the *model*
- *outcomes*: the output of a *model* run for a given *proposal*.
- *job*: a set of parameters to be calibrated by using a subset of the
  *outcomes*
- *wave*: a set of independent *jobs* that can be calibrated using the
  same run of the model.

The calibration process follows a typical proposal, validation loop.
Where the model is run with a set of parameters and new proposals are
made according to the results. This goes on until the model is fully
calibrated.

The specificity lies in the ability to try many proposal at once on an
HPC, and to set up this proposal validation loop without a long running
orchestrating job (pilot job).

The calibration is be split into *waves*. Each wave can contains
multiple *jobs*, each focusing on a set of parameters and related
outcomes. This permits the parallel calibration of multiple independent
parameters. At each *iteration*, the model is ran once per proposal, and
each job gather the outcomes it needs to make its next proposal. Once
all *jobs* in a *wave* are done, i.e. the parameters they govern are
calibrated, the system moves to the next *wave*. This allows the
sequential calibration of parameters when strong assumption about their
independence can be made.

This design was crafted for the very noisy models where many
replications are needed and where most parameters are independent or
conditionally independents.

## Versatility

`swfcalib` makes no assumptions on how a set of parameters should be
changed and how the quality of fit is assessed. It is up to the user to
provide the mechanism to: - produce the next *proposals* to be tested -
assess the quality of the fit
