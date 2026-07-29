# Get the default proposal of a calibration

The default proposal holds the current best value for every parameter
under calibration. It starts as `config$default_proposal` and is updated
as waves complete, so on a finished calibration it is the calibrated
parameter set.

## Usage

``` r
get_default_proposal(calib_object)
```

## Arguments

- calib_object:

  a formatted calibration object

## Value

a one-row `data.frame` of parameter values, one column per calibrated
parameter
