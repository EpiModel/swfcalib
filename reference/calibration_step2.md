# Second calibration step: run the model for each proposal

Second calibration step: run the model for each proposal

## Usage

``` r
calibration_step2(
  calib_object,
  n_cores,
  batch_num,
  n_batches,
  future_use_plan = NULL
)
```

## Arguments

- calib_object:

  a formatted calibration object

- n_cores:

  number of cores to run the processing on

- batch_num:

  the batch number for the current proposal

- n_batches:

  the total number of batches for this step

- future_use_plan:

  If `NULL`, `multisession` is used with
  `workers = ncores for its parallelization. Otherwise, it can take the output of a `future::tweak()\`
  call to setup a user defined temporary plan
