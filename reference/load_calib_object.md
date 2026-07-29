# Load a calibration object from disk

Reads the `calib_object.rds` stored under the calibration's root
directory. If nothing has been saved yet, the state of `calib_object` is
initialized, its directories are created on disk, and that freshly
initialized object is returned. Every calibration step calls this first,
and it is also how a downstream package reads a finished calibration
back.

## Usage

``` r
load_calib_object(calib_object)
```

## Arguments

- calib_object:

  a formatted calibration object

## Value

the calibration object stored on disk, or a newly initialized one when
no saved object exists
