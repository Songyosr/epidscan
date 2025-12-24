# Store Analysis Parameters in Result

Adds analysis parameters as attributes to the satscan_result object.
This is intended to be called internally by satscanr().

## Usage

``` r
attach_analysis_params(result, params)
```

## Arguments

- result:

  A `satscan_result` object.

- params:

  List of analysis parameters.

## Value

The result object with parameters attached.
