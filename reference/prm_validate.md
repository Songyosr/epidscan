# Validate PRM Parameters

Validates a parsed PRM against a known version template. Useful for
checking external PRM files before use.

## Usage

``` r
prm_validate(prm, version = NULL)
```

## Arguments

- prm:

  A `prm_list` object (from prm_parse).

- version:

  Version to compare against (e.g., "10.3"). If NULL, uses the prm's
  version attribute or defaults to "10.3".

## Value

A list with:

- `valid`: TRUE if no critical issues found

- `missing`: Character vector of keys in reference but not in prm

- `extra`: Character vector of keys in prm but not in reference

- `prm_version`: Version found in prm (or NULL)

- `ref_version`: Reference version used for comparison
