# Add a New Parameter to PRM

Adds a new parameter to a prm_list. Creates the section if it doesn't
exist. Errors if the parameter already exists (use `prm_set` for that).

## Usage

``` r
prm_add(prm, key, value, section, info = NULL)
```

## Arguments

- prm:

  A `prm_list` object.

- key:

  Parameter name (character).

- value:

  Parameter value.

- section:

  Section name without brackets (e.g., "Custom Section").

- info:

  Optional comment line (without leading semicolon).

## Value

Expanded `prm_list` with updated skeleton.
