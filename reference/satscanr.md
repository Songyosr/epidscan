# Run SaTScan Analysis

Runs a SaTScan session end-to-end: writes required SaTScan ASCII inputs
(`.cas`, `.geo`, and optionally `.pop`/`.ctl`/`.grd`), builds a final
`.prm` configuration using a strict precedence hierarchy, executes the
SaTScan command-line program, and parses outputs into an R result
object.

## Usage

``` r
satscanr(
  cas,
  pop = NULL,
  geo,
  ctl = NULL,
  grd = NULL,
  prm_path = NULL,
  prm = NULL,
  output_dir = NULL,
  keep_raw = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- cas:

  Case table (`satscan_table` of kind "cas") or `ss_tbl` of type "cas".

- pop:

  Population table (`satscan_table` of kind "pop", optional). Required
  for Poisson model. Created via
  [`prep_pop`](https://Songyosr.github.io/epidscan/reference/prep_pop.md).

- geo:

  Geometry table (`satscan_table` of kind "geo"). Created via
  [`prep_geo`](https://Songyosr.github.io/epidscan/reference/prep_geo.md).

- ctl:

  Control table (`satscan_table` of kind "ctl", optional). Required for
  Bernoulli model. Created via
  [`prep_ctl`](https://Songyosr.github.io/epidscan/reference/prep_ctl.md).

- grd:

  Grid table (`satscan_table` of kind "grd", optional). Custom scan
  centers. Created via
  [`prep_grd`](https://Songyosr.github.io/epidscan/reference/prep_grd.md).

- prm_path:

  Path to a template `.prm` file to load configuration from (Level 3).
  If NULL, uses bundled defaults.

- prm:

  Optional. A `prm_list` object or path to a parameter file. Parameters
  in this object/file act as the base configuration, which can be
  overridden by arguments in `...` or
  [`prm_options()`](https://Songyosr.github.io/epidscan/reference/prm_options.md).

- output_dir:

  Directory to copy final results to. If NULL, results remain in temp.

- keep_raw:

  Logical. Keep raw SaTScan output files.

- verbose:

  Logical. Print progress and debug info.

- ...:

  Additional SaTScan parameters (Level 2 Tweaks). See
  [`satscan_parameters`](https://Songyosr.github.io/epidscan/reference/satscan_parameters.md)
  for a full list. Common parameters include:

  - `AnalysisType`: 1=Purely Spatial, 2=Purely Temporal, 3=Retrospective
    Space-Time, 4=Prospective Space-Time, 5=Spatial Variation in
    Temporal Trends

  - `ModelType`: 0=Discrete Poisson, 1=Bernoulli, 2=Space-Time
    Permutation

  - `ScanAreas`: 1=High Rates, 2=Low Rates, 3=Both

  - `MonteCarloReps`: Number of Monte Carlo replications (e.g., 999,
    9999)

## Value

A `satscan_result` object containing:

- `cluster_summary`: Data frame of detected clusters with p-values and
  relative risks

- `location_summary`: Data frame of all locations with cluster
  assignments

- `main_results`: Full time-series data merged with results (if
  `merge_time_series=TRUE`)

- `raw_output`: Raw SaTScan output (col, gis, rr, shapefile, etc.)

- `prm`: Final `prm_list` object with all parameters used

- `work_dir`: Path to output directory

## Details

**What SaTScan does (short):** SaTScan detects and evaluates clusters by
scanning many candidate windows across space, time, or space-time. For
each candidate window, it computes a *probability-model-specific*
likelihood ratio (or analogous test statistic) comparing the risk
*inside* the window versus *outside*. The window with the maximum
statistic is reported as the **most likely cluster**. Statistical
significance is typically assessed by Monte Carlo simulation under the
null, repeating the same scan to obtain the null distribution of the
maximum statistic.

**Probability models:** The likelihood/test statistic depends on
`ModelType` (e.g., discrete Poisson, Bernoulli, space-time permutation,
multinomial, ordinal, exponential, normal). This function delegates all
inferential details to the SaTScan executable; this documentation only
summarizes the general workflow.

**Example (discrete Poisson; high-rate scanning):** Let \\C\\ be total
cases, \\c\\ observed cases inside a window, and \\E\\ the expected
cases inside that window under the null (possibly covariate-adjusted). A
commonly reported likelihood ratio form is proportional to: \$\$ \Lambda
= \left(\frac{c}{E}\right)^{c} \left(\frac{C-c}{C-E}\right)^{C-c} \cdot
\mathbb{I}(c \> E) \$\$ where \\\mathbb{I}(c \> E)\\ enforces the
"high-rate" constraint. (For low-rate or two-sided scanning, the
indicator rule differs; see the SaTScan User Guide.)

**Example (Bernoulli; high-rate scanning):** Let \\C\\ be total cases,
\\c\\ cases inside the window, \\N\\ total observations (cases +
controls), and \\n\\ observations inside the window. SaTScan uses an
analogous likelihood ratio based on the in-window vs out-of-window case
proportions.

**User guide:** For full details on model-specific likelihoods, scanning
windows, and parameter definitions, see the SaTScan technical
documentation (includes the User Guide PDF):
<https://www.satscan.org/techdoc.html>

## References

Kulldorff M. (1997). A spatial scan statistic. Communications in
Statistics: Theory and Methods, 26:1481-1496.

Kulldorff M. (2022). SaTScan User Guide for version 10.1.
<https://www.satscan.org/>

## See also

[`prep_cas`](https://Songyosr.github.io/epidscan/reference/prep_cas.md),
[`prep_geo`](https://Songyosr.github.io/epidscan/reference/prep_geo.md),
[`prep_pop`](https://Songyosr.github.io/epidscan/reference/prep_pop.md)
for input preparation.
[`satscan_parameters`](https://Songyosr.github.io/epidscan/reference/satscan_parameters.md)
for parameter reference.
[`prm_parse`](https://Songyosr.github.io/epidscan/reference/prm_parse.md),
[`prm_set`](https://Songyosr.github.io/epidscan/reference/prm_set.md)
for advanced parameter manipulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic Poisson analysis
cas <- prep_cas(cases_df, loc_id = id, time = date, cases = n, time_precision = "day")
geo <- prep_geo(locations_sf, loc_id = id)
pop <- prep_pop(pop_df, loc_id = id, time = year, pop = population)

result <- satscanr(cas, pop = pop, geo = geo, verbose = TRUE)

# View clusters
print(result)
summary(result)

# With custom parameters (Level 2 tweaks)
result2 <- satscanr(cas,
    pop = pop, geo = geo,
    AnalysisType = 3,
    MonteCarloReps = 9999,
    MaxTemporalSize = 50
)

# Using a template PRM file (Level 3)
result3 <- satscanr(cas,
    pop = pop, geo = geo,
    prm_path = "my_template.prm",
    verbose = TRUE
)
} # }
```
