# SaTScan Parameter Reference

This guide provides a comprehensive reference for all SaTScan parameters
available via the `epidscan` package. These parameters can be used in a
`.prm` template file or passed as direct overrides in
[`satscanr()`](https://Songyosr.github.io/epidscan/reference/satscanr.md).

## Input

These parameters define the data files required for SaTScan.

- **CaseFile**: Path to the case file. Contains location IDs, case
  counts, and optionally time and covariates.
- **ControlFile**: Path to the control file (required for Bernoulli
  model). Contains location IDs and control counts.
- **PopulationFile**: Path to the population file (required for Poisson
  model). Contains location IDs, time, population counts, and optional
  covariates.
- **CoordinatesFile**: Path to the coordinates file. Contains location
  IDs with X/Y or Lat/Long coordinates.
- **PrecisionCaseTimes**: Time precision of case data:
  - 0 = None (purely spatial analysis)
  - 1 = Year
  - 2 = Month
  - 3 = Day
  - 4 = Generic (arbitrary numeric units)
- **StartDate**: Study period start date (format: YYYY/MM/DD).
- **EndDate**: Study period end date (format: YYYY/MM/DD).
- **UseGridFile**: Use custom grid file instead of using case locations
  as centers? (y/n)
- **GridFile**: Path to the grid file. Optional file specifying
  alternative cluster center points.
- **CoordinatesType**: Coordinate system used:
  - 0 = Cartesian (X, Y in arbitrary units like meters)
  - 1 = Latitude/Longitude (geodetic, requires WGS84-like coordinates)

## Analysis

Core analysis settings including type and model.

- **AnalysisType**: Type of scan statistic:
  - 1 = Purely Spatial: Finds geographic clusters ignoring time.
  - 2 = Purely Temporal: Finds time clusters ignoring geography.
  - 3 = Retrospective Space-Time: Finds space-time clusters in
    historical data.
  - 4 = Prospective Space-Time: Surveillance mode, clusters must include
    the current time.
  - 5 = Spatial Variation in Temporal Trends: Finds areas with different
    time trends (Poisson only).
  - 6 = Prospective Purely Temporal: Temporal surveillance, cluster must
    end at current time.
  - 7 = Seasonal Temporal: Finds recurring seasonal patterns (Dec 31
    wraps to Jan 1).
- **ModelType**: Probability model:
  - 0 = Discrete Poisson: For count data with a known background
    population. Fast.
  - 1 = Bernoulli: For case/control data (0/1). Requires control file.
  - 2 = Space-Time Permutation: Case-only data, adjusts for purely
    spatial/temporal patterns.
  - 3 = Ordinal: For ordered categorical outcomes (e.g., cancer stage
    I/II/III).
  - 4 = Exponential: For survival time data (with censoring).
  - 5 = Normal: For continuous data (e.g., birth weight). Sensitive to
    outliers.
  - 6 = Continuous Poisson: For point process data where observations
    can occur anywhere.
  - 7 = Multinomial: For unordered categorical outcomes (e.g., disease
    subtypes).
  - 8 = Rank: Non-parametric rank-based model.
  - 9 = UniformTime: Uniform temporal model.
  - 10 = Batched: For batched data analysis.
- **ScanAreas**: Cluster types to detect:
  - 1 = High Rates (Poisson/Bernoulli/STP), High Values
    (Ordinal/Normal), Short Survival (Exponential)
  - 2 = Low Rates/Values, Long Survival
  - 3 = Both High and Low (recommended for unbiased analysis)
- **TimeAggregationUnits**: Units for aggregating time (0=None, 1=Year,
  2=Month, 3=Day, 4=Generic).
- **TimeAggregationLength**: Number of time units per aggregation period
  (positive integer).

## Output

Control which output files are generated.

- **ResultsFile**: Base filename for results (e.g., “analysis.txt”).
- **OutputGoogleEarthKML**: Generate Google Earth KML file? (y/n)
- **OutputShapefiles**: Generate ESRI shapefiles of clusters? (y/n)
- **OutputCartesianGraph**: Output cartesian graph file? (y/n)
- **MostLikelyClusterEachCentroidASCII**: Output cluster info in ASCII
  format? (y/n)
- **MostLikelyClusterEachCentroidDBase**: Output cluster info in dBase
  format? (y/n)
- **MostLikelyClusterCaseInfoEachCentroidASCII**: Output cluster case
  info in ASCII format? (y/n)
- **MostLikelyClusterCaseInfoEachCentroidDBase**: Output cluster case
  info (.col) in dBase format? (y/n)
- **CensusAreasReportedClustersASCII**: Output location info in ASCII
  format? (y/n)
- **CensusAreasReportedClustersDBase**: Output location info (.gis) in
  dBase format? (y/n)
- **IncludeRelativeRisksCensusAreasASCII**: Output risk estimates in
  ASCII format? (y/n)
- **IncludeRelativeRisksCensusAreasDBase**: Output risk estimates (.rr)
  in dBase format? (y/n)
- **SaveSimLLRsASCII**: Output simulated log likelihood ratios in ASCII
  format? (y/n)
- **SaveSimLLRsDBase**: Output simulated log likelihood ratios in dBase
  format? (y/n)
- **OutputGoogleMaps**: Generate Google Maps HTML output? (y/n)

## Multiple Data Sets

Settings for multivariate or covariate-adjusted analyses.

- **MultipleDataSetsPurposeType**: Purpose of multiple data sets:
  - 0 = Multivariate (analyze multiple outcomes)
  - 1 = Adjustment (covariate adjustment)
- **DataSet1-Name**: Name for data set 1 (default: “Data Set \#1”).

## Data Checking

Validation settings for input data.

- **StudyPeriodCheckType**: Date range checking:
  - 0 = Strict (error if data outside study period)
  - 1 = Relaxed (allow data outside study period)
- **GeographicalCoordinatesCheckType**: Coordinate validation:
  - 0 = Strict (error on invalid coordinates)
  - 1 = Relaxed (skip invalid coordinates)

## Locations Network

For network-based distance calculations.

- **LocationsNetworkFilename**: Path to network file.
- **UseLocationsNetworkFile**: Use network file for distance
  calculations? (y/n)

## Spatial Neighbors

For non-Euclidean spatial relationships.

- **UseNeighborsFile**: Use non-Euclidean neighbors file? (y/n)
- **NeighborsFilename**: Path to neighbors file.
- **UseMetaLocationsFile**: Use meta-locations (grouped locations)?
  (y/n)
- **MetaLocationsFilename**: Path to meta-locations file.
- **MultipleCoordinatesType**: How to handle multiple coordinates per
  location:
  - 0 = Only one location
  - 1 = At least one location
  - 2 = All locations
- **MultipleLocationsFile**: Filename for multiple locations per group.

## Spatial Window

Shape and size constraints on the spatial scanning window.

- **MaxSpatialSizeInPopulationAtRisk**: Maximum cluster size as percent
  of population at risk (0-50). Default: 50.
- **UseMaxCirclePopulationFileOption**: Restrict max spatial size using
  max circle population file? (y/n)
- **MaxSpatialSizeInMaxCirclePopulationFile**: Max spatial size in max
  circle population file (\<=50%).
- **MaxCirclePopulationFile**: Path to maximum circle population file.
- **UseDistanceFromCenterOption**: Restrict cluster size by distance?
  (y/n)
- **MaxSpatialSizeInDistanceFromCenter**: Maximum radius in distance
  units (positive integer).
- **IncludePurelyTemporal**: Include purely temporal clusters in
  space-time analysis? (y/n)
- **SpatialWindowShapeType**: Window shape:
  - 0 = Circular (default, fastest)
  - 1 = Elliptic (higher power for elongated clusters, slower)
- **NonCompactnessPenalty**: Penalty for non-compact ellipses:
  - 0 = No Penalty
  - 1 = Medium Penalty (default)
  - 2 = Strong Penalty
- **IsotonicScan**: Isotonic scan type:
  - 0 = Standard
  - 1 = Monotone

## Temporal Window

Size constraints on the temporal scanning window.

- **MinimumTemporalClusterSize**: Minimum cluster duration in time
  aggregation units.
- **MaxTemporalSizeInterpretation**: How to interpret max temporal size:
  - 0 = Percentage of study period
  - 1 = Absolute time units
- **MaxTemporalSize**: Maximum temporal cluster size (0-90 percent, or
  time units).
- **IncludePurelySpatial**: Include purely spatial clusters in
  space-time analysis? (y/n)
- **IncludeClusters**: Which temporal clusters to evaluate:
  - 0 = All clusters
  - 1 = Alive clusters (ending at study end)
  - 2 = Flexible window (specify ranges below)
- **IntervalStartRange**: Start date range for flexible window
  (YYYY/MM/DD,YYYY/MM/DD).
- **IntervalEndRange**: End date range for flexible window
  (YYYY/MM/DD,YYYY/MM/DD).

## Cluster Restrictions

Minimum requirements for reporting clusters.

- **RiskLimitHighClusters**: Apply risk threshold for high clusters?
  (y/n)
- **RiskThresholdHighClusters**: Minimum relative risk for high clusters
  (\>=1.0).
- **RiskLimitLowClusters**: Apply risk threshold for low clusters? (y/n)
- **RiskThresholdLowClusters**: Maximum relative risk for low clusters
  (0-1).
- **MinimumCasesInLowRateClusters**: Minimum cases required to report a
  low-rate cluster.
- **MinimumCasesInHighRateClusters**: Minimum cases required to report a
  high-rate cluster.

## Space and Time Adjustments

Controls for confounding and trend adjustment.

- **TimeTrendAdjustmentType**: Time trend adjustment:
  - 0 = None
  - 2 = Log-linear with user-specified percentage
  - 3 = Calculated log-linear
  - 4 = Time-stratified randomization
  - 5 = Calculated quadratic
- **TimeTrendPercentage**: Adjustment percentage (for type 2). Can be
  negative (\>-100).
- **TimeStratifiedAdjLength**: Length in time units for stratified
  adjustment (positive integer).
- **TimeTrendType**: Time trend type for SVTT only:
  - 0 = Linear
  - 1 = Quadratic
- **AdjustForWeeklyTrends**: Adjust for day-of-week effects
  (nonparametric)? (y/n)
- **SpatialAdjustmentType**: Spatial adjustment:
  - 0 = None
  - 1 = Spatially-stratified randomization
  - 2 = Spatial nonparametric
- **UseAdjustmentsByRRFile**: Use adjustments by known relative risks
  file? (y/n)
- **AdjustmentsByKnownRelativeRisksFilename**: Path to known relative
  risks file.

## Inference

Monte Carlo simulation and p-value calculation.

- **PValueReportType**: P-value calculation method:
  - 0 = Default p-value
  - 1 = Standard Monte Carlo
  - 2 = Early Termination (faster for non-significant clusters)
  - 3 = Gumbel p-value
- **EarlyTerminationThreshold**: Threshold for early termination
  (default: 50).
- **ReportGumbel**: Report Gumbel p-values? (y/n)
- **MonteCarloReps**: Number of Monte Carlo replications (0, 9, 999,
  n999). More = more precise p-values.
- **AdjustForEarlierAnalyses**: Adjust for multiple testing across
  repeated prospective analyses? (y/n)
- **ProspectiveStartDate**: For prospective analyses, the start date of
  surveillance (YYYY/MM/DD).
- **IterativeScan**: Remove detected clusters and re-scan? (y/n)
- **IterativeScanMaxIterations**: Maximum iterations for iterative scan
  (0-32000).
- **IterativeScanMaxPValue**: Stop iterative scan when cluster p-value
  exceeds this (0.0-1.0).

## Cluster Drilldown

Settings for drilling down into detected clusters.

- **PerformStandardDrilldown**: Perform standard drilldown on detected
  clusters? (y/n)
- **PerformBernoulliDrilldown**: Perform Bernoulli drilldown on detected
  clusters? (y/n)
- **DrilldownMinimumClusterLocations**: Minimum locations in cluster to
  perform drilldown (positive integer).
- **DrilldownMinimumClusterCases**: Minimum cases in cluster to perform
  drilldown (positive integer).
- **DrilldownClusterCutoff**: P-value cutoff for drilldown (0.0-1.0 for
  retrospective, \>0 for prospective).
- **DrilldownAdjustForWeeklyTrends**: Adjust for weekly trends in purely
  spatial Bernoulli drilldown? (y/n)

## Miscellaneous Analysis

Additional analysis options.

- **CalculateOliveira**: Calculate Oliveira’s F statistic? (y/n)
- **NumBootstrapReplications**: Number of bootstrap replications for
  Oliveira calculation (minimum=100, multiple of 100).
- **OliveiraPvalueCutoff**: P-value cutoff for clusters in Oliveira
  calculation (0.0-1.0).
- **ProspectiveFrequencyType**: Frequency type for prospective analyses:
  - 0 = Same as time aggregation
  - 1 = Daily
  - 2 = Weekly
  - 3 = Monthly
  - 4 = Quarterly
  - 5 = Yearly
- **ProspectiveFrequency**: Frequency of prospective analyses (positive
  integer).

## Power Evaluation

Estimate statistical power of the analysis.

- **PerformPowerEvaluation**: Run power analysis? (y/n, Poisson only)
- **PowerEvaluationsMethod**: Power evaluation method:
  - 0 = Analysis and power evaluation together
  - 1 = Power evaluation with case file only
  - 2 = Power evaluation with defined total cases
- **PowerEvaluationTotalCases**: Total cases for power calculation.
- **CriticalValueType**: Critical value type:
  - 0 = Monte Carlo
  - 1 = Gumbel
  - 2 = User Specified Values
- **CriticalValue05**: Critical value for p=0.05 (\>0).
- **CriticalValue01**: Critical value for p=0.01 (\>0).
- **CriticalValue001**: Critical value for p=0.001 (\>0).
- **PowerEstimationType**: Power estimation type:
  - 0 = Monte Carlo
  - 1 = Gumbel
- **NumberPowerReplications**: Number of simulations for power
  estimation.
- **AlternativeHypothesisFilename**: File defining alternative
  hypothesis for power.
- **PowerEvaluationsSimulationMethod**: Simulation method for power
  step:
  - 0 = Null Randomization
  - 2 = File Import
- **PowerEvaluationsSimulationSourceFilename**: Input file for power
  simulation.
- **ReportPowerEvaluationSimulationData**: Report power evaluation
  simulation data? (y/n)
- **PowerEvaluationsSimulationOutputFilename**: Output file for power
  simulation data.

## Spatial Output

Settings for spatial output and cluster reporting.

- **LaunchMapViewer**: Automatically launch map viewer (GUI only)? (y/n)
- **CompressKMLtoKMZ**: Create compressed KMZ file instead of KML? (y/n)
- **IncludeClusterLocationsKML**: Include cluster locations in KML
  output? (y/n)
- **ThresholdLocationsSeparateKML**: Threshold for generating separate
  KML files for cluster locations (positive integer).
- **ReportHierarchicalClusters**: Report hierarchical structure of
  overlapping clusters? (y/n)
- **CriteriaForReportingSecondaryClusters**: Overlap criteria for
  secondary clusters:
  - 0 = No Geographic Overlap (most restrictive)
  - 1 = No Centers in Other Cluster
  - 2 = No Centers in Most Likely Cluster
  - 3 = No Centers in Less Likely Cluster
  - 4 = No Pair of Centers in Each Other
  - 5 = No Restrictions (report all)
- **ReportGiniClusters**: Report Gini clusters? (y/n)
- **GiniIndexClusterReportingType**: Gini cluster reporting:
  - 0 = Optimal index only
  - 1 = All values
- **SpatialMaxima**: Spatial window maxima stops, comma-separated
  percentages (e.g., “1,2,3,4,5,6,8,10,12,15,20,25,30,40,50”).
- **GiniIndexClustersPValueCutOff**: Max p-value for clusters used in
  Gini coefficient calculation (0.0-1.0).
- **ReportGiniIndexCoefficents**: Report Gini index coefficients to
  results file? (y/n)
- **UseReportOnlySmallerClusters**: Restrict reported clusters to
  maximum geographical size? (y/n)
- **MaxSpatialSizeInPopulationAtRisk_Reported**: Maximum reported
  spatial size in population at risk (\<=50%).
- **UseMaxCirclePopulationFileOption_Reported**: Restrict max reported
  size using max circle file? (y/n)
- **MaxSizeInMaxCirclePopulationFile_Reported**: Max reported size in
  max circle population file (\<=50%).
- **UseDistanceFromCenterOption_Reported**: Restrict max reported size
  by distance? (y/n)
- **MaxSpatialSizeInDistanceFromCenter_Reported**: Maximum reported
  spatial size in distance (positive integer).

## Temporal Output

Settings for temporal graph output.

- **OutputTemporalGraphHTML**: Output temporal graph HTML file? (y/n)
- **TemporalGraphReportType**: Temporal graph cluster reporting:
  - 0 = Only most likely cluster
  - 1 = X most likely clusters
  - 2 = Only significant clusters
- **TemporalGraphMostMLC**: Number of most likely clusters to report in
  temporal graph (positive integer).
- **TemporalGraphSignificanceCutoff**: P-value cutoff for significant
  clusters in temporal graph (0.0-1.0).

## Other Output

Additional output settings.

- **CriticalValue**: Report critical values for 0.01 and 0.05? (y/n)
- **ReportClusterRank**: Report cluster rank? (y/n)
- **PrintAsciiColumnHeaders**: Print ASCII column headers in output
  files? (y/n)
- **ResultsTitle**: User-defined title for results file.
- **CutoffClusterLinelistCSV**: P-value cutoff for clusters in line list
  CSV (0.0-1.0 for retrospective, \>0 for prospective).

## Line List

Settings for line list caching.

- **LineListIndividualCache**: Filename for line list individuals cache.

## Notifications

Email notification settings.

- **EmailAlwaysSummary**: Always email results summary? (y/n)
- **EmailAlwaysRecipients**: Recipients for always emails
  (comma-separated list).
- **EmailCutoffSummary**: Email results summary per cluster meeting
  cutoff? (y/n)
- **EmailCutoffRecipients**: Recipients for cutoff emails
  (comma-separated list).
- **EmailCutoffValue**: P-value cutoff to email results (0.0-1.0 for
  retrospective, \>0 for prospective).
- **EmailAttachResults**: Attach results to email? (y/n)
- **EmailIncludeResults**: Include results/directory path in email?
  (y/n)
- **CustomEmail**: Use custom email format? (y/n)
- **CustomEmailSubjectLine**: Custom email subject line (use
  `<results-name>` placeholder).
- **CustomEmailMessageBody**: Custom email message body (use
  `<summary-paragraph>`, `<location-paragraph>`, `<footer-paragraph>`,
  `<linebreak>` placeholders).

## Elliptic Scan

Settings when using elliptic scanning window (SpatialWindowShapeType=1).

- **EllipseShapes**: Ellipse shape ratios, comma-separated (e.g.,
  “1.5,2,3,4,5”).
- **EllipseAngles**: Number of angles per ellipse, comma-separated
  (e.g., “4,6,9,12,15”).

## Power Simulations

Settings for simulation data in power analysis.

- **SimulatedDataMethodType**: Simulation method:
  - 0 = Null Randomization
  - 2 = File Import
- **SimulatedDataInputFilename**: Input file for simulated data (with
  File Import=2).
- **PrintSimulatedDataToFile**: Print simulated data to file? (y/n)
- **SimulatedDataOutputFilename**: Output filename for simulated data.

## Run Options

Performance and execution settings.

- **NumberParallelProcesses**: Number of parallel processes (0 = all
  available CPUs).
- **SuppressWarnings**: Suppress warning messages? (y/n)
- **LogRunToHistoryFile**: Log run to history file? (y/n)
- **ExecutionType**: Execution method:
  - 0 = Automatic
  - 1 = Successively
  - 2 = Centrically

## System

- **Version**: SaTScan version (read-only, do not modify).

## References

- Kulldorff M. A spatial scan statistic. Communications in Statistics:
  Theory and Methods. 1997;26:1481-1496.
- Kulldorff M, et al. A space-time permutation scan statistic for
  disease outbreak detection. PLoS Medicine. 2005;2:e59.
