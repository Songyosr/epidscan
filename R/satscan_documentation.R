#' SaTScan Parameter Reference
#'
#' A comprehensive reference for all SaTScan parameters available via \code{rsatscan}.
#' Use these parameters in a \code{.prm} template file or as overrides in \code{satscanr(...)}.
#'
#' @details
#' SaTScan is a free software for spatial, temporal and space-time scan statistics,
#' developed by Martin Kulldorff. It can detect clusters and evaluate their statistical significance.
#'
#' For full methodology and examples, see the \href{http://www.satscan.org/}{SaTScan website}.
#'
#' \strong{How to Use These Parameters:}
#' \enumerate{
#'   \item \strong{Template File}: Create a \code{.prm} file and pass to \code{satscanr(prm_path = "your.prm")}.
#'   \item \strong{Direct Override}: Pass as named arguments: \code{satscanr(..., AnalysisType = 1)}.
#' }
#'
#' @section Input Files:
#' These parameters define the data files required for SaTScan.
#' \describe{
#'   \item{CaseFile}{Path to the case file. Contains location IDs, case counts, and optionally time and covariates.}
#'   \item{ControlFile}{Path to the control file (required for Bernoulli model). Contains location IDs and control counts.}
#'   \item{PopulationFile}{Path to the population file (required for Poisson model). Contains location IDs, time, population counts, and optional covariates.}
#'   \item{CoordinatesFile}{Path to the coordinates file. Contains location IDs with X/Y or Lat/Long coordinates.}
#'   \item{GridFile}{Path to the grid file. Optional file specifying alternative cluster center points.}
#'   \item{UseGridFile}{Use custom grid file instead of using case locations as centers? (y/n)}
#' }
#'
#' @section Time Settings:
#' These parameters define how time is handled in the analysis.
#' \describe{
#'   \item{PrecisionCaseTimes}{Time precision of case data:
#'     \itemize{
#'       \item 0 = None (purely spatial analysis)
#'       \item 1 = Year
#'       \item 2 = Month
#'       \item 3 = Day
#'       \item 4 = Generic (arbitrary numeric units)
#'     }
#'   }
#'   \item{StartDate}{Study period start date (format: YYYY/MM/DD).}
#'   \item{EndDate}{Study period end date (format: YYYY/MM/DD).}
#'   \item{TimeAggregationUnits}{Units for aggregating time:
#'     \itemize{
#'       \item 0 = None
#'       \item 1 = Year
#'       \item 2 = Month
#'       \item 3 = Day
#'       \item 4 = Generic
#'     }
#'   }
#'   \item{TimeAggregationLength}{Number of time units per aggregation period (positive integer).}
#' }
#'
#' @section Coordinate System:
#' \describe{
#'   \item{CoordinatesType}{Coordinate system used:
#'     \itemize{
#'       \item 0 = Cartesian (X, Y in arbitrary units like meters)
#'       \item 1 = Latitude/Longitude (geodetic, requires WGS84-like coordinates)
#'     }
#'   }
#' }
#'
#' @section Analysis Type:
#' The core scan statistic to perform.
#' \describe{
#'   \item{AnalysisType}{Type of scan statistic:
#'     \itemize{
#'       \item 1 = Purely Spatial: Finds geographic clusters ignoring time.
#'       \item 2 = Purely Temporal: Finds time clusters ignoring geography.
#'       \item 3 = Retrospective Space-Time: Finds space-time clusters in historical data.
#'       \item 4 = Prospective Space-Time: Surveillance mode, clusters must include the current time.
#'       \item 5 = Spatial Variation in Temporal Trends: Finds areas with different time trends (Poisson only).
#'       \item 6 = Prospective Purely Temporal: Temporal surveillance, cluster must end at current time.
#'       \item 7 = Seasonal Temporal: Finds recurring seasonal patterns (Dec 31 wraps to Jan 1).
#'     }
#'   }
#' }
#'
#' @section Probability Model:
#' The statistical model for the scan.
#' \describe{
#'   \item{ModelType}{Probability model:
#'     \itemize{
#'       \item 0 = Discrete Poisson: For count data with a known background population. Fast.
#'       \item 1 = Bernoulli: For case/control data (0/1). Requires control file.
#'       \item 2 = Space-Time Permutation: Case-only data, adjusts for purely spatial/temporal patterns. Best for short study periods (<1 year).
#'       \item 3 = Ordinal: For ordered categorical outcomes (e.g., cancer stage I/II/III).
#'       \item 4 = Exponential: For survival time data (with censoring).
#'       \item 5 = Normal: For continuous data (e.g., birth weight). Sensitive to outliers.
#'       \item 6 = Continuous Poisson: For point process data where observations can occur anywhere.
#'       \item 7 = Multinomial: For unordered categorical outcomes (e.g., disease subtypes).
#'       \item 8 = Rank: Non-parametric rank-based model.
#'       \item 9 = UniformTime: Uniform temporal model.
#'       \item 10 = Batched: For batched data analysis.
#'     }
#'   }
#' }
#'
#' @section Cluster Detection:
#' What type of clusters to scan for.
#' \describe{
#'   \item{ScanAreas}{Cluster types to detect:
#'     \itemize{
#'       \item 1 = High Rates (Poisson/Bernoulli/STP), High Values (Ordinal/Normal), Short Survival (Exponential)
#'       \item 2 = Low Rates/Values, Long Survival
#'       \item 3 = Both High and Low (recommended for unbiased analysis)
#'     }
#'   }
#' }
#'
#' @section Spatial Window:
#' Shape and size constraints on the spatial scanning window.
#' \describe{
#'   \item{MaxSpatialSizeInPopulationAtRisk}{Maximum cluster size as percent of population at risk (0-50). Default: 50.}
#'   \item{UseDistanceFromCenterOption}{Restrict cluster size by distance? (y/n)}
#'   \item{MaxSpatialSizeInDistanceFromCenter}{Maximum radius in distance units (positive integer).}
#'   \item{SpatialWindowShapeType}{Window shape:
#'     \itemize{
#'       \item 0 = Circular (default, fastest)
#'       \item 1 = Elliptic (higher power for elongated clusters, slower)
#'     }
#'   }
#'   \item{NonCompactnessPenalty}{Penalty for non-compact ellipses:
#'     \itemize{
#'       \item 0 = No Penalty
#'       \item 1 = Medium Penalty (default)
#'       \item 2 = Strong Penalty
#'     }
#'   }
#'   \item{IncludePurelyTemporal}{Include purely temporal clusters in space-time analysis? (y/n)}
#' }
#'
#' @section Temporal Window:
#' Size constraints on the temporal scanning window.
#' \describe{
#'   \item{MinimumTemporalClusterSize}{Minimum cluster duration in time aggregation units.}
#'   \item{MaxTemporalSizeInterpretation}{How to interpret max temporal size:
#'     \itemize{
#'       \item 0 = Percentage of study period
#'       \item 1 = Absolute time units
#'     }
#'   }
#'   \item{MaxTemporalSize}{Maximum temporal cluster size (0-90 percent, or time units).}
#'   \item{IncludePurelySpatial}{Include purely spatial clusters in space-time analysis? (y/n)}
#'   \item{IncludeClusters}{Which temporal clusters to evaluate:
#'     \itemize{
#'       \item 0 = All clusters
#'       \item 1 = Alive clusters (ending at study end)
#'       \item 2 = Flexible window (specify ranges below)
#'     }
#'   }
#'   \item{IntervalStartRange}{Start date range for flexible window (YYYY/MM/DD,YYYY/MM/DD).}
#'   \item{IntervalEndRange}{End date range for flexible window (YYYY/MM/DD,YYYY/MM/DD).}
#' }
#'
#' @section Inference Settings:
#' Monte Carlo simulation and p-value calculation.
#' \describe{
#'   \item{MonteCarloReps}{Number of Monte Carlo replications (0, 9, 999, 9999, 99999). More = more precise p-values.}
#'   \item{PValueReportType}{P-value calculation method:
#'     \itemize{
#'       \item 0 = Default (standard Monte Carlo)
#'       \item 1 = Standard Monte Carlo
#'       \item 2 = Early Termination (faster for non-significant clusters)
#'       \item 3 = Gumbel approximation
#'     }
#'   }
#'   \item{ProspectiveStartDate}{For prospective analyses, the start date of surveillance (YYYY/MM/DD).}
#'   \item{AdjustForEarlierAnalyses}{Adjust for multiple testing across repeated prospective analyses? (y/n)}
#'   \item{IterativeScan}{Remove detected clusters and re-scan? (y/n)}
#'   \item{IterativeScanMaxIterations}{Maximum iterations for iterative scan.}
#'   \item{IterativeScanMaxPValue}{Stop iterative scan when cluster p-value exceeds this.}
#' }
#'
#' @section Cluster Restrictions:
#' Minimum requirements for reporting clusters.
#' \describe{
#'   \item{MinimumCasesInHighRateClusters}{Minimum cases required to report a high-rate cluster.}
#'   \item{MinimumCasesInLowRateClusters}{Minimum cases required to report a low-rate cluster.}
#'   \item{RiskLimitHighClusters}{Apply risk threshold for high clusters? (y/n)}
#'   \item{RiskThresholdHighClusters}{Minimum relative risk for high clusters (>=1.0).}
#'   \item{RiskLimitLowClusters}{Apply risk threshold for low clusters? (y/n)}
#'   \item{RiskThresholdLowClusters}{Maximum relative risk for low clusters (0-1).}
#' }
#'
#' @section Adjustments:
#' Controls for confounding and trend adjustment.
#' \describe{
#'   \item{TimeTrendAdjustmentType}{Time trend adjustment:
#'     \itemize{
#'       \item 0 = None
#'       \item 2 = Log-linear with user-specified percentage
#'       \item 3 = Calculated log-linear
#'       \item 4 = Time-stratified randomization
#'       \item 5 = Calculated quadratic
#'     }
#'   }
#'   \item{TimeTrendPercentage}{Adjustment percentage (for type 2). Can be negative.}
#'   \item{AdjustForWeeklyTrends}{Adjust for day-of-week effects? (y/n)}
#'   \item{SpatialAdjustmentType}{Spatial adjustment:
#'     \itemize{
#'       \item 0 = None
#'       \item 1 = Spatially-stratified randomization
#'       \item 2 = Spatial nonparametric
#'     }
#'   }
#' }
#'
#' @section Output Files:
#' Control which output files are generated.
#' \describe{
#'   \item{ResultsFile}{Base filename for results (e.g., "analysis.txt").}
#'   \item{OutputShapefiles}{Generate ESRI shapefiles of clusters? (y/n)}
#'   \item{OutputGoogleEarthKML}{Generate Google Earth KML file? (y/n)}
#'   \item{OutputGoogleMaps}{Generate Google Maps HTML output? (y/n)}
#'   \item{CensusAreasReportedClustersDBase}{Output location-level results (.gis) in dBase format? (y/n)}
#'   \item{IncludeRelativeRisksCensusAreasDBase}{Output relative risk estimates (.rr) in dBase format? (y/n)}
#'   \item{MostLikelyClusterCaseInfoEachCentroidDBase}{Output cluster info (.col) in dBase format? (y/n)}
#' }
#'
#' @section Secondary Cluster Reporting:
#' Options for handling overlapping clusters.
#' \describe{
#'   \item{ReportHierarchicalClusters}{Report hierarchical structure of overlapping clusters? (y/n)}
#'   \item{CriteriaForReportingSecondaryClusters}{Overlap criteria:
#'     \itemize{
#'       \item 0 = No Geographic Overlap (most restrictive)
#'       \item 1 = No Centers in Other Cluster
#'       \item 2 = No Centers in Most Likely Cluster
#'       \item 3 = No Centers in Less Likely Cluster
#'       \item 4 = No Pair of Centers in Each Other
#'       \item 5 = No Restrictions (report all)
#'     }
#'   }
#' }
#'
#' @section Data Validation:
#' \describe{
#'   \item{StudyPeriodCheckType}{Date range checking:
#'     \itemize{
#'       \item 0 = Strict (error if data outside study period)
#'       \item 1 = Relaxed (allow data outside study period)
#'     }
#'   }
#'   \item{GeographicalCoordinatesCheckType}{Coordinate validation:
#'     \itemize{
#'       \item 0 = Strict (error on invalid coordinates)
#'       \item 1 = Relaxed (skip invalid coordinates)
#'     }
#'   }
#' }
#'
#' @section Advanced: Network and Neighbors:
#' For non-Euclidean spatial relationships.
#' \describe{
#'   \item{UseLocationsNetworkFile}{Use network file for distance calculations? (y/n)}
#'   \item{LocationsNetworkFilename}{Path to network file.}
#'   \item{UseNeighborsFile}{Use non-Euclidean neighbors file? (y/n)}
#'   \item{NeighborsFilename}{Path to neighbors file.}
#'   \item{UseMetaLocationsFile}{Use meta-locations (grouped locations)? (y/n)}
#'   \item{MetaLocationsFilename}{Path to meta-locations file.}
#' }
#'
#' @section Power Evaluation:
#' Estimate statistical power of the analysis.
#' \describe{
#'   \item{PerformPowerEvaluation}{Run power analysis? (y/n, Poisson only)}
#'   \item{PowerEvaluationsMethod}{Power evaluation method:
#'     \itemize{
#'       \item 0 = Analysis and power evaluation together
#'       \item 1 = Power evaluation with case file only
#'       \item 2 = Power evaluation with defined total cases
#'     }
#'   }
#'   \item{PowerEvaluationTotalCases}{Total cases for power calculation.}
#'   \item{NumberPowerReplications}{Number of simulations for power estimation.}
#'   \item{AlternativeHypothesisFilename}{File defining alternative hypothesis for power.}
#' }
#'
#' @section Elliptic Scan:
#' Settings when using elliptic scanning window (SpatialWindowShapeType=1).
#' \describe{
#'   \item{EllipseShapes}{Ellipse shape ratios, comma-separated (e.g., "1.5,2,3,4,5").}
#'   \item{EllipseAngles}{Number of angles per ellipse, comma-separated (e.g., "4,6,9,12,15").}
#' }
#'
#' @section Runtime Options:
#' Performance and execution settings.
#' \describe{
#'   \item{NumberParallelProcesses}{Number of parallel processes (0 = all available CPUs).}
#'   \item{SuppressWarnings}{Suppress warning messages? (y/n)}
#'   \item{LogRunToHistoryFile}{Log run to history file? (y/n)}
#'   \item{ExecutionType}{Execution method:
#'     \itemize{
#'       \item 0 = Automatic
#'       \item 1 = Successively
#'       \item 2 = Centrically
#'     }
#'   }
#' }
#'
#' @section System:
#' \describe{
#'   \item{Version}{SaTScan version (read-only, do not modify).}
#' }
#'
#' @references
#' Kulldorff M. A spatial scan statistic. Communications in Statistics: Theory and Methods. 1997;26:1481-1496.
#'
#' Kulldorff M, et al. A space-time permutation scan statistic for disease outbreak detection. PLoS Medicine. 2005;2:e59.
#'
#' @seealso \code{\link{satscanr}} for running the analysis.
#'
#' @name satscan_options
#' @docType data
NULL
