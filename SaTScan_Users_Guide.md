
## Page 1

# SaTScan™ User Guide

for version 10.1

By Martin Kulldorff
July 2022
http://www.satscan.org/

---


## Page 2

# Table of Contents

## INTRODUCTION
*   THE SATSCAN SOFTWARE &lt;page_number&gt;1&lt;/page_number&gt;
*   DOWNLOAD AND INSTALLATION &lt;page_number&gt;2&lt;/page_number&gt;
*   TEST RUN &lt;page_number&gt;2&lt;/page_number&gt;
*   HELP SYSTEM &lt;page_number&gt;3&lt;/page_number&gt;
*   SAMPLE DATA SETS &lt;page_number&gt;3&lt;/page_number&gt;

## STATISTICAL METHODOLOGY
*   SPATIAL, TEMPORAL AND SPACE-TIME SCAN STATISTICS &lt;page_number&gt;6&lt;/page_number&gt;
*   BERNOUlli MODEL &lt;page_number&gt;8&lt;/page_number&gt;
*   DISCRETE POISSON MODEL &lt;page_number&gt;9&lt;/page_number&gt;
*   SPACE-TIME PERMUTATION MODEL &lt;page_number&gt;9&lt;/page_number&gt;
*   MULTINOMIAL MODEL &lt;page_number&gt;10&lt;/page_number&gt;
*   ORDINAL MODEL &lt;page_number&gt;10&lt;/page_number&gt;
*   EXPONENTIAL MODEL &lt;page_number&gt;12&lt;/page_number&gt;
*   NORMAL MODEL &lt;page_number&gt;12&lt;/page_number&gt;
*   CONTINUOUS POISSON MODEL &lt;page_number&gt;13&lt;/page_number&gt;
*   PROBABILITY MODEL COMPARISON &lt;page_number&gt;14&lt;/page_number&gt;
*   LIKELIHOOD RATIO TEST &lt;page_number&gt;16&lt;/page_number&gt;
*   SECONDARY CLUSTERS &lt;page_number&gt;17&lt;/page_number&gt;
*   ADJUSTING FOR MORE LIKELY CLUSTERS &lt;page_number&gt;17&lt;/page_number&gt;
*   DRILLDOWN &lt;page_number&gt;18&lt;/page_number&gt;
*   COVARIATE ADJUSTMENTS &lt;page_number&gt;18&lt;/page_number&gt;
*   SPATIAL AND TEMPORAL ADJUSTMENTS &lt;page_number&gt;21&lt;/page_number&gt;
*   MISSING DATA &lt;page_number&gt;22&lt;/page_number&gt;
*   MULTIVARIATE SCAN WITH MULTIPLE DATA SETS &lt;page_number&gt;24&lt;/page_number&gt;

## COMPARISON WITH OTHER METHODS
*   SCAN STATISTICS &lt;page_number&gt;25&lt;/page_number&gt;
*   SPATIAL AND SPACE-TIME CLUSTERING &lt;page_number&gt;25&lt;/page_number&gt;

## INPUT DATA
*   DATA REQUIREMENTS &lt;page_number&gt;27&lt;/page_number&gt;
*   CASE FILE &lt;page_number&gt;28&lt;/page_number&gt;
*   CONTROL FILE &lt;page_number&gt;30&lt;/page_number&gt;
*   POPULATION FILE &lt;page_number&gt;30&lt;/page_number&gt;
*   COORDINATES FILE &lt;page_number&gt;31&lt;/page_number&gt;
*   GRID FILE &lt;page_number&gt;32&lt;/page_number&gt;
*   NETWORK FILE &lt;page_number&gt;32&lt;/page_number&gt;
*   NON-EUCLIDEAN NEIGHBORS FILE &lt;page_number&gt;33&lt;/page_number&gt;
*   META LOCATION FILE &lt;page_number&gt;33&lt;/page_number&gt;
*   MAX CIRCLE SIZE FILE &lt;page_number&gt;34&lt;/page_number&gt;
*   ADJUSTMENTS FILE &lt;page_number&gt;34&lt;/page_number&gt;
*   ALTERNATIVE HYPOTHESIS FILE &lt;page_number&gt;35&lt;/page_number&gt;
*   SATSCAN IMPORT WIZARD &lt;page_number&gt;35&lt;/page_number&gt;
*   SATSCAN ASCII FILE FORMAT &lt;page_number&gt;37&lt;/page_number&gt;

## BASIC SATSCAN FEATURES
*   INPUT TAB &lt;page_number&gt;40&lt;/page_number&gt;
*   ANALYSIS TAB &lt;page_number&gt;43&lt;/page_number&gt;
*   OUTPUT TAB &lt;page_number&gt;46&lt;/page_number&gt;

SaTScan User Guide v10.1

---


## Page 3

# ADVANCED FEATURES
*   MULTIPLE DATA SETS TAB 49
*   DATA CHECKING TAB 50
*   SPATIAL NEIGHBORS TAB 51
*   NETWORK TAB 52
*   LINE LIST TAB 53
*   SPATIAL WINDOW TAB 55
*   TEMPORAL WINDOW TAB 58
*   CLUSTER RESTRICTIONS TAB 60
*   SPACE AND TIME ADJUSTMENTS TAB 61
*   INFERENCE TAB 64
*   POWER EVALUATION TAB 67
*   DRILDDOWN TAB 68
*   MISCELLANEOUS TAB 69
*   SPATIAL OUTPUT TAB 71
*   TEMPORAL OUTPUT TAB 74
*   OTHER OUTPUT TAB 75
*   ALERTS TAB 77

# RUNNING SATSCAN
*   SPECIFYING ANALYSIS AND DATA OPTIONS 79
*   LAUNCHING THE ANALYSIS 79
*   STATUS MESSAGES 80
*   WARNINGS AND ERRORS 80
*   SAVING ANALYSIS PARAMETERS 81
*   PARALLEL PROCESSORS 81
*   BATCH MODE 82
*   MULTIPLE ANALYSES 82
*   COMPUTING TIME 85
*   MEMORY REQUIREMENTS 86

# RESULTS OF ANALYSIS
*   STANDARD TEXT-BASED RESULTS FILE (*.OUT.*) 89
*   KML GEOGRAPHICAL OUTPUT FILE (*.KML) 91
*   SHAPEFILE GEOGRAPHICAL OUTPUT (*.SHP AND *.SHX) 91
*   TEMPORAL GRAPHS HTML FILE (*.TEMPORAL.HTML) 91
*   CLUSTER INFORMATION FILE (*.COL.*) 92
*   STRATIFIED CLUSTER INFORMATION FILE (*.SCI.*) 94
*   LOCATION INFORMATION FILE (*.GIS.*) 94
*   RISK ESTIMATES FOR EACH LOCATION FILE (*.RR.*) 95
*   SIMULATED LOG LIKELIHOOD RATIOS FILE (*.LLR.*) 95

# MISCELLANEOUS
*   NEW VERSIONS 96
*   ANALYSIS HISTORY FILE 96
*   RANDOM NUMBER GENERATOR 96
*   CONTACT US 97
*   ACKNOWLEDGEMENTS 97

# FREQUENTLY ASKED QUESTIONS
*   INPUT DATA 99
*   ANALYSIS 100
*   RESULTS 100

SaTScan User Guide v10.1

---


## Page 4

INTERPRETATION .................................................... 101
OPERATING SYSTEMS .............................................. 103

SATSCAN BIBLIOGRAPHY ........................................... 104

SUGGESTED CITATIONS ............................................. 104
SATSCAN METHODOLOGY PAPERS .................................. 105
SELECTED SATSCAN APPLICATIONS BY FIELD OF STUDY ........ 108
OTHER REFERENCES MENTIONED IN THE USER GUIDE ............ 129

---
SaTScan User Guide v10.1

---


## Page 5

# Introduction

## The SaTScan Software

### Purpose

SaTScan is a free software that analyzes spatial, temporal and space-time data using the spatial, temporal, or space-time scan statistics. It is designed for any of the following interrelated purposes:

*   Perform geographical surveillance of disease, to detect spatial or space-time disease clusters, and to see if they are statistically significant.
*   Test whether a disease is randomly distributed over space, over time or over space and time.
*   Evaluate the statistical significance of disease cluster alarms.
*   Perform prospective real-time or time-periodic disease surveillance for the early detection of disease outbreaks.

The software may also be used for similar problems in other fields such as archaeology, astronomy, botany, criminology, ecology, economics, engineering, forestry, genetics, geography, geology, history, neurology or zoology.

### Data Types and Methods

SaTScan can be used for discrete as well as continuous scan statistics. For discrete scan statistics the geographical locations where data are observed are non-random and fixed by the user. These locations may be the actual locations of the observations, such as houses, schools or ant nests, or it could be a central location representing a larger area, such as the geographical or population weighted centroids of postal areas, counties or provinces. For continuous scan statistics, the locations of the observations are random and can occur anywhere within a predefined study area defined by the user, such as a rectangle.

For discrete scan statistics, SaTScan uses either a discrete Poisson-based model, where the number of events in a geographical location is Poisson-distributed, according to a known underlying population at risk; a Bernoulli model, with 0/1 event data such as cases and controls; a space-time permutation model, using only case data; a multinomial model for categorical data; an ordinal model, for ordered categorical data; an exponential model for survival time data with or without censored variables; a normal model for other types of continuous data; or a spatial variation in temporal trends model, looking for geographical areas with unusually high or low temporal trends. A common feature of all these discrete scan statistics is that the geographical locations where data can be observed are non-random and fixed by the user.

For the discrete scan statistics, the data may be either aggregated at the census tract, zip code, county or other geographical level, or there may be unique coordinates for each observation. SaTScan adjusts for the underlying spatial inhomogeneity of a background population. It can also adjust for any number of categorical covariates provided by the user, as well as for temporal trends, known space-time clusters and missing data. It is possible to scan multiple data sets simultaneously to look for clusters that occur in one or more of them.

For continuous scan statistics, SaTScan uses a continuous Poisson model.

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;1&lt;/page_number&gt;

---


## Page 6

# Developers and Funders

The SaTScan™ software was developed by Martin Kulldorff together with Information Management Services Inc. Financial support for SaTScan has been received from the following institutions:

*   National Cancer Institute, Division of Cancer Prevention, Biometry Branch [v1.0, 2.0, 2.1]
*   National Cancer Institute, Division of Cancer Control and Population Sciences, Statistical Research and Applications Branch [v3.0 (part), 6.1 (part), 8.0 (part), 9.0 (part)]
*   Alfred P. Sloan Foundation, through a grant to the New York Academy of Medicine [SaTScan v3.0 (part), 3.1, 4.0, 5.0, 5.1] and the Fund for Public Health in New York City [v10.0 (part)]
*   Centers for Disease Control and Prevention, through Association of American Medical Colleges Cooperative Agreement award number MM-0870 [SaTScan v6.0, 6.1 (part)] and ELC CARES grant NU50CK000517-01-09 [v10.0 (part)]
*   National Institute of Child Health and Development, through grant #RO1HD048852 [v7.0, 8.0, 9.0 (part)]
*   National Cancer Institute, Division of Cancer Epidemiology and Genetics [v9.0 (part), 9.1]
*   National Institute of General Medical Sciences, through a Modeling Infectious Disease Agent Studies grant #U01GM076672 [v9.0 (part)]
*   CDC Foundation, through a grant to the Fund for Public Health in NYC [v10.0 (part)]

Their financial support is greatly appreciated. The contents of SaTScan are the responsibility of the developer and do not necessarily reflect the official views of the funders.

**Related Topics:** *Statistical Methodology*, *SaTScan Bibliography*

---

# Download and Installation

To install SaTScan, go to the SaTScan Web site at: http://www.satscan.org/ and select the SaTScan download link. After downloading the SaTScan installation executable to your PC, click on its icon and install the software by following the step-wise instructions.

**Related Topics:** *New Versions.*

---

# Test Run

Before using your own data, we recommend trying one of the sample data sets provided with the software. Use these to get an idea of how to run SaTScan. To perform a test run:

1.  Click on the SaTScan application icon.
2.  Click on ‘Open Saved Session’.
3.  Select one of the parameter files, for example ‘NewMexicoSpaceTimePoisson.prm’ (Poisson model), ‘NHumberSideSpatialBernoulli.prm’ (Bernoulli model) or ‘NYCSPaceTimePermutation.prm’ (space-time permutation model).
4.  Click on ‘Open’.
5.  Click on the Execute &lt;img&gt;Play button icon&lt;/img&gt; button. A new window will open with the program running in the top section and a Warnings/Errors section below. When the program finishes running the results will be displayed.

**Note:** The sample files should not produce warnings or errors.

---

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;2&lt;/page_number&gt;

---


## Page 7

Related Topics: Sample Data Sets.

# Help System

The SaTScan help system consists of four parts:

i. SaTScan User Guide in PDF format located in the same folder as the SaTScan executable. It can also be obtained from the SaTScan web site (www.satscan.org/techdoc.html) or directly within the SaTScan software by selecting Help > User Guide. You may print this as a single document for easy reference.
ii. SaTScan help entries, extracted from the User Guide. The complete set of entries can be found within the SaTScan software by typing F1 or by selecting Help > Help Content. The system can be searched by clicking the magnifying glass. Many individual entries can also be reached directly by clicking on any sub-title seen on the input tabs.
iii. Methodological papers describe the details about the statistical methods available in the SaTScan software. These papers are listed in the SaTScan bibliography, which can be found both at the end of the User Guide and on the web (http://www.satscan.org/references.html). The bibliography also contains a large number of papers that have applied different SaTScan features for a wide range of different types of data. These can serve as inspiration for how SaTScan can be used for different types of scientific and public health problems.
iv. Sample data sets are provided for each of the SaTScan probability models. Described below, they make it easy to familiarize oneself with the software.

# Sample Data Sets

Six different sample data sets are provided with the software. They are automatically downloaded to your computer together with the software itself. Other sample data sets are available at http://www.satscan.org/datasets/.

**Discrete Poisson Model, Space-Time and Spatial Variation in Temporal Trends: Brain Cancer Incidence in New Mexico**

Case file: nm.cas
Format: <county> <cases=1> <year> <age group> <sex>
Population file: nm.pop
Format: <county> <year> <population> <age group> <sex>
Coordinates file: nm.geo
Format: <county> <latitude> <longitude>
Study period: 1973-1991
Aggregation: 32 counties
Precision of case times: Years
Coordinates: Latitude/Longitude
Covariate #1, age groups: 1 = 0-4 years, 2 = 5-9 years, ... 18 = 85+ years
Covariate #2, gender: 1 = male, 2 = female
Population years: 1973, 1982, 1991
Data source: New Mexico SEER Tumor Registry

SaTScan User Guide v10.1 &lt;page_number&gt;3&lt;/page_number&gt;

---


## Page 8

This is a condensed version of a more complete data set with the population given for each year from 1973 to 1991, and with ethnicity as a third covariate. The complete data set can be found at:
http://www.satscan.org/datasets/

**Bernoulli Model, Purely Spatial: Childhood Leukemia and Lymphoma Incidence in North HumberSide**

**Case file:** NHumberSide.cas
**Format:** <location id> <# cases>

**Control file:** Nnumberside.ctl
**Format:** <location id> <# controls>

**Coordinates file:** Nnumberside.geo
**Format:** <location id> <x-coordinate> <y-coordinate>

**Study period:** 1974-1986

**Controls:** Randomly selected from the birth registry

**Aggregation:** 191 Postal Codes (most with only a single individual)

**Precision of case and control times:** None

**Coordinates:** Cartesian

**Covariates:** None

**Data source:** Drs. Ray Cartwright and Freda Alexander. Published by J. Cuzick and R. Edwards, Journal of the Royal Statistical society, B:52 73-104, 1990

**Space-Time Permutation Model: Hospital Emergency Room Admissions Due to Fever at New York City Hospitals**

**Case file:** NYCfever.cas
**Format:** <zip> <#cases=1> <date>

**Coordinates file:** NYCfever.geo
**Format:** <zip> <latitude> <longitude>

**Study period:** Nov 1, 2001 – Nov 24, 2001

**Aggregation:** Zip code areas

**Precision of case times:** Days

**Coordinates:** Latitude/Longitude

**Covariates:** None

**Data source:** New York City Department of Health

**Multinomial and Ordinal Model, Purely Spatial: Education Attainment Levels in Maryland**

**Case file:** MarylandEducation.cas
**Format:** <county> <# individuals> <category #>

**Coordinates file:** MarylandEducation.geo
**Format:** <county> <latitude> <longitude>

**Study period:** 2000

**Aggregation:** 24 Counties and County Equivalents

SaTScan User Guide v10.1
&lt;page_number&gt;4&lt;/page_number&gt;

---


## Page 9

**Precision of case times:** None
**Coordinates:** Latitude / Longitude
**Covariates:** None
**Categories:**
1 = Less than 9<sup>th</sup> grade
2 = 9<sup>th</sup> to 12<sup>th</sup> grade, but no high school diploma
3 = High school diploma, but no bachelor's degree
4 = Bachelor or higher degree

**Data source:** United States Census Bureau: Information about education comes from the long Census 2000 form, filled in by about 1/6 households.

**Note:** Only people age 25 and above are included in the data. For each county, the census provides information about the percent of people with different levels of formal education. The number of individuals reporting different education levels in each county was estimated as this percentage times the total population age 25+ divided by six to reflect the 1/6 sampling fraction for the long census form.

*Exponential Model, Space-Time: Artificially Created Survival Data*

**Case file:** SurvivalFake.cas
**Format:** <location id> <# individuals> <time of diagnosis> <survival time> <censored>

**Coordinates file:** SurvivalFake.geo
**Format:** <location id> <x-coordinate> <y-coordinate>

**Study period:** 2000-2005
**Aggregation:** 5 Locations
**Precision of times of diagnosis:** Day
**Precision of survival/censoring times:** Day
**Coordinates:** Cartesian
**Covariates:** None
**Data source:** Artificially created data.
**Note:** This data set can also be used for a test run of the seasonal scan statistic.

*Normal Model, Purely Spatial: Artificially Created Continuous Data*

**Case file:** NormalFake.cas
**Format:** <location id> <# individuals> <weight increase>

**Coordinates file:** NormalFake.geo
**Format:** <location id> <x-coordinate> <y-coordinate>

**Study period:** 2006
**Aggregation:** 26 Locations
**Coordinates:** Cartesian
**Covariates:** None
**Data source:** Artificially created data.
**Related Topics:** Test Run, Input Data.

SaTScan User Guide v10.1
&lt;page_number&gt;5&lt;/page_number&gt;

---


## Page 10

# Statistical Methodology

For all discrete spatial and space-time analyses, the user will typically provide data containing the spatial coordinates of a set of locations (coordinates file). As an alternatively, it is possible to define a set of locations on a network specifying the distance between neighbors on that network. For each location, the data must furthermore contain information about the number of cases at that location (case file). For temporal and space-time analyses, the number of cases must be stratified by time, e.g. the time of diagnosis. Depending on the type of analysis, other information about cases such as age, gender, weight, length of survival and/or cancer stage may also be provided. For the Bernoulli model, it is also necessary to specify the number of controls at each location (control file). For the discrete Poisson model, the user must specify a population size for each location (population file). The population may vary over time.

Scan statistics are used to detect and evaluate clusters of cases in either a purely temporal, purely spatial or space-time setting. This is done by gradually scanning a window across time and/or space, noting the number of observed and expected observations inside the window at each location. In the SaTScan software, the scanning window is an interval (in time), a circle or an ellipse (in space) or a cylinder with a circular or elliptic base (in space-time). It is also possible to specify your own non-Euclidean distance structure in a special file or to look for clusters of neighboring locations on a user defined network. Multiple different window sizes are used. The window with the maximum likelihood is the most likely cluster, that is, the cluster least likely to be due to chance. A p-value is assigned to this cluster.

Scan statistics use a different probability model depending on the nature of the data. A Bernoulli, discrete Poisson or space-time permutation model is used for count data such as the number of people with asthma; a multinomial model is used for categorical data such as cancer histology; an ordinal model for ordered categorical data such as cancer stage; an exponential model for survival time data with or without censoring; and a normal model for other continuous data such as birth weight or blood lead levels. The general statistical theory behind the spatial and space-time scan statistics used in the SaTScan software is described in detail by Kulldorff (1997)<sup>1</sup> for the Bernoulli, discrete Poisson and continuous Poisson models; by Kulldorff et al. (2005)<sup>5</sup> for the space-time permutation model; by Jung et al. (2008)<sup>6</sup> for the multinomial model; by Jung et al. (2007)<sup>7</sup> for the ordinal model; by Huang et al. (2006)<sup>8</sup> for the exponential model, by Kulldorff et al. (2009)<sup>9</sup> for the normal model and by Huang et al. (2009)<sup>10</sup> for the normal model with weights. Please read these papers for a detailed description of each model. Here we only give a brief non-mathematical description.

For all discrete probability models, the scan statistic adjusts for the uneven geographical density of a background population. For all models, the analyses are conditioned on the total number of cases observed.

**Related Topics:** *The SaTScan Software*, *Basic SaTScan Features*, *Advanced Features*, *Analysis Tab*, *Methodological Papers*.

---

# Spatial, Temporal and Space-Time Scan Statistics

## Spatial Scan Statistic

The standard purely spatial scan statistic imposes a circular window on the map. The window is in turn centered on each of several possible grid points positioned throughout the study region. For each grid point, the radius of the window varies continuously in size from zero to some upper limit specified by the user. In this way, the circular window is flexible both in location and size. In total, the method creates an infinite number of distinct geographical circles with different sets of neighboring data locations within them. Each circle is a possible candidate cluster.

---

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;6&lt;/page_number&gt;

---


## Page 11

The user defines the set of grid points used through a grid file. If no grid file is specified, the grid points are set to be identical to the coordinates of the location IDs defined in the coordinates file. The latter option ensures that each data location is a potential cluster in itself, and it is the recommended option for most types of analyses.

As an alternative to the circle, it is also possible to use an elliptic window shape, in which case a set of ellipses with different shapes and angles are used as the scanning window together with the circle. This provides slightly higher power for true clusters that are long and narrow in shape, and slightly lower power for circular and other very compact clusters.

It is sometimes of interest to look for clusters along a network, such as a road network or a water distribution network. This can be done by specifying a network file, with one entry for each pair of neighboring locations in the network, together with the distance between those locations.

It is also possible to use a non-Euclidean distance metric with a special neighbors file. With such a file, a list of the closest neighbors and their order is specified for each location.

**Related Topics:** *Analysis Tab*, *Coordinates File*, *Elliptic Scanning Window*, *Grid File*, *Maximum Spatial Cluster Size*, *Spatial Window Tab*.

## Space-Time Scan Statistic

The space-time scan statistic is defined by a cylindrical window with a circular (or elliptic or network based) geographic base and with height corresponding to time. The base is defined exactly as for the purely spatial scan statistic, while the height reflects the time period of potential clusters. The cylindrical window is then moved in space and time, so that for each possible geographical location and size, it also visits each possible time period. In effect, we obtain an infinite number of overlapping cylinders of different size and shape, jointly covering the entire study region, where each cylinder reflects a possible cluster.

The space-time scan statistic may be used for either a single retrospective analysis, using historic data, or for time-periodic prospective surveillance, where the analysis is repeated for example every day, week, month or year.

**Related Topics:** *Analysis Tab*, *Spatial Window Tab*, *Temporal Window Tab*.

## Purely Temporal Scan Statistic

The temporal scan statistic uses a window that moves in one dimension, time, defined in the same way as the height of the cylinder used by the space-time scan statistic. This means that it is flexible in both start and end date. The maximum temporal length is specified on the Temporal Window Tab.

**Related Topics:** *Analysis Tab*, *Temporal Window Tab*. *Space-Time Scan Statistic*, *Seasonal Scan Statistic*.

## Seasonal Scan Statistic

The seasonal scan statistic is a purely temporal scan statistic where all the data is on a connecting loop, such as the year, where December 31 is followed by January 1. The key feature that distinguishes the seasonal scan statistic from the purely temporal scan statistic is that it ignores which year the observation was made and only cares about the day and month. The detected cluster may cover the place where the loop connects, such as a cluster from December 24 to January 13, which in a 2010-2011 analysis will include three time periods: the beginning of 2010, the days around the 2010/11 New Year, and the end of 2011. The seasonal scan statistic can also be used for other types of data on a line that connects in a loop, completely unrelated to time, such as data collected along the shore of a lake.

---
SaTScan User Guide v10.1 &lt;page_number&gt;7&lt;/page_number&gt;

---


## Page 12

The minimum and maximum temporal lengths can be specified on the Temporal Window Tab.

**Related Topics:** *Analysis Tab, Temporal Window Tab. Purely Temporal Scan Statistic.*

---

## Spatial Variation in Temporal Trends Scan Statistic

When the scan statistic is used to evaluate the spatial variation in temporal trends, the scanning window is purely spatial in nature. The temporal trend is then calculated inside as well as outside the scanning window, for each location and size of that window. The null hypothesis is that the trends are the same, while the alternative is that they are different. Based on these hypotheses, a likelihood is calculated, which is higher the more unlikely it is that the difference in trends is due to chance. The most likely cluster is the cluster for which the temporal trend inside the window is least likely to be the same as the temporal trend outside the cluster. This could be because of various reasons. For example, if the temporal trend inside the cluster is higher, it could be because all areas has the same incidence rate of a disease at the beginning of the time period, but the cluster area has a higher rate at the end of the time period. It could also be because the cluster area has a lower incidence rate at the beginning of the time period, after which it ‘catches up’ with the rest so that the rate is about the same at the end of the time period. Hence, a statistically significant cluster in the spatial variation in temporal trend analysis does not necessarily mean that the overall rate of disease is higher or lower in the cluster.

The spatial variation in temporal trends scan statistic can only be run with the discrete Poisson probability model. For it to work, it is important that the total study period length is evenly divisible by the length of the time interval aggregation, so that all time intervals have the same number of years, if it is specified in years, the same number of months if it is specified in months or the same number of days if it is specified in days.

**Related Topics:** *Analysis Tab, Space-Time Scan Statistic.*

---

## Bernoulli Model

With the Bernoulli model¹,², there are cases and non-cases represented by a 0/1 variable. These variables may represent people with or without a disease, or people with different types of disease such as early and late stage breast cancer. They may reflect cases and controls from a larger population, or they may together constitute the population as a whole. Whatever the situation may be, these variables will be denoted as cases and controls throughout the user guide, and their total will be denoted as the population. Bernoulli data can be analyzed with the purely temporal, the purely spatial or the space-time scan statistics.

*Example:* For the Bernoulli model, cases may be newborns with a certain birth defect while controls are all newborns without that birth defect.

The Bernoulli model requires information about the location of a set of cases and controls, provided to SaTScan using the case, control and coordinates files. Separate locations may be specified for each case and each control, or the data may be aggregated for states, provinces, counties, parishes, census tracts, postal code areas, school districts, households, etc., with multiple cases and controls at each data location. To do a temporal or space-time analysis, it is necessary to have a time for each case and each control as well.

**Related Topics:** *Analysis Tab, Case File, Control File, Coordinates File, Likelihood Ratio Test, Methodological Papers, Probability Model Comparison.*

---

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;8&lt;/page_number&gt;

---


## Page 13

# Discrete Poisson Model

With the discrete Poisson model¹, the number of cases in each location is Poisson-distributed. Under the null hypothesis, and when there are no covariates, the expected number of cases in each area is proportional to its population size, or to the person-years in that area. Poisson data can be analyzed with the purely temporal, the purely spatial, the space-time and the spatial variation in temporal trends scan statistics.

*Example:* For the discrete Poisson model, cases may be stroke occurrences while the population is the combined number of person-years lived, calculated as 1 for someone living in the area for the whole time period and ½ for someone dying or moving away in the middle of the time period.

The discrete Poisson model requires case and population counts for a set of data locations such as counties, parishes, census tracts or zip code areas, as well as the geographical coordinates for each of those locations. These need to be provided to SaTScan using the case, population and coordinates files.

The population data need not be specified continuously over time, but only at one or more specific ‘census times’. For times in between, SaTScan does a linear interpolation based on the population at the census times immediately preceding and immediately following. For times before the first census time, the population size is set equal to the population size at that first census time, and for times after the last census time, the population is set equal to the population size at that last census time. To get the population size for a given location and time period, the population size, as defined above, is integrated over the time period in question.

**Related Topics:** *Analysis Tab, Case File, Continuous Poisson Model, Coordinates File, Likelihood Ratio Test, Methodological Papers, Population File, Probability Model Comparison.*

# Space-Time Permutation Model

The space-time permutation model⁵ requires only case data, with information about the spatial location and time for each case, with no information needed about controls or a background population at risk. The number of observed cases in a cluster is compared to what would have been expected if the spatial and temporal locations of all cases were independent of each other so that there is no space-time interaction. That is, there is a cluster in a geographical area if, during a specific time period, that area has a higher proportion of its cases in that time period compared to the remaining geographical areas. This means that if, during a specific week, all geographical areas have twice the number of cases than normal, none of these areas constitute a cluster. On the other hand, if during that week, one geographical area has twice the number of cases compared to normal while other areas have a normal amount of cases, then there will be a cluster in that first area. The space-time permutation model automatically adjusts for both purely spatial and purely temporal clusters. Hence there are no purely temporal or purely spatial versions of this model.

*Example:* In the space-time permutation model, cases may be daily occurrences of ambulance dispatches to stroke patients.

It is important to realize that space-time permutation clusters may be due either to an increased risk of disease, or to different geographical population distribution at different times, where for example the population in some areas grows faster than in others. This is typically not a problem if the total study period is less than a year. However, the user is advised to be very careful when using this method for data spanning several years. If the background population increases or decreases faster in some areas than in others, there is risk for population shift bias, which may produce biased p-values when the study period is longer than a few years. For example, if a new large neighborhood is developed, there will be an increase in cases there simply because the population increases, and using only case data, the space-time permutation model cannot distinguish an increase due to a local population increase versus an increase in the disease risk. As with all space-time interaction methods, this is mainly a concern when the study period is longer than a few

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;9&lt;/page_number&gt;

---


## Page 14

years¹⁸⁰,¹⁸². If the population increase (or decrease) is the same across the study region, that is okay, and will not lead to biased results.

**Related Topics:** *Analysis Tab, Case File, Coordinates File, Likelihood Ratio Test, Methodological Papers, Probability Model Comparison.*

---

# Multinomial Model

With the multinomial model⁶, each observation is a case, and each case belongs to one of several categories. The multinomial scan statistic evaluates whether there are any clusters where the distribution of cases is different from the rest of the study region. For example, there may be a higher proportion of cases of types 1 and 2 and a lower proportion of cases of type 3 while the proportion of cases of type 4 is about the same as outside the cluster. If there are only two categories, the ordinal model is identical to the Bernoulli model, where one category represents the cases and the other category represents the controls. The cases in the multinomial model may be a sample from a larger population or they may constitute a complete set of observations. Multinomial data can be analyzed with the purely temporal, the purely spatial or the space-time scan statistics.

*Example:* For the multinomial model, the data may consist of everyone diagnosed with meningitis, with five different categories representing five different clonal complexes of the disease⁶. The multinomial scan statistic will simultaneously look for high or low clusters of any of the clonal complexes, or a group of them, adjusting for the overall geographical distribution of the disease. The multiple comparisons inherent in the many categories used are accounted for when calculating the p-values.

The multinomial model requires information about the location of each case in each category. A unique location may be specified for each case, or the data may be aggregated for states, provinces, counties, parishes, census tracts, postal code areas, school districts, households, etc, with multiple cases in the same location. To do a temporal or space-time analysis, it is necessary to have a time for each case as well.

With the multinomial model it is not necessary to specify a search for high or low clusters, since there is no hierarchy among the categories, but in the output, it is shown what categories are more prominent inside the cluster. The order or indexing of the categories does not affect the analysis in terms of the clusters found, but it may influence the randomization used to calculate the p-values.

**Related Topics:** *Analysis Tab, Case File, Coordinates File, Likelihood Ratio Test, Methodological Papers, Probability Model Comparison.*

---

# Ordinal Model

With the ordinal model⁷, each observation is a case, and each case belongs to one of several ordinal categories. If there are only two categories, the ordinal model is identical to the Bernoulli model, where one category represents the cases and the other category represent the controls in the Bernoulli model. The cases in the ordinal model may be a sample from a larger population or they may constitute a complete set of observations. Ordinal data can be analyzed with the purely temporal, the purely spatial or the space-time scan statistics.

*Example:* For the ordinal model, the data may consist of everyone diagnosed with breast cancer during a ten-year period, with three different categories representing early, medium and late stage cancer at the time of diagnosis.

The ordinal model requires information about the location of each case in each category. Separate locations may be specified for each case, or the data may be aggregated for states, provinces, counties, parishes, census tracts, postal code areas, school districts, households, etc. with multiple cases in the same or different

---
<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;10&lt;/page_number&gt;

---


## Page 15

categories at each data location. To do a temporal or space-time analysis, it is necessary to have a time for each case as well.

With the ordinal model it is possible to search for high clusters, with an excess of cases in the high-valued categories, for low clusters with an excess of cases in the low-valued categories, or simultaneously for both types of clusters. Reversing the order of the categories has the same effect as changing the analysis from high to low and vice versa.

**Related Topics:** *Analysis Tab*, *Case File*, *Coordinates File*, *Likelihood Ratio Test*, *Methodological Papers*, *Probability Model Comparison*.

---
<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;11&lt;/page_number&gt;

---


## Page 16

# Exponential Model

The exponential model⁸ is designed for survival time data, although it could be used for other continuous type data as well. Each observation is a case, and each case has one continuous variable attribute as well as a 0/1 censoring designation. For survival data, the continuous variable is the time between diagnosis and death or depending on the application, between two other types of events. If some of the data is censored, due to loss of follow-up, the continuous variable is then instead the time between diagnosis and time of censoring. The 0/1 censoring variable is used to distinguish between censored and non-censored observations.

*Example:* For the exponential model, the data may consist of everyone diagnosed with prostate cancer during a ten-year period, with information about either the length of time from diagnosis until death or from diagnosis until a time of censoring after which survival is unknown.

When using the temporal or space-time exponential model for survival times, it is important to realize that there are two very different time variables involved. The first is the time the case was diagnosed, and that is the time that the temporal and space-time scanning window is scanning over. The second is the survival time, that is, time between diagnosis and death or for censored data the time between diagnosis and censoring. This is an attribute of each case, and there is no scanning done over this variable. Rather, we are interested in whether the scanning window includes exceptionally many cases with a small or large value of this attribute.

It is important to note, that while the exponential model uses a likelihood function based on the exponential distribution, the true survival time distribution must not be exponential and the statistical inference (p-value) is valid for other survival time distributions as well. The reason for this is that the randomization is not done by generating observations from the exponential distribution, but rather, by permuting the space-time locations and the survival time/censoring attributes of the observations.

**Related Topics:** Likelihood Ratio Test, Analysis Tab, Probability Model Comparison, Methodological Papers.

# Normal Model

The normal model¹⁰ is designed for continuous data. For each individual or for each observation, called a case, there is a single continuous attribute that may be either negative or positive. The model can also be used for ordinal data when there are many categories. That is, different cases are allowed to have the same attribute value.

*Example:* For the normal model, the data may consist of the birth weight and residential census tract for all newborns, with an interest in finding clusters with lower birth weight. One individual is then a ‘case’. Alternatively, the data may consist of the average birth weight in each census tract. It is then the census tract that is the ‘case’, and it is important to use the weighted normal model, since each average will have a different variance due to a different number of births in each tract.

It is important to note that while the normal model uses a likelihood function based on the normal distribution, the true distribution of the continuous attribute must not be normal. The statistical inference (p-value) is valid for any continuous distribution. The reason for this is that the randomization is not done by generating simulated data from the normal distribution, but rather, by permuting the space-time locations and the continuous attribute (e.g. birth weight) of the observations. While still being formally valid, the results can be greatly influenced by extreme outliers, so it may be wise to truncate such observations before doing the analysis.

SaTScan User Guide v10.1 &lt;page_number&gt;12&lt;/page_number&gt;

---


## Page 17

In the standard normal model⁹, it is assumed that each observation is measured with the same variance. That may not always be the case. For example, if an observation is based on a larger sample in one location and a smaller sample in another, then the variance of the uncertainty in the estimates will be larger for the smaller sample. If the reliability of the estimates differs, one should instead use the weighted normal scan statistic¹⁰that takes these unequal variances into account. The weighted version is obtained in SaTScan by simply specifying a weight for each observation as an extra column in the input file. This weight may for example be proportional to the sample size used for each estimate or it may be the inverse of the variance of the observation.

If all values are multiplied with or added to the same constant, the statistical inference will not change, meaning that the same clusters with the same log likelihoods and p-values will be found. Only the estimated means and variances will differ. If the weight is the same for all observations, then the weighted normal scan statistic will produce the same results as the standard normal version. If all the weights are multiplied by the same constant, the results will not change.

**Related Topics:** *Analysis Tab*, *Likelihood Ratio Test*, *Methodological Papers*, *Probability Model Comparison*.

---

# Continuous Poisson Model

All the models described above are based on data observed at discrete locations that are considered to be non-random, as defined by a regular or irregular lattice of location points. That is, the locations of the observations are considered to be fixed, and we evaluate the spatial randomness of the observation conditioning on the lattice. Hence, those are all versions of what are called discrete scan statistics¹⁷⁴. In a continuous scan statistics, observations may be located anywhere within a study area, such as a square or rectangle. The stochastic aspect of the data consists of these random spatial locations, and we are interested to see if there are any clusters that are unlikely to occur if the observations where independently and randomly distributed across the study area. Under the null hypothesis, the observations follow a homogeneous spatial Poisson process with constant intensity throughout the study area, with no observations falling outside the study area.

*Example:* The data may consist of the location of bird nests in a square kilometer area of a forest. The interest may be to see whether the bird nests are randomly distributed spatially, or in other words, whether there are clusters of bird nests or whether they are located independently of each other.

In SaTScan, the study area can be any collection of convex polygons, which are convex regions bounded by any number straight lines. Triangles, squares, rectangles, rhombuses, pentagons and hexagons are all examples of convex polygons. In the simplest case, there is only one polygon, but the study area can also be the union of multiple convex polygons. If the study area is not convex, divide it into multiple convex polygons and define each one separately. The study area does not need to be contiguous and may for example consist of five different islands.

The analysis is conditioned on the total number of observations in the data set. Hence, the scan statistic simply evaluates the spatial distribution of the observation, but not the number of observations.

The likelihood function used as the test statistic is the same as for the Poisson model for the discrete scan statistic, where the expected number of cases is equal to the total number of observed observations, times the size of the scanning window, divided by the size of the total study area. As such, it is a special case of the variable window size scan statistic described by Kulldorff (1997)¹. When the scanning window extends outside the study area, the expected count is still based on the full size of the circle, ignoring the fact that some parts of the circle have zero expected counts. This is to avoid strange non-circular clusters at the border of the study area. Since the analysis is based on Monte Carlo randomizations, the p-values are automatically adjusted for these boundary effects. The reported expected counts are based on the full circle

---

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;13&lt;/page_number&gt;

---


## Page 18

though, so the Obs/Exp ratios provided should be viewed as a lower bound on the true value whenever the circle extends outside the spatial study region.

The continuous Poisson model can only be used for purely spatial data. It uses a circular scanning window of continuously varying radius up to a maximum specified by the user. Only circles centered on one of the observations are considered, as specified in the coordinates file. If the optional grid file is provided, the circles are instead centered on the coordinates specified in that file. The continuous Poisson model has not been implemented to be used with an elliptic window.

**Related Topics:** *Analysis Tab*, *Likelihood Ratio Test*, *Methodological Papers*, *Poisson Model*, *Probability Model Comparison*.

---

# Probability Model Comparison

In SaTScan, there are seven different probability models for discrete scan statistics. For count data, there are three different probability models: discrete Poisson, Bernoulli and space-time permutation. The ordinal and multinomial models are designed for categorical data with and without an inherent ordering from for example low to high. There are two models for continuous data: normal and exponential. The latter is primarily designed for survival type data. For continuous scan statistics there is only the homogeneous Poisson model.

The discrete Poisson model is usually the fastest to run. The ordinal model is typically the slowest.

With the discrete Poisson and space-time permutations models, an unlimited number of covariates can be adjusted for, by including them in the case and population files. With the normal model, it is also possible to adjust for covariates by including them in the case file, but only for purely spatial analyses. With the Bernoulli, ordinal, exponential and normal models, covariates can be adjusted for by using multiple data sets, which limits the number of covariate categories that can be defined, or through a pre-processing regression analysis done before running SaTScan.

All discrete probability models can be used for either individual locations or aggregated data.

With the discrete Poisson model, population data is only needed at selected time points and the numbers are interpolated in between. A population time must be specified even for purely spatial analyses. Regardless of model used, the time of a case or control need only be specified for purely temporal and space-time analyses.

The space-time permutation model automatically adjusts for purely spatial and purely temporal clusters. For the discrete Poisson model, purely temporal and purely spatial clusters can be adjusted for in a number of different ways. For the Bernoulli, ordinal, exponential and normal models, spatial and temporal adjustments can be done using multiple data sets, but it is limited by the number of different data sets allowed, and it is also much more computer intensive.

Purely temporal and space-time analyses cannot be performed using the homogeneous Poisson model. Spatial variation in temporal trend analyses can only be performed using the discrete Poisson model.

## Few Cases Compared to Controls

In a purely spatial analysis where there are few cases compared to controls, say less than 10 percent, the discrete Poisson model is a very good approximation to the Bernoulli model. The former can then be used also for 0/1 Bernoulli type data and may be preferable as it has more options for various types of adjustments, including the ability to adjust for covariates specified in the case and population files. As an approximation for Bernoulli type data, the discrete Poisson model produces slightly conservative p-values.

---

<footer>SaTScan User Guide v10.1</footer>&lt;page_number&gt;14&lt;/page_number&gt;

---


## Page 19

**Bernoulli versus Ordinal Model**

The Bernoulli model is mathematically a special case of the ordinal model, when there are only two categories. The Bernoulli model runs faster, making it the preferred model to use when there are only two categories.

**Normal versus Exponential Model**

Both the normal and exponential models are meant for continuous data. The exponential model is primarily designed for survival time data but can be used for any data where all observations are positive. It is especially suitable for data with a heavy right tail. The normal model can be used for continuous data that takes both positive and negative values. While still formally valid, results from the normal model are sensitive to extreme outliers.

**Normal versus Ordinal Model**

The normal model can be used for categorical data when there are very many categories. As such, it is sometimes a computationally faster alternative to the ordinal model. There is an important difference though. With the ordinal model, only the order of the observed values matters. For example, the results are the same for ordered values ‘1 – 2 – 3 – 4’ and ‘1 – 10 – 100 – 1000’. With the normal model, the results will be different, as they depend on the relative distance between the values used to define the categories.

**Discrete versus Homogeneous Poisson Model**

Instead of using the homogeneous Poisson model, the data can be approximated by the discrete Poisson model by dividing the study area into many small pieces. For each piece, a single coordinates point is specified, the size of the piece is used to define the population at that location and the number of observations within that small piece of area is the number of cases in that location. As the number of pieces increases towards infinity, and hence, as their size decreases towards zero, the discrete Poisson model will be asymptotically equivalent to the homogeneous Poisson model.

**Temporal Data**

For temporal and space-time data, there is an additional difference among the probability models, in the way that the temporal data is handled. With the Poisson model, population data may be specified at one or several time points, such as census years. The population is then assumed to exist between such time points as well, estimated through linear interpolation between census years. With the Bernoulli, space-time permutation, ordinal, exponential and normal models, a time needs to be specified for each case and for the Bernoulli model, for each control as well.

**Related Topics:** *Bernoulli Model*, *Poisson Model*, *Space-Time Permutation Model*, *Likelihood Ratio Test*, *Methodological Papers*.

SaTScan User Guide v10.1 &lt;page_number&gt;15&lt;/page_number&gt;

---


## Page 20

# Likelihood Ratio Test

For each location and size of the scanning window, the alternative hypothesis is that there is an elevated risk within the window as compared to outside. Under the Poisson assumption, the likelihood function for a specific window is proportional to¹:

$$\left(\frac{c}{E[c]}\right)^c \left(\frac{C-c}{C-E[c]}\right)^{C-c} I()$$

where C is the total number of cases, c is the observed number of cases within the window and E[c] is the covariate adjusted expected number of cases within the window under the null-hypothesis. Note that since the analysis is conditioned on the total number of cases observed, C-E[c] is the expected number of cases outside the window. I() is an indicator function. When SaTScan is set to scan only for clusters with high rates, I() is equal to 1 when the window has more cases than expected under the null-hypothesis, and 0 otherwise. The opposite is true when SaTScan is set to scan only for clusters with low rates. When the program scans for clusters with either high or low rates, then I()=1 for all windows.

The space-time permutation model uses the same function as the Poisson model. Due to the conditioning on the marginals, the observed number of cases is only approximately Poisson distributed. Hence, it is no longer a formal likelihood ratio test, but it serves the same purpose as the test statistic.

For the Bernoulli model the likelihood function is¹,²:

$$\left(\frac{c}{n}\right)^c \left(\frac{n-c}{n}\right)^{n-c} \left(\frac{C-c}{N-n}\right)^{C-c} \left(\frac{(N-n)-(C-c)}{N-n}\right)^{(N-n)-(C-c)} I()$$

where c and C are defined as above, n is the total number of cases and controls within the window, while N is the combined total number of cases and controls in the data set.

The likelihood function for the multinomial, ordinal, exponential, and normal models are more complex, due to the more complex nature of the data. We refer to papers by Jung, Kulldorff and Richards⁶, Jung, Kulldorff and Klassen⁷; Huang, Kulldorff and Gregorio⁸; Kulldorff et al⁹, and Huang et al.¹⁰ for the likelihood functions for these models. The likelihood function for the spatial variation in temporal trends scan statistic is also more complex, as it involves the maximum likelihood estimation of several different trend functions.

The likelihood function is maximized over all window locations and sizes, and the one with the maximum likelihood constitutes the most likely cluster. This is the cluster that is least likely to have occurred by chance. The likelihood ratio for this window constitutes the maximum likelihood ratio test statistic. Its distribution under the null-hypothesis is obtained by repeating the same analytic exercise on a large number of random replications of the data set generated under the null hypothesis. The p-value is obtained through Monte Carlo hypothesis testing¹⁴, by comparing the rank of the maximum likelihood from the real data set with the maximum likelihoods from the random data sets. If this rank is R, then p = R / (1 + #simulation). In order for p to be a ‘nice looking’ number, the number of simulations is restricted to 999 or some other number ending in 999 such as 1999, 9999 or 99999. That way it is always clear whether to reject or not reject the null hypothesis for typical cut-off values such as 0.05, 0.01 and 0.001.

The SaTScan program scans for areas with high rates (clusters), for areas with low rates, or simultaneously for areas with either high or low rates. The latter should be used rather than running two separate tests for high and low rates respectively, in order to make correct statistical inference. The most common analysis is to scan for areas with high rates, that is, for clusters.

SaTScan User Guide v10.1 &lt;page_number&gt;16&lt;/page_number&gt;

