This is documentation for the New Mexico lung cancer data in the following files. 


lung.cas: lung cancer incidence file
------------------------------------

All lung cancer cases are malignant and were diagnosed during the 1973-1991 period. Format:

1st column:   name of county

Note: In 1981, Valencia county was split into Cibola and Valencia counties. In this data, they are treated as one during the whole time period listed under Valencia.

2nd column:  number cancer cases

3rd column: Month of Diagnosis
   January 1973 = 2001
  February 1973 = 2002
   .
   .
   January 1974 = 2013
   .
   etc.

4th column: Age Group
   1 = ages < 5
   2 = ages 5-9
   3 = ages 10-14
   .
   .
  17 = ages 80-84
  18 = ages 85+       

5th column: Sex
  1 = Male
  2 = Female

Data Source: National Cancer Institutes Surveillance, Epidemiology and End Results (SEER) Program.


nm-m.pop: background population file
------------------------------------

1st column:   name of county
2nd column:   month
3rd column:   population, for that county, month, age group and sex
   Note: Yearly population counts are from the census, and are assigned to July of each year.
4th column:   age group
5th column:   sex

Data Source: United States Census Bureau.

nm-km.pop: geographic coordinates file
--------------------------------------

Geographic coordinate of the county seats in units of kilometers.

Data Source: Rand McNally Cosmopolitan World Atlas
