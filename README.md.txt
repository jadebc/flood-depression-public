# Overview

Prenatal depression can have lasting adverse impacts on child health. Little is known about the impact of floods on prenatal depression in low- and middle-income countries. We investigated the association between flooding and prenatal depression in a rural and riverine community in Bangladesh. We used participant reported flooding data, administered the Edinburgh Postnatal Depression Scale (EPDS) to evaluate depressive symptoms, and obtained water level data and remote sensing data on distance to surface water. We fit generalized linear and log-linear models to evaluate associations. 

# Directory structure

-**0-config.R**: configuration file that sets data directories, sources base functions, and loads required libraries

-**0-base-functions**: R script containing general functions used across the analysis

-**0-data-processing.R**: R script for processing data used in the analysis by generating necessary variables and merging in additional data

-**1-analysis**: folder containing analysis scripts
	-1-depression-flood-regression.R
	-2-depression-surface-water-dist-regression.R
	-3-depression-surface-water-prop-regression.R
	-4-depression-water-level-regression.R
	-5-e-value-calculations.R
	-6-VI-regression.R

-**2-figure-scripts**: folder containing figure scripts
	-tables-flood-depression.R
	-appendix-tables-flood-depression.R

-**3-table-scripts**: folder containing table scripts
	-flood-water-map.R
	-plot-VI-depression.R
	-plot-water-level-flood.R

-**4-figures**: folder containing figures generated from the analysis

-**5-results**: folder containing analysis results

-**6-tables**: folder containing tables generated from the analysis


# Contributors

Suhi Hanif, Jannat-E-Tajreen Momo, Farjana Jahan, Liza Goldberg, Natalie Herbert, Afsana Yeamin, Abul Kasham Shoab, Reza Mostary Akhter, Sajal Kumar Roy, Gabriella Barratt Heitmann, Ayse Ercumen, Mahbub Rahman, Fahmida Tofail, Gabrielle Wong-Parodi, Jade Benjamin-Chung