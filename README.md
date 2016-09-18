# TopHRU
###Threshold optimization for HRUs in SWAT

topHRU calculates the spatial error measure aREA (average Relative Error of Aggregation) for different input data aggregation levels of the Soil and Water Assessment Tool (SWAT). Input aggregation in SWAT is based on HRU thresholds for land use, soil, and slope. topHRU allows identifying pareto-optimal threshold combinations (minimal spatial error for a given number of HRUs) to minimze the trade-off between computation time and model error.

###Installing the topHRU package
To install the package please execute following lines in R:

req_pckg <- c("abind", "devtools", "emoa", "ggplot2", "plotly")
pckg_installed <- req_pckg %in% rownames(installed.packages())
install.packages(req_pckg[!pckg_installed])
devtools::install_git("https://github.com/chrisschuerz/TopHRU")

###Input data
As INPUT you need the "hrus" table from the "project_name.mdb" from a SWAT project data base imported as data.frame in R database. The hrus table will become available AFTER the definition of HRUs in an ArcSWAT project WITHOUT applying thresholds 
(i.e. 0 for land use, soil, and slope).

### Minimum example
The package contains the template data set hru_data. 
To see the structure of the required table you can compare with the template data set:
View(hru_data)

To run an HRU analysis for the template data set run:
hru_analysis <- topHRU(hru_data)

The resulting list holds the complete results of the HRU analysis
head(hru_analysis$result_all)
a reduced result table only showing the pareto optimal threshold combinations:
head(hru_analysis$result_nondominated)
and a interactive visualization of the dominated and non dominated threshold combinations that supports the user in finding an adequate threshold combination for the respective project:
hru_analysis$pareto_plot


Authors: Robert Schweppe, Michael Strauch

Contact: michael.strauch@ufz.de

https://zenodo.org/badge/latestdoi/18405/michstrauch/TopHRU
