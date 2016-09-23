#topHRU - threshold optimization for HRUs in SWAT
######*Michael Strauch, Christoph Schürz, and Robert Schweppe*
######Contact: michael.strauch@ufz.de, DOI: https://doi.org/10.5281/zenodo.154379

### About
topHRU calculates the average Relative Error of Aggregation (aREA) for different levels of input data aggregation in the Soil and Water Assessment Tool (SWAT). Input data aggregation in SWAT is based on HRU thresholds for land use, soil, and slope. topHRU allows identifying pareto-optimal threshold combinations (minimal spatial error for a given number of HRUs) to minimze the trade-off between computation time and model error.

### Installing the topHRU package
To install the package please execute following lines in R:


```r
req_pckg <- c("abind", "devtools", "emoa", "ggplot2", "plotly")
pckg_installed <- req_pckg %in% rownames(installed.packages())
if(any(pckg_installed == FALSE)){
  install.packages(req_pckg[!pckg_installed])
}
rm(req_pckg, pckg_installed)
devtools::install_git("https://github.com/michstrauch/TopHRU")
```

### Input data
As INPUT you need the "hrus" table from the "project_name.mdb" from a SWAT project database imported as data.frame in R database. The hrus table will become available AFTER the definition of HRUs in an ArcSWAT project WITHOUT applying thresholds 
(i.e. 0 for land use, soil, and slope).

### Minimum example
The package contains the template dataset hru_data. 
To see the structure of the required table you can compare with the template dataset:

```r
library(topHRU)
View(hru_data)
```


To run an HRU analysis for the template dataset run:

```r
hru_analysis <- topHRU(hru_data)
```

The resulting list holds the complete results of the HRU analysis,

```r
hru_analysis$result_all
```

a reduced result table only showing the pareto-optimal threshold combinations,

```r
hru_analysis$result_nondominated
```

and an interactive visualization of the dominated and nondominated threshold combinations that supports the user in finding an adequate threshold combination for the respective project:

```r
hru_analysis$pareto_plot
```
