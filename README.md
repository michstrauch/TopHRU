#topHRU - threshold optimization for HRUs in SWAT
######*Michael Strauch, Christoph Schürz, and Robert Schweppe*
######Contact: michael.strauch@ufz.de, DOI: https://doi.org/10.5281/zenodo.154379

### About
topHRU calculates the average Relative Error of Aggregation (aREA) for different levels of input data aggregation within the Soil and Water Assessment Tool (SWAT). Input data aggregation in SWAT is based on HRU thresholds for land use, soil, and slope. topHRU allows identifying pareto-optimal threshold combinations (minimal spatial error for a given number of HRUs) to minimze the trade-off between computation time and model error.

![Evaluation of threshold combinations for a SWAT2012 project](https://github.com/chrisschuerz/SWATcoRe/blob/master/figures/topHRU.png)

### Installing the topHRU package
To install the package please execute following lines in R:


```r
install.packages(c("devtools","abind", "emoa", "dplyr", "ggplot2","miniUI","yaml"))
devtools::install_github(c("ropensci/plotly","michstrauch/TopHRU"))

```
### Minimum example
The package contains the template dataset hru_data. 
To see the structure of the required table you can compare with the template dataset:

```r
library(topHRU)
View(hru_demo)
```

#### Input data
As INPUT you need the "hrus" table from the "project_name.mdb" from a SWAT project database. The hrus table will become available AFTER the definition of HRUs in an ArcSWAT project WITHOUT applying thresholds (i.e. 0 for land use, soil, and slope).
You can export the hrus table either from a *.csv* file (therefore the hrus table must be extracted from the "project_name.mdb" and saved to "name.csv" prior to loading it in R. Otherwise you can load the table directly from the "project_name.mdb". Therefore the *RODBC* package must be installed and a **32 bit version** of R must be used.

```r
hru_table <- extract_hru("path/to/extracted/hrus.csv") 
#or
hru_table <- extract_hru("path/to/project_name.mdb") 
```
####Analysis
To run an HRU analysis for the template dataset run:

```r
hru_eval <- evaluate_hru(hru_demo)
```

The resulting list holds the complete results of the HRU analysis,

```r
hru_eval$result_all
```

and a reduced result table only showing the pareto-optimal threshold combinations,

```r
hru_eval$result_nondominated
```

####Visualization
*plot_pareto* provides two options for visualization. The default is an interactive visualization of the dominated and nondominated threshold combinations, where the function returns a *plotly* object. Such plot supports the user in finding an adequate threshold combination for the respective project is available with the following function call:

```r
plot_pareto(hru_analysis = hru_analysis, area_thrs = 0.1, hru_thrs = 2000)
```
For publication a ggplot object is more useful. Hence setting the parameter *interactive = FALSE* returns a static plot:


```r
plot_pareto(hru_analysis = hru_analysis, area_thrs = 0.1, hru_thrs = 2000, interactive = FALSE)
```
The two threshold parameters in the function define the positions of the dashed guide lines that seperate the data points. If these are not needed, no threshold values are provided with the function call.
