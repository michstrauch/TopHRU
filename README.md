# TopHRU
Threshold optimization for HRUs in SWAT

This R-Script includes a function to calculate the spatial error (aREA - average Relative Error of Aggregation) 
for thousands of different SWAT input data aggregation levels based on HRU thresholds for land use, soil, and 
(optional) slope and identifies pareto-optimal solutions to minimze the trade-off between computation time and 
spatial error.
As INPUT you need the "hrus" table exported as txt-file from your SWAT project database (including column names 
in the first line) AFTER you have defined HRUs in your SWAT-GIS interface (e.g. ArcSWAT) WITHOUT applying thresholds 
(i.e. 0 for land use, soil, and slope).

Authors: Robert Schweppe, Michael Strauch

Contact: michael.strauch@ufz.de
