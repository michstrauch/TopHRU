# TopHRU
Threshold optimization for HRUs in SWAT

TopHRU includes a function to calculate the spatial error measure aREA (average Relative Error of Aggregation) for different input data aggregation levels  of the Soil and Water Assessment Tool (SWAT). Input aggregation in SWAT is based on HRU thresholds for land use, soil, and (optional) slope. TopHRU allows identifying pareto-optimal threshold combinations (minimal spatial error for a given number of HRUs) to minimze the trade-off between computation time and model error.
As INPUT you need the "hrus" table exported as txt-file from your SWAT project database (including column names 
in the first line) AFTER you have defined HRUs in your SWAT-GIS interface (e.g. ArcSWAT) WITHOUT applying thresholds 
(i.e. 0 for land use, soil, and slope).

Authors: Robert Schweppe, Michael Strauch

Contact: michael.strauch@ufz.de
