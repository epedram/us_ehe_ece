# GridEX: A dataset for assessing the area-wide extreme heat and cold exposure (AWEHE/AWECE) in the United States (2008-2023)

GridEX provides a grid-based measure of exposure to extreme heat and cold events. It leverages a data science pipeline to gather station-based climatological data and estimate fine-resolution surfaces of ambient (i.e., temperature of air at 2m above the land surface) and apparent temperature (i.e., the perceived temperature by humans which takes into account humidity and wind in addition to the actual air temperature) with 500x500 meter resolution.

This repository provides a suit of R scripts that help to replicating our data science pipeline and generating the dataset.

We implemented our informatics pipeline in the R Statistical Software (version 4.2.1) and utilized the "Tidyverse" suite. We used the "sf" library to manage vector spatial data processes 375 and leveraged the “terra” library to perform raster geoprocessing operations. We conducted the meteorological unit conversions through the “rnoaa” and “isdparser” packages and employed "naniar" and "visdat" packages to examine the quality of the collected raw data and to visualize the distribution of the missing values. We imputed missing values using methods implemented in the "imputeTS" library. We used the “gstat” package to perform all the spatial IDW interpolations.
