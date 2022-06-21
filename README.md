# URBAN ET
Modelling of evapotranspiration in urban environments

This page aims to share R codes to model urban ET using the Soil-Canopy-Observation of Photosynthesis and Energy fluxes (SCOPE). A case study using open-access data for Berlin is used throghout the repository. SCOPE code is originally in MATLAB, but it will be run through the R package rSCOPE (see more at https://github.com/AlbyDR/rSCOPE).

The codes are divided into:

1- Model inputs collection and preprocessing (R)

   1.1 Meteorological station data (DWD - German Climate Data Center)
   
   1.2 LAI derived Remote Sensing Data (Copernicus)
   
   1.3 Vegetation height and vegetation fraction (Berlin Environmental Atlas)
   
   1.4 SCOPE input parameters preparetion (pixel timeseries)
   
   1.5 Eddy Covariance data cleaning and footprints for validation (TUB UCO)
 
   
   
2- Modelling (Matlab through R)

   2.1 Original code (link)
   
   2.2 Iniciation code (.m) and set parameters file
   
   2.3 Input files (filenames_, input_data_, and setoptions_)
   
   
3- Results and Model accuracy assessment (R)

   3.1 Read SCOPE outputs
   
   3.2 Apply the correction factor and calculate model metrics
   
   3.3 Analysis of the footprint results
   
   3.4 Plots, maps and figures
