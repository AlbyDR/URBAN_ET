# URBAN ET
Modelling of evapotranspiration in urban environments

This page aims to share R codes to model urban ET using the Soil-Canopy-Observation of Photosynthesis and Energy fluxes (SCOPE). A case study using open-access data for Berlin is used throghout the repository. SCOPE code is originally in MATLAB, but it will be run through the R package rSCOPE (see more at https://github.com/AlbyDR/rSCOPE).

The codes are divided into:

1- Model inputs collection and preprocessing (R)
   1.1 Meteorological station data from DWD
   
   1.2 ICOS CO2 data
   
   1.3 EC Data / cleaning
   1.4 Atlas land cover maps
   1.5 Footprints
   1.6 RS Data
   1.7 SCOPE input parameters
   
2- Modelling (Matlab)
   2.1 Original code (link)
   2.2 Iniciation code (.m) and set parameters file
   2.3 Input files (filenames_, input_data_, and setoptions_)
   
3- Results and Model accuracy assessment (R)
   3.1 Read SCOPE outputs
   3.2 Apply the correction factor and calculate model metrics
   3.3 Analysis of the footprint results
   3.4 Plots, maps and figures
