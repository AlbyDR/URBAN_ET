# URBAN ET
Modelling of evapotranspiration in urban environments

This page aims to share R codes to model urban ET using the Soil-Canopy-Observation of Photosynthesis and Energy fluxes (SCOPE). A case study using open-access data for Berlin is used throghout the repository. SCOPE code is originally in MATLAB, but it will be run through the R package rSCOPE (see more at https://github.com/AlbyDR/rSCOPE).


### **The codes are divided into:**


**1- Model inputs collection and preprocessing (R)**

      1.1 Meteorological station data (DWD - German Climate Data Center)

      1.2 LAI derived Remote Sensing Data (Copernicus)

      1.3 Vegetation height and vegetation fraction (Berlin Environmental Atlas)

      1.4 SCOPE input parameters preparetion (pixel timeseries)

      1.5 Eddy Covariance data cleaning and footprints for validation (TUB UCO)
 
   
   
**2- Modelling (Matlab through R)**

      2.1 Setting and SCOPE input files especification  

      2.2 Run SCOPE

      2.3 Getting the prediction
      
      2.4 Correct the prediction to urban environment
      
      2.5 Model accuracy assessment with EC data (if available)
      
      2.6 Simulated climate change scenarios (Sensitivity analysis)
     
   
   
   
**3- Results and Model accuracy assessment (R)**

         3.1 Mapping ET

         3.2 Creating the cooling service indices (Greening, Evapotranspirative and radiative effects)

         3.3 Generate nice plots, maps and figures
