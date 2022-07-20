# URBAN ET
Modelling and mapping evapotranspiration in urban environments

This page aims to share R codes to model urban ET using the Soil-Canopy-Observation of Photosynthesis and Energy fluxes (SCOPE). A case study using open-access data for Berlin is used throghout the repository. SCOPE code is originally in MATLAB, but it will be run through the R package rSCOPE (see more at https://github.com/AlbyDR/rSCOPE).


### **The codes are divided into:**

**1- Model inputs collection and preprocessing (R)**

      1.1 Download meteorological station data (DWD - German Climate Data Center)
      
      1.2 Interpolation of the meteorological data

      1.3 LAI derived Remote Sensing Data (Copernicus)

      1.4 Vegetation height and vegetation fraction (Berlin Environmental Atlas)

      1.5 Eddy Covariance data cleaning and footprints for validation (TUB UCO)
 
   
   
**2- Modelling (Matlab through R)**

      2.1 SCOPE input parameters preparetion (pixel timeseries)  

      2.2 Run the SCOPE model

      2.3 Getting the prediction
      
      2.4 Simulated climate change scenarios (Sensitivity analysis)
     
   
   
   
**3- Results and Model accuracy assessment (R)**

         3.1 Correct the prediction to urban environment and map ET and cooling services (Greening, Evapotranspirative and radiative effects)
      
         3.2 Model accuracy assessment with EC data (if available)

         3.3 Generate nice plots, maps and figures
         
         
#### References:
Rocha, A. D., Vulova, S., Meier, F., Förster, M., & Kleinschmit, B. (2022). Mapping evapotranspirative and radiative cooling services in an urban environment. SSRN Electronic Journal, 85. https://doi.org/10.2139/ssrn.4089553.

Duarte Rocha, A., Vulova, S., van der Tol, C., Förster, M., and Kleinschmit, B.: Modelling hourly evapotranspiration in urban environments with SCOPE using open remote sensing and meteorological data, Hydrol. Earth Syst. Sci., 26, 1111–1129, https://doi.org/10.5194/hess-26-1111-2022, 2022.

#### Package repository
The codes and R package for the input pre-processing, modelling and mapping is available in the GitHub https://github.com/AlbyDR/URBAN_ET and https://github.com/AlbyDR/rSCOPE

Duarte Rocha, A.: AlbyDR/rSCOPE: rSCOPE v1.0 (Evapotranspiration), Zenodo [code], https://doi.org/10.5281/zenodo.6204580, 2022.

#### Data repository
Duarte Rocha, A. (2022). Berlin Evapotranspiration and Cooling Services. https://doi.org/10.14279/depositonce-15870

