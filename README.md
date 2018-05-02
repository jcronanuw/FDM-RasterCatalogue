PROJECT TITLE
FUELBED DYNAMICS MODEL (FDM) -- Variant: Fire Progression

Purpose
The purpose of these scripts is to generate maps during disturbance mapping sub-models in FDM. These maps can be imported into ArcGIS as a raster catalog file and animated to illustrate how FDM spreads disturbance across the simulated landscape. This variant of FDM serves to illustrate how FDM works. It is not required to run FDM for simulations. For simulations use the most recent version of FDM on the GitHub EglinAirForceBase repository. At some point this variant should be folded into FDM as an option that can be accessed through the input parameters. At that point this repository should be deleted.

Internal Project Reference.
Project Name: Detailed Fuelbed Characterization, mapping, and Future Fire Hazard Assessment for Eglin Air Force Base, FL
Project Short Name: Fuelbed Characterization & Mapping
Project Number: 2012_FCM_c

Author(s)(number indicates affiliation of organization listed below)
Jim Cronan(1)
Phone: 206-732-7873
Email: jcronan@fs.fed.us

Organization(s)
(1)
Pacific Wildland Fire Sciences Laboratory
400 North 34th Street, Suite 201
Seattle, WA 98122
Phone: 206-732-7800

Operation
Creating fire progression maps in ArcGIS is a three step process.
step 1) 
Open C:\Users\jcronan\Documents\GitHub\FDM-RasterCatalogue\step_1\FCM_IdentifyMapCoords.mxd and user the "Identify" tool to locate the pixel value (in _coordmaplmap) of the lower left corner of the cutout map you want to use for fire progression.
Use the "Measure" tool to record the dimensions (width (x) and height (y) in meters) of the cutout map you wish to create.
Enter these values in the input parameter file (C:\Users\jcronan\Documents\GitHub\FDM-RasterCatalogue\pre_inputs\sef_pre_parameters.csv) in the order listed above (row names are self explanatory).
For the last row in the sef_pre_parameters.csv file (run_no) enter a row number. This will be used in all output files produced in steps 2 and 3 and will allow you to store multiple sets of cutout maps.

step 2)
Run C:\Users\jcronan\Documents\GitHub\FDM-RasterCatalogue\step_2\sef_preFDMv2_fireProgression.r
This code will subset maps and create all input files needed to run the Fuelbed Dynamics Mode - Fire Progression Variant.
Enter values into the input paramters file (C:\Users\jcronan\Documents\GitHub\FDM-RasterCatalogue\pre_inputs\sef_pre_parameters.csv) as described above.

# FDM-RasterCatalogue
Version of FDM built to store small raster datasets at high frequency to show disturbance progression in ArcMap 

This version copied the FDM script from the EglinAirForceBase on September 25, 2017 (C:\Users\jcronan\Documents\GitHub\EglinAirForceBase\sef_FDM_v2.0.r).


 
