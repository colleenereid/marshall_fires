# marshall_fires
AQ data repository for Marshall Firest project

## Files
- [Methods.docx](/Methods.docx): a file documenting our methods of analysis (hopefully for publication)
- [marshall0.qgz](/marshall0.qgz): QGIS map showing burn area, damaged & destroyed buildings, and sensors
- [smoke-affected-parcels.qgz](/smoke-affected-parcels.qgz): QGIS project with complete Boulder County parcel data, and imported smoke data from the time period the fire was burning. TO DO: look at types of buildings on each parcel & more closely examine areas that were smoke-affected.

### code
Each .Rmd file has a corresponding .html file which is the knitted, complete version of that file.
- [purple_air_extraction.Rmd](/code/purple_air_extraction.Rmd): script used to download the Purple Air sensor data (NOT WORKING)
- [clean_AQ.Rmd](/code/clean_AQ.Rmd): script used to clean & correct the PA data
- [visualization.Rmd](/code/visualization.Rmd): script used to analyze and create visualizations of the data
- [code-help.docx](/code/code-help.docx): written instructions stepping through each of the 3 scripts explaining how they work

### fire_counties_PAs
Each file in this directory is the data for an individual sensor that was downloaded.

### GIS_inputs_destruction_fireboundary
- [Boulder_county_munis](GIS_inputs_destruction_fireboundary/Boulder_county_munis): shapefile holding the municipal boundaries for Boulder County
- [Boulder_county_Road_Map_Roads](GIS_inputs_destruction_fireboundary/Boulder_county_Road_Map_Roads): shapefile holding the Boulder County roads
- [Broomfield_Precincts](GIS_inputs_destruction_fireboundary/Broomfield_Precincts): shapefile holding the Broomfield city & county boundaries
- [fire_damage_raw_intermediate_files](GIS_inputs_destruction_fireboundary/fire_damage_raw_intermediate_files): damaged business & home input data before cleaning
- [output_damage_files](GIS_inputs_destruction_fireboundary/output_damage_files): damaged business & homes final data layers
- [Precincts](GIS_inputs_destruction_fireboundary/Precincts): shapefile for Boulder County voter precincts used for boundaries
- [Unincorporated_Boulder](GIS_inputs_destruction_fireboundary/Unincorporated_Boulder): shapefile for areas of Boulder outside of the city
- [Westminster_CityLimits](GIS_inputs_destruction_fireboundary/Westminster_CityLimits): shapefile for the boundaries of Westminster
- [WFIGS_-_Wildland_Fire_Perimeters_Full_History](GIS_inputs_destruction_fireboundary/WFIGS_-_Wildland_Fire_Perimeters_Full_History): shapefile of fires used to get the Marshall Fire boundary
- [Parcels/Parcels.zip](GIS_inputs_destruction_fireboundary/Parcels/Parcels.zip): shapefile of all Boulder County land parcels (needs to be unzipped)
- [Parcels/full_parcel_building_info.zip](GIS_inputs_destruction_fireboundary/Parcels/full_parcel_building_info.zip): the parcel data joined with the building information for each parcel provided by Boulder County
- [Parcels/parcel_smoke_data.zip](GIS_inputs_destruction_fireboundary/Parcels/parcel_smoke_data.zip): the parcels that are smoke-affected and the mean value of the PM2.5 concentration during the fire for each parcel
- [Parcels/smoke_affected_parcels.zip](GIS_inputs_destruction_fireboundary/Parcels/smoke_affected_parcels.zip): the full parcel data limited to only htose within the smoke boundary
- [Account_Parcels.csv](GIS_inputs_destruction_fireboundary/Account_Parcels.csv): the key to merge the building data to the parcel data
- [Buildings.csv](GIS_inputs_destruction_fireboundary/Buildings.csv): the building data for each parcel
- [smoke_affected.tiff](GIS_inputs_destruction_fireboundary/smoke_affected.tiff): raster export from the visualization.Rmd script of the areas that have >12 µg/m^3 PM2.5 concentration during the fire

### images
- outputs from the visualization.Rmd script, many of the images are from older iterations of the script

### intermediary_outputs
- [aq_data.csv](intermediary_outputs/aq_data.csv): created by purple_air_extraction.Rmd, has all of the air quality data
- [sensor_data_full.csv](intermediary_outputs/sensor_data_full.csv): created by purple_air_extraction.Rmd, has information on every sensor within our study area
- [corrected_AQ_data.csv](intermediary_outputs/corrected_AQ_data.csv): created by clean_AQ.Rmd, has the cleaned & corrected air quality data
