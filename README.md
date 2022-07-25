# marshall_fires
AQ data repository for Marshall Firest project

## Files
- [Methods.docx](/Methods.docx): a file documenting our methods of analysis (hopefully for publication)
- [marshall0.qgz](/marshall0.qgz): QGIS map showing burn area, damaged & destroyed buildings, and sensors

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

### images
- outputs from the visualization.Rmd script, many of the images are from older iterations of the script

### intermediary_outputs
- [aq_data.csv](intermediary_outputs/aq_data.csv): created by purple_air_extraction.Rmd, has all of the air quality data
- [sensor_data_full.csv](intermediary_outputs/sensor_data_full.csv): created by purple_air_extraction.Rmd, has information on every sensor within our study area
- [corrected_AQ_data.csv](intermediary_outputs/corrected_AQ_data.csv): created by clean_AQ.Rmd, has the cleaned & corrected air quality data
