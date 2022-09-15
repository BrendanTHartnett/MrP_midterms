# MrP_midterms

Hi Professor Schaffner! Thank you so much for looking this over. Here's a general overview of the workflow.

FOR STATE MAPS — "state_model_works_mrp.R"

Survey data: "BLWwave13.csv" and "BLWwave14.csv"

Census data: "ACS_state_data.csv"

Map projection: "cb_2018_us_state_500k..." (need .shp, .shx, and .dbf to load)


___________

FOR CONGRESSIONAL MAPS — "congressional_model_works_mrp.R"

Survey data: "blw_geocoded_cong_dat.csv" (matched respondents to congressional districts by their zip code's centroid in ArcGIS)

Census data: "District_level_census_dat..." (need .shp, .shx, and .dbf to load)

      NOTE: States with 1 congressional district excluded, as their values are the same as in the state map and I will just import those polygons at the final stage once we think the model looks good. 
      
___________
      
      
RANDOM FOREST TEST FOR MODEL PREDICTIVE ACCURACY

Data: "model_prediction_classifier_dat.csv"

R script: "rf_classif_tesT_model.R"

Confusion matrix output: "screenshot..."
