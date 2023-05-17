
surveys <- fread("data&scripts/data/output/surveys_EWMecon_20Feb2022.csv")

colnames(surveys)

#col 1 - a unique ID for each survey in our dataset
#col 2 - a dept. of waters number for the lake where survey was done
#col 3 - The name of the subbasin that was surveyed (if whole-lake was not surveyed)
#col 4-54 columns from MN Hydrography dataset (https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_dnr_hydrography/metadata/dnr_hydrography_all_water_features.html)
#col 55 Who shared the survey with our team
#col
#...

#subset to only columns that Adrianna needs:

cols <-  c("dowlknum", #lake ID
           "DATESURVEYSTART", #survey Date
           "GDD_10C", #mean annual Growing Degree Days for that lake (Shyam is this right? mean?)
           "max_depth_dow", # max depth of lake
           "year_EWM_infes", #year lake was designated infested
           "roads_500m_mperha", #density of roads (meters/ha) within 500m buffer around lake
           "ewm_percent_alltimevegdlittoral" # abundance of ewm (EWM count/littoral sample points) here, littoral is defined as the max depth that plants were ever observed at 
                      )

surveys[ , .SD , .SDcols = cols]
