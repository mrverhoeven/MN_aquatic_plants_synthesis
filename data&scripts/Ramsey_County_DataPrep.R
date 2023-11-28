
# load pkgs ----------------------------------------------------------------

library(data.table)


# load in data -------------------------------------------------
# # 
# plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
# plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
# plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
surveys <- fread(file = "data&scripts/data/output/surveys_aqplants.csv")


surveys[DOW %in% c(62000000:63000000, 2000300, 82016700 ), .N , .(LAKE_NAME,year, DOW) ]


ramsey_co_plant_surveys <- surveys[DOW %in% c(62000000:63000000, 2000300, 82016700 ), c(1:4, 58:76, 308:320, 77:307  )   ]



names(ramsey_co_plant_surveys)

ramsey_co_plant_surveys[ ,.N ,]
ramsey_co_plant_surveys[ , .N , SURVEY_ID] 
ramsey_co_plant_surveys[SUBBASIN != "" ,  , ]

names(ramsey_co_plant_surveys[ , ,])

ramsey_co_plant_surveys[ , .( ), ]


#merge to invasion status
zm_inv_yr <- fread("E:\\Shared drives\\Hansen Lab\\RESEARCH PROJECTS\\Ramsey County project\\data\\zm_ramseycty.xlsx - Sheet1.csv")
ramsey_co_plant_surveys[ zm_inv_yr, on = .(DOW = dow_formatted), year_listed_ZM := `year listed` ]

ramsey_co_plant_surveys[, invaded_ZM =  , ]

new_plants <- readRDS("data&scripts/data/input/db_picharter_19Oct2023.rds")

fwrite( )


#Ellen Albright side notes

colnames(surveys[ , c(1:4, 58:76, 308:320, 77:307  ) , ])


surveys[ , mean(max_depth_surveyed) , SURVEY_DATASOURCE ]


setDT(new_plants)

new_plants[DOW %in% c(62000000:63000000, 2000300, 82016700 ), length(unique(SURVEY_START)) , .( DOW) ]

















