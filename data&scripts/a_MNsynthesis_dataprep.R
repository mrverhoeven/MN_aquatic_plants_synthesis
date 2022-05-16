#'---
#' title: "Statewide Synthesis - Data Prep"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull in data for PI surveys, Secchi clarity, Lake
#' attributes, management data, and sync them into a dataset to be used to
#' evaluate impacts of invasive plants in MN Lakes and the effects (invader 
#' control & native recovery) of control of those plants. 
#' 



#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
# Load Libraries
library(data.table) # remotes::install_github("Rdatatable/data.table")
library(ggplot2)
library(stringr)
library(sf)
library(vegan)
library(gridExtra)
library(dplyr)
library(tidyr)

# library(maps)
# library(rgdal)
# library(ggsn)
# library(moments)
# library(shiny)
# library(plotly)
# library(ggspatial)
# library(broom)
# library(woodson)
# 



# load in functions -------------------------------------------------------
f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}


# load in data -------------------------------------------------
plants <- fread(input = "data&scripts/data/input/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers

#Secchi data
secchi <- fread(input = "data&scripts/data/input/AllSecchi_plus_ShallowLakesSecchi.csv") #import, dropping the exported row numbers

#Public waters
pwi_l <- st_read(dsn = "data&scripts/data/input/shp_water_dnr_hydrography", layer = "dnr_hydro_features_all")
# pwi_r <- readOGR(dsn = "data&scripts/data/input/shp_water_mn_public_waters", layer = "public_waters_watercourses_delineations")

#species statuses
rte <- fread(input = "data&scripts/data/input/2013_dnr_plant_checklist_web.csv")




# run dependencies, respect order -------------------------------------------------------------

source(file = "data&scripts/a1_MNsynthesis_dataprep_georef.R")# will run the georef cleaning and connecting of the data set, and takes about 2 minutes to run. After run, pwi_l and plants can be linked on the shared "order_ID" column

source(file = "data&scripts/a2_MNsynthesis_dataprep_rakeabund.R") # will clean up the rake abundaces, scaling them to the 1-3 scale

source(file = "data&scripts/a3_MNsynthesis_dataprep_mgmtdat.R") # will load and clean management data and leave in WS a mgmtdata file


# summarize whats there ---------------------------------------------------

str(plants) #what data formats?
names(plants) #field names

plants[ , length(unique(SURVEY_ID)) , ] #how many surveys in all?
plants[ INDATABASE == T , length(unique(SURVEY_ID))] #how many surveys do we have the data in our db for?
plants[ , length((unique(DOW))) , ] #how many lake in all?
plants[ , length(unique(YEAR)) , ] #how many years of data?
plants[ , length(unique(POINT_ID)),] #how samples pulled from the lake?
plants[!is.na(TAXON) , length(unique(OBS_ID))] # how many times was a plant identified in these data? 


#' Lets see how many surveys we have been given by each contributor
plants[ , unique(DATASOURCE) ,] #we need to clean these up
plants[ , DATASOURCE := tolower( word(DATASOURCE, sep = fixed("_"))) , ] #DO
plants[DATASOURCE == "fieldseth" , DATASOURCE := "eric fieldseth"  , ] # Combine
plants[DATASOURCE == "crwd" , DATASOURCE := "britta belden"  , ] # Combine
plants[DATASOURCE == "fieldseth" , DATASOURCE := "eric fieldseth"  , ] # Combine

ggplot(plants[ , .N, .(SURVEY_ID, DATASOURCE, INDATABASE)], aes(DATASOURCE, fill = INDATABASE))+
  geom_bar(stat = "count", position = "stack" )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(label = "n surveys by contributor")

#' The database has all the surveys we know exist for MN in it, including those
#' for which we do not have the data. It is often useful to snip those no-data
#' ones off right away to avoid running any calcs using all those rows w/o any
#' species data.
#' 

missing_data_surveys <- plants[ INDATABASE == F] 

plants <- plants[INDATABASE == T]

#' Now "plants" is only those surveys for which we were able to gather and
#' collate the data. 

#how many surveys and how many points were sampled in each?
plants[ , length(unique(POINT_ID)) , SURVEY_ID]
hist(plants[ , length(unique(POINT_ID)) , SURVEY_ID][ , V1])

#how many unique TAXA?
unique(plants$TAXON)
# N taxa per survey:
plants[ , .("Ntaxa" = length(unique(TAXON))) , SURVEY_ID] #if you want to name cols on the fly you need to wrap in .() which makes list from them 
hist(plants[ , length(unique(TAXON)) , SURVEY_ID][ , V1], main = "N taxa per survey")
hist(plants[ , length(unique(TAXON)) , POINT_ID][ , V1], main = "N taxa per point")

#' That is a good intro to the structure of these data. Some things to keep in 
#' mind:
#'  - Each row is an observation of a species at a point. 
#'  - If there was no species observed, the row is a placeholder for that point,
#'  and that row will have NO_VEG_FOUND set to TRUE, but TAXON and density will 
#'  be blank
#'  - If there are no data for a survey, that row is a placeholder for the
#'  survey, and all of the point-level data will be NAs


# build up a survey level table -------------------------------------------


surveys <- plants[ , .(tot_n_samp = length(unique(POINT_ID)))  , SURVEY_ID ]

# #richness, ensuring we aren't counting NAs:
# plants[ , length(unique(TAXON))  , SURVEY_ID ]
# plants[!is.na(TAXON) , length(unique(TAXON))  , SURVEY_ID ]
# plants[ , sum(is.na(TAXON)), SURVEY_ID][V1<1]#not every survey has at least 1 NA in the TAXON column
# plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID]
# 
# #we can subtract 1 if the taxon column contained NA
# plants[ , length(unique(TAXON))   , SURVEY_ID ][ , V1] - plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID][,V1]

#add richness to the surveys dataset
surveys[  , taxa_richness := #take the "taxon count" and subtract one if the survey includes NAs (see next two lines)
            plants[ , length(unique(TAXON))   , SURVEY_ID ][ , V1]-# ("total richness", but counts NAs as a taxon) minus
            plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID][,V1],]#  (each survey get a 0 if no NAs or a 1 if contains NA's)

# extent of vegetation in survey (prop vegetated)
# plants[NO_VEG_FOUND == F, length(unique(POINT_ID)) , SURVEY_ID ]
# plants[!is.na(TAXON), length(unique(POINT_ID)) , SURVEY_ID ]

surveys <- merge(surveys,plants[!is.na(TAXON), .(n_points_vegetated=length(unique(POINT_ID))) , SURVEY_ID ], by = "SURVEY_ID", all.x = TRUE)[is.na(n_points_vegetated), n_points_vegetated := 0 ]

surveys[ , prop_veg := n_points_vegetated/tot_n_samp ,]


#create a plant observation matrix (species abund by survey)
plants[!is.na(TAXON) , .("count" = length(unique(POINT_ID))) , .(SURVEY_ID,TAXON)]
a <- dcast(plants[!is.na(TAXON) , .("count" = length(unique(POINT_ID))) , .(SURVEY_ID,TAXON)], SURVEY_ID ~ TAXON, value.var = "count", fill = 0)


#diversity metrics go here
names(a)
a[ , shannon_div := diversity(a[,2:236],index = "shannon") ]
a[ , simpsons_div := diversity(a[,2:236],index = "simpson") ]
setcolorder(a, neworder = c(1,237,238))


# depth surveyed stats:
surveys <- surveys[plants[ !is.na(DEPTH_FT), .("max_depth_surveyed" = max(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
surveys <- surveys[plants[ !is.na(DEPTH_FT), .("min_depth_surveyed" = min(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
surveys <- surveys[plants[ !is.na(DEPTH_FT), .("mean_depth_surveyed" = mean(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
surveys <- surveys[plants[ !is.na(DEPTH_FT), .("median_depth_surveyed" = median(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
surveys <- surveys[plants[ !is.na(DEPTH_FT), .("IQR_depth_surveyed" = IQR(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
# summary(surveys)#note we dropped the two surveys with no depth data:
# plants[ !SURVEY_ID %in% plants[!is.na(DEPTH_FT) , .N  ,  SURVEY_ID][,SURVEY_ID], ]

#vegetated depths data
#max depth vegetated:
plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT)) , SURVEY_ID]
surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT, na.rm = T)) , SURVEY_ID] , by = "SURVEY_ID" , all.x =TRUE )

#other depth vegetated stats:
surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("min_depth_vegetated" = min(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("mean_depth_vegetated" = mean(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("median_depth_vegetated" = median(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("IQR_depth_vegetated" = IQR(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )

#species matrix for lakes
surveys <- merge(surveys, a, by = "SURVEY_ID", all.x = T)
f_dowle3natozeros(surveys, names(a))
summary(surveys[,1:17])

#append lake data (basic data from plants db) to these
names(plants)

surveys <- merge(plants[ , .("nobs" = .N) , .(SURVEY_ID, DATASOURCE, LAKE_NAME, DOW, DATESURVEYSTART, SUBBASIN, MULTIPARTSURVEY, order_ID) ],surveys,  by = "SURVEY_ID")

summary(surveys)

names(surveys) <- gsub(" ", "_", gsub( "\\(", "_", gsub( "\\)", "_", names(surveys))))

#get dates squared away in surveys
surveys[ ,sort(unique(DATESURVEYSTART)),]

surveys[ , DATESURVEYSTART := as.Date(DATESURVEYSTART, format = "%m/%d/%Y" ),]

surveys[ , hist(month(DATESURVEYSTART)) ,]


# add a CLP metric to summer surveys where CLP Spring abund was me --------

#put a CLP index into the peak surveys:
early_clpsurveys <- surveys[month(DATESURVEYSTART) %in% c(3,4,5,6) , .(SURVEY_ID, DOW, year = year(DATESURVEYSTART), LAKE_NAME, SUBBASIN, Potamogeton_crispus,DATESURVEYSTART, n_points_vegetated, tot_n_samp)  , ]

early_clpsurveys[ , potcri_early_vegdfoc := Potamogeton_crispus/n_points_vegetated ]

surveys[ , year := year(DATESURVEYSTART) , ]

summer_surveys <- surveys[yday(DATESURVEYSTART) > 166 & yday(DATESURVEYSTART) < 274 , , ]

summer_surveys <- summer_surveys[early_clpsurveys, on = .(DOW=DOW, year=year, LAKE_NAME=LAKE_NAME, SUBBASIN=SUBBASIN, DATESURVEYSTART = DATESURVEYSTART ), nomatch = NA, mult = "first" ]



nrow(merge(summer_surveys, early_clpsurveys, by = c("DOW", "year", "LAKE_NAME", "SUBBASIN" ), suffixes = c(".summer", ".spring")))

#merge mgmt data on dows & years
mgmtdata[ , p_dow := round(downum/100, 0) ,]
summer_surveys[ , p_dow := round(DOW/100, 0) , ]

summer_surveys <- merge(summer_surveys, mgmtdata, by.x = c("DOW", "year"), by.y = c("downum", "year"), all.x = T)

summer_surveys[is.na(clp_targeted), clp_targeted := F ]

summer_surveys[is.na(ewm_targeted), ewm_targeted := F]


#summary stats

summer_surveys[ , summary(clp_targeted) ,]
summer_surveys[ , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum > 0 , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum == 0 & ewm_targeted == T , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART.x )  ,]

# visualize "effects"
ggplot(summer_surveys[potcri_early_vegdfoc > 0], aes(potcri_early_vegdfoc, shannon_div, group = clp_targeted))+
  geom_point(aes(color = clp_targeted), alpha = 0.4)+
  geom_smooth(aes(color = clp_targeted), method = "lm")

ggplot(summer_surveys[Myriophyllum_spicatum > 0], aes(Myriophyllum_spicatum/n_points_vegetated.x, shannon_div, group = ewm_targeted))+
  geom_point(aes(color = ewm_targeted), alpha = 0.4)+
  geom_smooth(aes(color = ewm_targeted), method = "lm")

surveys[ , .N , .(DOW, year) ]
surveys[str_detect(LAKE_NAME, "minnet")]
  
y_secchi <- secchi[ , .("mean_secchi" = mean(Secchi_m)) , .(DOW, year(Date))] 

y_secchi[ , DOW := as.numeric(DOW)]

summer_surveys <- merge(summer_surveys, y_secchi, by.x = c("DOW", "year"), by.y = c("DOW", "year"), all.x = T)

ggplot(summer_surveys, aes(mean_secchi, shannon_div))+
  geom_point(aes(color = Myriophyllum_spicatum/n_points_vegetated.x), alpha = 0.4)+
  geom_smooth(method = "lm")













