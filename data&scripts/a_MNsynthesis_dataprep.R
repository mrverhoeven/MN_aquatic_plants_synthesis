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
library(janitor)
library(lme4)
library(sjPlot)
library(mediation)
library(ggpubr)
library(EnvStats)
library(lmerTest)
library(merTools)
library(rstanarm)

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

watersheds_huc8 <- st_read(dsn = "data&scripts/data/input/shp_geos_dnr_watersheds", layer = "dnr_watersheds_dnr_level_04_huc_08_majors")

#species statuses
rte <- fread(input = "data&scripts/data/input/2013_dnr_plant_checklist_web.csv")

#collaborator inputs
coll_edits <- fread(input = "data&scripts/data/input/Edited_post_contrib_feedback.csv")



# run dependencies, respect order -------------------------------------------------------------


#collaborator corrections
source(file = "data&scripts/a0_1_MNsynthesis_dataprep_collabcorrections.R")# will bring in the corrections offered by our collabs and merge into the plants db those corrections


#georef
source(file = "data&scripts/a1_MNsynthesis_dataprep_georef.R")# will run the georef cleaning and connecting of the data set, and takes about 2 minutes to run. After run, pwi_l and plants can be linked on the shared "order_ID" column

#rake abunds
source(file = "data&scripts/a2_MNsynthesis_dataprep_rakeabund.R") # will clean up the rake abundaces, scaling them to the 1-3 scale

#secchi data
source(file = "data&scripts/a3_MNsynthesis_dataprep_secchidata.R") # NEEDS ENLIGHTENED COMMENT



# on the fly fixes --------------------------------------------------------

plants[TAXON == "Mitellopsis", TAXON := "Nitellopsis"]


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
plants[ , unique(SURVEY_DATASOURCE) ,] #we need to clean these up
# survey contribution
ggplot(plants[ , .N, .(SURVEY_ID, SURVEY_DATASOURCE, INDATABASE)], aes(SURVEY_DATASOURCE, fill = INDATABASE))+
  geom_bar(stat = "count", position = "stack" )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(label = "n surveys by contributor")

# point contributions
ggplot(plants[INDATABASE==T , .N, .(POINT_ID, SURVEY_DATASOURCE, INDATABASE)], aes(SURVEY_DATASOURCE))+
  geom_bar(stat = "count", position = "stack" )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(label = "n points by contributor")

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
survey_species_matrix <- dcast(plants[!is.na(TAXON) , .("count" = length(unique(POINT_ID))) , .(SURVEY_ID,TAXON)], SURVEY_ID ~ TAXON, value.var = "count", fill = 0)



# survey level diversity metrics ------------------------------------------


#native div (exclude typha ang, typha glauca, M spicatum, L salicaria, P crispus, N minor)

rte <- clean_names(rte)
natcols <- names(survey_species_matrix)[!names(survey_species_matrix)%in%c(rte[native_status == "I", mn_dnr_scientific_name], "Nitellopsis", "Typha glauca", "SURVEY_ID")]

#diversity metrics go here
names(survey_species_matrix)
survey_species_matrix[ , shannon_div := diversity(survey_species_matrix[,2:236],index = "shannon") ]
survey_species_matrix[ , simpsons_div := diversity(survey_species_matrix[,2:236],index = "invsimpson") ]

survey_species_matrix[ , shannon_div_nat := diversity(survey_species_matrix[,.SD, .SDcols = natcols],index = "shannon") ]
survey_species_matrix[ , simpsons_div_nat := diversity(survey_species_matrix[,.SD, .SDcols = natcols],index = "invsimpson") ]

survey_species_matrix[simpsons_div_nat == Inf, simpsons_div_nat := 0]


survey_species_matrix[ ,  nat_richness := rowSums(survey_species_matrix[ , .SD, .SDcols = natcols] > 0), ]


# depth stats ----------------------------------------------------

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


# bring species matrix & lake data back to the survey data -------------------------

#species matrix for surveys
surveys <- merge(surveys, survey_species_matrix, by = "SURVEY_ID", all.x = T)
f_dowle3natozeros(surveys, names(survey_species_matrix))
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
surveys[ , year := year(DATESURVEYSTART) , ]


# add secchi data to these survey metrics ---------------------------------

# rescue the secchi data from the plants db for these surveys

surveys <- merge(surveys,plants[ , .("Secchi.m" = mean(Secchi_m.mean)), SURVEY_ID], by.x = "SURVEY_ID", by.y = "SURVEY_ID")


# point level rake abund --------------------------------------------------

# plants[!is.na(TAXON) , length(unique(TAXON)) , POINT_ID ]

plants_rakeabund_wide <- dcast(plants[!is.na(REL_ABUND_CORRECTED)], SURVEY_ID + POINT_ID + Secchi_m.mean + DEPTH_FT ~ TAXON, value.var = c("REL_ABUND_CORRECTED"), fun.aggregate = last, fill = 0)

#calculate diversity metrics for each rake throw
rake_natcols <- names(plants_rakeabund_wide)[names(plants_rakeabund_wide)%in%natcols]
plants_rakeabund_wide[ , shannon_div := diversity(plants_rakeabund_wide[ , c(3:138)],index = "shannon") ]
plants_rakeabund_wide[ , simpsons_div := diversity(plants_rakeabund_wide[ , c(3:138)],index = "invsimpson") ]
plants_rakeabund_wide[ , shannon_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "shannon") ]
plants_rakeabund_wide[ , simpsons_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "invsimpson") ]


plants_rakeabund_wide[ ,  richness := rowSums(.SD > 0), .SDcols = c(3:138) ]
plants_rakeabund_wide[ ,  nat_richness := rowSums(.SD > 0), .SDcols = rake_natcols ]

summary(plants_rakeabund_wide$`Potamogeton crispus`)

# rename data for easy plotting
plants_rakeabund_wide <- clean_names(plants_rakeabund_wide)


# point level p/a  -------------------------------------------------

plants_occurrence_wide <- dcast(plants, SURVEY_ID + POINT_ID + NO_VEG_FOUND + Secchi_m.mean + DEPTH_FT ~ TAXON, value.var = "INDATABASE", fun.aggregate = last, fill = 0)

#diversity metrics (only have richness with p/a, no "evenness"):
point_natcols <- names(plants_occurrence_wide)[!names(plants_occurrence_wide)%in%c(rte[native_status == "I", mn_dnr_scientific_name], "Nitellopsis", "Typha glauca", "SURVEY_ID", "POINT_ID", "richness", "NA", "DEPTH_FT", "Secchi_m.mean", "NO_VEG_FOUND")]
plants_occurrence_wide[ ,  richness := rowSums(.SD > 0), .SDcols = c(7:241) ]
plants_occurrence_wide[ ,  nat_richness := rowSums(.SD > 0), .SDcols = point_natcols ]


# calculate point level light avail -------------------------------------------------

plants_rakeabund_wide[ , proplight := exp(-(log(10)/secchi_m_mean)*(depth_ft/3.2804)) ]

plants_occurrence_wide[ , proplight := exp(-(log(10)/Secchi_m.mean)*(DEPTH_FT/3.2804)) ]


# species pools -----------------------------------------------------------

#' We have super awesome species pool data!
#' From the smallest (point-- plants_rakeabund_wide or plants_occurrence_wide)
#' scale we have a species abundance matrix that can be treated as a product of
#' the species pool above it (whole survey/lake), which we also have an
#' abundance matrix for! We can also move up to the landscape scale, building 
#' species abundance matricies by aggregating these lake data.
#' 
#' For example, we can do as described above (compressing matricies to richness
#' for viz) and aggregate to the HUC-8 watershed level. 
#' 
#' 

#first add the lake level richness from each survey to the point rake abund data
plants_rakeabund_wide[ , surveyrichness := surveys[match(plants_rakeabund_wide[ ,survey_id ,], surveys[, SURVEY_ID, ]), taxa_richness] ,]

  ggplot( data = plants_rakeabund_wide,
          aes(jitter(surveyrichness), jitter(nat_richness)))+
    geom_point(alpha = .05)

# create a watershed level species matrix:
  #check keys
  pwi_l$order_ID
  
  plants$order_ID
  
  plants[ , watershed := pwi_l[match(plants[ , order_ID ,],pwi_l[ , order_ID ,]), major , ],]
  
  plants[ , length(unique(POINT_ID)) , watershed]
  
  watersheds <- plants[ , .("n_points" = length(unique(POINT_ID))) , watershed]
  
  watersheds <- merge(watersheds, 
        plants[!is.na(TAXON) , .("n_species" = length(unique(TAXON))) , watershed],
        by = "watershed", 
        all.x = T)
  
  
  watershed_occurrence_wide <- dcast(plants, watershed ~ TAXON, value.var = "INDATABASE", fun.aggregate = sum, fill = 0)
  
  watershed_occurrence_wide <- merge(watersheds, 
                      watershed_occurrence_wide,
                      by = "watershed", 
                      all.x = T)
  
  plants_rakeabund_wide[ , watershed := plants[match(plants_rakeabund_wide[ ,survey_id ,], plants[, SURVEY_ID, ]), watershed] ,]
  
  plants_rakeabund_wide[ , watershedrichness := watershed_occurrence_wide[match(plants_rakeabund_wide[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), n_species] ,]
  
  # watershed richness as the predictor: 
  
  ggplot( data = plants_rakeabund_wide,
          aes(jitter(watershedrichness), jitter(nat_richness)))+
    geom_point()
  
  surveys[ , watershed := plants[match(surveys[ , SURVEY_ID ,], plants[, SURVEY_ID, ]), watershed] ,]
  surveys[ , watershedrichness := watershed_occurrence_wide[match(surveys[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), n_species] ,]
  
  ggplot( data = surveys,
          aes(watershedrichness, nat_richness))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("Survey Richness")+
    xlab("HUC-8 Watershed Richness")+
    theme_bw()

  # Get watershed ENSpie

  names(watershed_occurrence_wide) %in% natcols
  
  watershed_occurrence_wide[ , simpson_div := diversity(.SD,index = "invsimpson" ) , .SDcols = c(names(watershed_occurrence_wide) %in% natcols)]
  
  hist(watershed_occurrence_wide$simpson_div)
  
  plants_rakeabund_wide[ , watershedsimpson := watershed_occurrence_wide[match(plants_rakeabund_wide[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), simpson_div] ,]
  
  plants_rakeabund_wide[ , surveysimpson := surveys[match(plants_rakeabund_wide[ ,survey_id ,], surveys[, SURVEY_ID, ]), simpsons_div_nat] ,]
  
  surveys[ , watershedsimpson := watershed_occurrence_wide[match(surveys[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), simpson_div] ,]
  
  
  #redo all species pool plots:
  point_pools <- ggplot( data = plants_rakeabund_wide,
          aes(surveysimpson, simpsons_div_nat))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("Point-scale ENSpie")+
    xlab("Lake-scale ENSpie")+
    theme_bw()
  
  lake_pools <- ggplot( data = surveys,
          aes(watershedsimpson, simpsons_div_nat))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("Lake-scale ENSpie")+
    xlab("Watershed-scale ENSpie")+
    theme_bw()
   ggarrange(
     point_pools,
     lake_pools
   )
  
  
  # cursor catcher
