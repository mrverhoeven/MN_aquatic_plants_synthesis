#'---
#' title: "Rare Species Occurrences"
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



# load in functions -------------------------------------------------------


# load in data -------------------------------------------------

#observation level data
plants <- fread(input = "data&scripts/data/input/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers

#Secchi data
secchi <- fread(input = "data&scripts/data/input/AllSecchi_plus_ShallowLakesSecchi.csv") #import, dropping the exported row numbers

#Public waters
pwi_l <- st_read(dsn = "data&scripts/data/input/shp_water_dnr_hydrography", layer = "dnr_hydro_features_all")
# pwi_r <- readOGR(dsn = "data&scripts/data/input/shp_water_mn_public_waters", layer = "public_waters_watercourses_delineations")

#species status
rte <- fread(input = "data&scripts/data/input/2013_dnr_plant_checklist_web.csv")

#how many rte observations? 1785
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], , ]

#how many lakes do these rep? 122
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .N , DOW ]

#how many acres are the lakes
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(square_mi = first(acres)/640) , DOW ][, hist(square_mi )]

#who contributed the data? n observations per person
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(n_observations = .N, n_DOW = length(unique(DOW)), n_surveys = length(unique(SURVEY_ID)) ) , DATASOURCE ]

#who contributed the data? n surveys per person
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], length(unique(SURVEY_ID)) , DATASOURCE ]

#do the data have local georefs?
summary(plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(X,Y,NORTHING,EASTING,LATITUDE,LONGITUDE, UTMX, UTMY) ,])

#drop unused spatial cols
summary(plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(LATITUDE,LONGITUDE, UTMX, UTMY) ,])
#for each plant, a # of observations, # of waterbodies, size distro of waterbodies
plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(n_obs = .N, n_surveys = length(unique(SURVEY_ID)), n_DOW = length(unique(DOW)), min_square_mi = min(acres)/640, mean_square_mi = mean(acres)/640, mean_square_mi = mean(acres)/640)   , . (TAXON)]  


fwrite(
  plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(n_obs = .N, n_surveys = length(unique(SURVEY_ID)), n_DOW = length(unique(DOW)), min_square_mi = min(acres)/640, mean_square_mi = mean(acres)/640, IQR_square_mi = IQR(acres)/640)   , . (TAXON)]  ,
  file = "data&scripts/data/output/rare_plant_occ.csv"
)

fwrite(
  plants[ TAXON %in% rte[Rarity_Status != "" , MN_DNR_Scientific_Name,], .(n_observations = .N, n_DOW = length(unique(DOW)), n_surveys = length(unique(SURVEY_ID)) ) , DATASOURCE ], 
  file = "data&scripts/data/output/rare_obs_contrib.csv"
)









