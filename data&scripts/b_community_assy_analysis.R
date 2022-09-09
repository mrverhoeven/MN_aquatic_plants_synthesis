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

# ben bolkers CI calc fn (https://github.com/bbolker/asaglmm/blob/master/papers/bolker_chap.rmd)
easyPredCIbinom <- function(model,newdata,alpha=0.05) {
  ## baseline prediction, on the linear predictor (log-odds) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],
                    newdata)
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link (logistic) function: could also use plogis()
  plogis <- model@resp$family$plogis
  ## construct 95% Normal CIs on the link scale and
  ##  transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  plogis(cbind(lwr=pred0-crit*pred.se,
               upr=pred0+crit*pred.se))
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

#managemnt data
source(file = "data&scripts/a3_MNsynthesis_dataprep_mgmtdat.R") # will load and clean management data and leave in WS a mgmtdata file

#secchi data

source(file = "data&scripts/a4_MNsynthesis_dataprep_secchidata.R") # will load and clean management data and leave in WS a mgmtdata file

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



# setcolorder(survey_species_matrix, neworder = c(1,237,238))

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


# add a CLP metric to summer surveys where CLP Spring abund was me --------

#put a CLP index into the peak surveys:
early_clpsurveys <- surveys[month(DATESURVEYSTART) %in% c(3,4,5,6) , .(SURVEY_ID, DOW, year = year(DATESURVEYSTART), LAKE_NAME, SUBBASIN, Potamogeton_crispus,DATESURVEYSTART, n_points_vegetated, tot_n_samp)  , ]

early_clpsurveys[ , potcri_early_vegdfoc := Potamogeton_crispus/n_points_vegetated ]

surveys[ , year := year(DATESURVEYSTART) , ]

#thin summer surveys to exclude pre june 15
summer_surveys <- surveys[yday(DATESURVEYSTART) > 151  , , ]

#append early clp metrics to these summer surveys:
summer_surveys <- merge(summer_surveys, early_clpsurveys, by = c("DOW", "year", "LAKE_NAME", "SUBBASIN"), suffixes = c(".summer", ".spring"), all.x = T)

summer_surveys[ , summary(potcri_early_vegdfoc)]

#any dups? Y
sum(duplicated(summer_surveys[ ,SURVEY_ID.summer ,]))

#get those survey IDs
summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer]

#now all rows with those?
summer_surveys[SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], .(DATESURVEYSTART.spring, potcri_early_vegdfoc) , SURVEY_ID.summer]

#grab the date of the latest clp survey
keepers <- summer_surveys[SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], .("DATESURVEYSTART.spring"=max(DATESURVEYSTART.spring)) , SURVEY_ID.summer]

keepers <- summer_surveys[keepers]

keepers[ , DATESURVEYSTART.spring := as.Date(DATESURVEYSTART.spring) ,]
keepers[ , year := as.integer(year) ]

#dump all duplicated summer surveys
summer_surveys <- summer_surveys[!SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], ]

#re-add in those keepers
summer_surveys <- rbind(summer_surveys, keepers)

rm(keepers)

#verify that summer surveys has clp spring data:

summer_surveys[ , summary(potcri_early_vegdfoc) , ]

# bring in mgmt data ------------------------------------------------------

#merge mgmt data on dows & years
mgmtdata[ , p_dow := round(downum/100, 0) ,]
summer_surveys[ , p_dow := round(DOW/100, 0) , ]

#gotta deal with subbasin exents
summer_surveys[ , sort(unique(SUBBASIN)) ,]

#minnetonka
summer_surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
mgmtdata[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
mgmtdata[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
mgmtdata[str_detect(lakename, "innet") , subbasin := word(lakename, -1, sep = "-") , ]
mgmtdata[str_detect(lakename, "innet") , lakename := "minnetonka"]
#grays bay
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "gray") , subbasin := "Grays Bay"]
#lower lake
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "lower") , subbasin := "Lower Lake"]
#carsons bay
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "carsons") , subbasin := "Carsons Bay"]
#crystal bay
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "crystal") , subbasin := "Crystal Bay"]
#st.albans
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "albans") , subbasin := "St Albans Bay"]
#northarm
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "orth") , subbasin := "North Arm Bay" ]
#halsteads
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "halsted") , subbasin := "Halsteads Bay" ]
#upper
mgmtdata[str_detect(lakename, "innet") &
           str_detect(subbasin, "upper") , subbasin := "Upper Lake" ]


mgmtdata[ ]


summer_surveys <- merge(summer_surveys, mgmtdata, by.x = c("DOW", "year"), by.y = c("downum", "year"), all.x = T)

summer_surveys[is.na(clp_targeted), clp_targeted := F ]

summer_surveys[is.na(ewm_targeted), ewm_targeted := F]

sum(duplicated(summer_surveys[ , SURVEY_ID.summer ,]))




#summary stats

summer_surveys[ , summary(clp_targeted) ,]
summer_surveys[potcri_early_vegdfoc > 0 , summary(clp_targeted) ,]
summer_surveys[is.na(potcri_early_vegdfoc) & clp_targeted == T , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART.summer )  ,]
summer_surveys[ , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum > 0 , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum == 0 & ewm_targeted == T , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART.summer )  ,]


# add secchi data to these survey metrics ---------------------------------


# rescue the secchi data from the plants db for these surveys

summer_surveys <- merge(summer_surveys,plants[ , .("Secchi.m" = first(Secchi_m.mean)), SURVEY_ID], by.x = "SURVEY_ID.summer", by.y = "SURVEY_ID")


# visualize the invader effects across scales ---------------------------------------------------


# lake level invader effects ----------------------------------------------


# visualize "effects" at lake level
summer_surveys[, myrspi_summer_vegdfoc := Myriophyllum_spicatum/n_points_vegetated.summer]
summer_surveys[, potcri_summer_vegdfoc := Potamogeton_crispus.summer/n_points_vegetated.summer]

# figure 1: combined lake level CLP & EWM abundance effects ---------------

legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
INV_ENSpie <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
             aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
             alpha = 0.4)+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
             aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
             alpha = 0.4)+
  geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
              aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
              method = "lm")+
  geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
              aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
              method = "lm")+
  xlab("Invader Lakewide Prevalence")+
  ylab("Native Species ENSpie")+
  ggtitle("INVADED LAKE SURVEYS")+
  labs(color = NULL) +
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"))



INV_richness <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
             aes(potcri_early_vegdfoc, nat_richness),
             alpha = 0.4, color = "red")+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
             aes(myrspi_summer_vegdfoc, nat_richness),
             alpha = 0.4, color = "blue")+
  geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
              aes(potcri_early_vegdfoc, nat_richness),
              method = "lm", color = "red")+
  geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
              aes(myrspi_summer_vegdfoc, nat_richness),
              method = "lm", color = "blue")+
  xlab(NULL)+
  ylab("Richness")+
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y.left = element_text(angle = 90))
INV_evenness <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
             aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
             alpha = 0.4, color = "red")+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
             aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
             alpha = 0.4, color = "blue")+
  # geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
  #             aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
  #             method = "lm", color = "red")+
  # geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
  #             aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
  #             method = "lm", color = "blue")+
  xlab("Invader Lakewide Prevalence")+
  ylab("Evenness")+
  theme_bw()+
  theme(axis.text.y.left = element_text(angle = 90))

ggarrange(INV_ENSpie, ggarrange(INV_richness, INV_evenness, ncol = 1))


#presence absence lake scale boxplots

clpboxplot <- summer_surveys[!is.na(potcri_early_vegdfoc) & clp_targeted == F,
                             "inv.pres" := potcri_early_vegdfoc > 0 ]
clpboxplot[ , species := "CLP" ,]
clpboxplot <- clpboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]

ewmboxplot <- summer_surveys[!is.na(myrspi_summer_vegdfoc) & ewm_targeted == F,
                             "inv.pres" := myrspi_summer_vegdfoc > 0 ]
ewmboxplot[ , species := "EWM" ,]
ewmboxplot <- ewmboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]

boxplot_data <- rbind(ewmboxplot,clpboxplot)
rm(clpboxplot, ewmboxplot)

boxplot_data[ , evenness := simpsons_div_nat/nat_richness]

boxplot_data <- melt(boxplot_data, id.vars = c("inv.pres","species"), variable.name = "metric", value.name = "value")

boxplot_data[ , metric:= factor(metric, levels = c("simpsons_div_nat", "nat_richness", "evenness")),]

box1 <- ggplot(data = boxplot_data[metric == "simpsons_div_nat"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("CLP", "EWM"),labels=c("Potamogeton crispus\nAbsent    Present\nn = 1956    n = 676","Myriophyllum spicatum\nAbsent    Present\nn = 2241   n = 391"))+
  scale_fill_discrete(limits= c("FALSE", "TRUE"),labels=c("Invader Absent","Invader Present"))+
  xlab(NULL)+
  ylab("Native Species ENSpie")+
  theme_bw()+
  theme(axis.text.x = element_text( face = "italic"), legend.position = c(.35,.9), legend.title = element_blank(), legend.background = element_blank())+
  ggtitle("ALL SURVEYS")

boxplot_data[metric == "nat_richness", .N, .(species,inv.pres)]

box2 <- ggplot(data = boxplot_data[metric == "nat_richness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("CLP", "EWM"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Richness")+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.position = "none")+
  ggtitle("")+
  theme(axis.text.y.left = element_text(angle = 90))

box3 <- ggplot(data = boxplot_data[metric == "evenness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("CLP", "EWM"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Evenness")+
  theme_bw()+
  theme(axis.text.x = element_text( face = "italic"), legend.position = "none")+
  theme(axis.text.y.left = element_text(angle = 90))

boxps <- ggarrange(box1, ggarrange(box2, box3, ncol = 1,
                                   labels = c("b","c"),
                                   label.x = c(0.15,0.15),
                                   label.y = c(0.85,0.93)),
                   labels = c("a", ""),
                   label.x = 0.15,
                   label.y = 0.92)


abundps <- ggarrange(INV_ENSpie, ggarrange(INV_richness, INV_evenness,
                                           ncol = 1,
                                           labels = c("e","f"),
                                           label.x = c(0.15,0.15),
                                           label.y = c(0.86,0.95)),
                     labels = c("d",""),
                     label.x = 0.15,
                     label.y = 0.91)+
  border(color = "black", size = 0.8, linetype = NULL)


ggarrange(boxps,abundps, ncol = 1)



# tests for fig 1: lake level invader effects ----------------------------------------

# pres/abs inv ------------------------------------------------------------


#### ENSpie
# EWM Pres/abs
t.test(summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc == 0, simpsons_div_nat ],
       summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc > 0, simpsons_div_nat ])

#CLP Pres/abs
t.test(summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc == 0, simpsons_div_nat ],
       summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc > 0, simpsons_div_nat ])

#### Richness
# EWM Pres/abs
t.test(summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc == 0, nat_richness ],
       summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc > 0, nat_richness ])

#CLP Pres/abs
t.test(summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc == 0, nat_richness ],
       summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc > 0, nat_richness ])

####Evenness
# EWM Pres/abs
t.test(summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc == 0, simpsons_div_nat/nat_richness ],
       summer_surveys[!is.na(myrspi_summer_vegdfoc) & myrspi_summer_vegdfoc > 0, simpsons_div_nat/nat_richness ])

#CLP Pres/abs
t.test(summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc == 0, simpsons_div_nat/nat_richness ],
       summer_surveys[!is.na(potcri_early_vegdfoc) & potcri_early_vegdfoc > 0, simpsons_div_nat/nat_richness ])


# abund invader -----------------------------------------------------------



#### ABUNDANCE ENSPie
#EWM Abund
EWMabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F] )
summary(EWMabund_nat_lake_ENSpie)
plot(EWMabund_nat_lake_ENSpie)


#CLP Abund
CLPabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F])
summary(CLPabund_nat_lake_ENSpie)
# plot(CLPabund_nat_lake_ENSpie)

#### ABUNDANCE Richness
#EWM Abund
EWMabund_nat_lake_richness <- glmer(nat_richness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F], family = poisson())
summary(EWMabund_nat_lake_richness)
# plot(EWMabund_nat_lake_richness)

#CLP Abund
CLPabund_nat_lake_richness <- glmer(nat_richness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F] , family = poisson())
summary(CLPabund_nat_lake_richness)
# plot(CLPabund_nat_lake_richness)

#### ABUNDANCE Evenness
#EWM Abund
EWMabund_nat_lake_evenness<- glmer(simpsons_div_nat/nat_richness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F], family = binomial())
summary(EWMabund_nat_lake_evenness)
# plot(EWMabund_nat_lake_evenness)

#CLP Abund
CLPabund_nat_lake_evenness <- glmer(simpsons_div_nat/nat_richness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F] , family = binomial())
summary(CLPabund_nat_lake_evenness)
# plot(CLPabund_nat_lake_evenness)


# visualize tests ---------------------------------------------------------

# ENSPie ~ Abund: EWM
nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F])
#Prev Secchi
## [1] 385
newdat1<-data.frame(myrspi_summer_vegdfoc = seq(min(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, myrspi_summer_vegdfoc]), max(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, myrspi_summer_vegdfoc]), length.out = nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F])),
                    year = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, year],
                    DOW = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, DOW])
#fitted values

newdat1$fitted <- predict(EWMabund_nat_lake_ENSpie, newdat1, re.form = NA)

preds <- predictInterval(EWMabund_nat_lake_ENSpie, newdat1, which = "fixed", n.sims = 9999)

newdat1 <- cbind(newdat1,preds)
# newdat1$fitted<-predict(EWMabund_nat_lake_ENSpie, newdat=newdat1, se.fit = TRUE)$fit
# 
# newdat1$se<-predict(EWMabund_nat_lake_ENSpie, newdat=newdat1, se.fit = TRUE)$se.fit

# ENSPie ~ Abund: CLP
nrow(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F])
## [1] 503
newdat2<-data.frame(potcri_early_vegdfoc = seq(min(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, potcri_early_vegdfoc]), max(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, potcri_early_vegdfoc]), length.out = nrow(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F])),
                    year = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, year],
                    DOW = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, DOW])
#fitted values

newdat2$fitted <- predict(CLPabund_nat_lake_ENSpie, newdat2, re.form = NA)

preds2 <- predictInterval(CLPabund_nat_lake_ENSpie, newdat2, which = "fixed", n.sims = 9999)

newdat2 <- cbind(newdat2,preds2)

# newdat2$fitted<-predict(CLPabund_nat_lake_ENSpie, newdat=newdat2, se.fit = TRUE)$fit
# 
# newdat2$se<-predict(CLPabund_nat_lake_ENSpie, newdat=newdat2, se.fit = TRUE)$se.fit



legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
INV_ENSpie <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
             aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
             alpha = 0.4)+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
             aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
             alpha = 0.4)+
  geom_line(data = newdat1, 
            aes(myrspi_summer_vegdfoc,fit), color = "red")+
  geom_ribbon(data = newdat1, aes(x = myrspi_summer_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "black", alpha = .15)+
  geom_line(data = newdat2, 
            aes(potcri_early_vegdfoc,fit), color = "blue")+
  geom_ribbon(data = newdat2, aes(x = potcri_early_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "black", alpha = .15)+
  xlab("Invader Lakewide Prevalence")+
  ylab("Native Species ENSpie")+
  ggtitle("INVADED LAKE SURVEYS")+
  labs(color = NULL) +
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus: n = 503","Myriophyllum spicatum: n = 385"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))


#Richness~Abund: EWM

nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F])
## [1] 385
newdat1<-data.frame(myrspi_summer_vegdfoc = seq(min(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, myrspi_summer_vegdfoc]), max(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, myrspi_summer_vegdfoc]), length.out = nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F])),
                    year = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, year],
                    DOW = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F, DOW])
#fitted values

newdat1$fitted <- exp(predict(EWMabund_nat_lake_richness, newdat1, re.form = NA))

preds <- exp(predictInterval(EWMabund_nat_lake_richness, newdat1, which = "fixed", n.sims = 9999))

newdat1 <- cbind(newdat1,preds)

# newdat1$fitted<-exp(predict(EWMabund_nat_lake_richness, newdat=newdat1, se.fit = TRUE)$fit)
# 
# newdat1$se<-exp(predict(EWMabund_nat_lake_richness, newdat=newdat1, se.fit = TRUE)$se.fit)

#Richness~Abund: CLP
nrow(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F])
## [1] 503
newdat2<-data.frame(potcri_early_vegdfoc = seq(min(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, potcri_early_vegdfoc]), max(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, potcri_early_vegdfoc]), length.out = nrow(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F])),
                    year = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, year],
                    DOW = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F, DOW]
)
#fitted values
newdat2$fitted <- exp(predict(CLPabund_nat_lake_richness, newdat2, re.form = NA))

preds <- exp(predictInterval(CLPabund_nat_lake_richness, newdat2, which = "fixed", n.sims = 9999))

newdat2 <- cbind(newdat2,preds)

# 
# newdat2$fitted<-exp(predict(CLPabund_nat_lake_richness, newdat=newdat2, se.fit = TRUE)$fit)
# 
# newdat2$se<-exp(predict(CLPabund_nat_lake_richness, newdat=newdat2, se.fit = TRUE)$se.fit)


#plotting:

INV_richness <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F],
             aes(potcri_early_vegdfoc, nat_richness),
             alpha = 0.4, color = "red")+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ],
             aes(myrspi_summer_vegdfoc, nat_richness),
             alpha = 0.4, color = "blue")+
  geom_line(data = newdat1, 
            aes(myrspi_summer_vegdfoc,fitted), color = "red")+
  geom_ribbon(data = newdat1, aes(x = myrspi_summer_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "black", alpha = .15)+
  geom_line(data = newdat2, 
            aes(potcri_early_vegdfoc,fitted), color = "blue")+
  geom_ribbon(data = newdat2, aes(x = potcri_early_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "black", alpha = .15)+
  xlab(NULL)+
  ylab("Richness")+
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y.left = element_text(angle = 90))


# remake Figure 1 with predictions from models -----------------------------

boxps <- ggarrange(box1, ggarrange(box2, box3, ncol = 1,
                                   labels = c("b","c"),
                                   label.x = c(0.15,0.15),
                                   label.y = c(0.85,0.93)),
                   labels = c("a", ""),
                   label.x = 0.15,
                   label.y = 0.92)


abundps <- ggarrange(INV_ENSpie, ggarrange(INV_richness, INV_evenness,
                                           ncol = 1,
                                           labels = c("e","f"),
                                           label.x = c(0.15,0.15),
                                           label.y = c(0.86,0.95)),
                     labels = c("d",""),
                     label.x = 0.15,
                     label.y = 0.91)+
  border(color = "black", size = 0.8, linetype = NULL)


ggarrange(boxps,abundps, ncol = 1)

# Clean up Workspace:
# rm(abundps, box1, box2, box3, boxplot_data, boxps, CLP_ENSpie, CLP_evenness, CLP_nat_lake_mod, CLP_richness, CLPabund_nat_lake_ENSpie, CLPabund_nat_lake_evenness, CLPabund_nat_lake_richness)



# point level rake abund --------------------------------------------------

# plants[!is.na(TAXON) , length(unique(TAXON)) , POINT_ID ]

plants_rakeabund_wide <- dcast(plants[!is.na(REL_ABUND_CORRECTED)], SURVEY_ID + POINT_ID + Secchi_m.mean + DEPTH_FT ~ TAXON, value.var = c("REL_ABUND_CORRECTED"), fun.aggregate = last, fill = 0)

#diversity metrics go here
rake_natcols <- names(plants_rakeabund_wide)[names(plants_rakeabund_wide)%in%natcols]
plants_rakeabund_wide[ , shannon_div := diversity(plants_rakeabund_wide[ , c(3:138)],index = "shannon") ]
plants_rakeabund_wide[ , simpsons_div := diversity(plants_rakeabund_wide[ , c(3:138)],index = "invsimpson") ]
plants_rakeabund_wide[ , shannon_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "shannon") ]
plants_rakeabund_wide[ , simpsons_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "invsimpson") ]
plants_rakeabund_wide[ ,  richness := rowSums(.SD), .SDcols = c(3:138) ]
plants_rakeabund_wide[ ,  nat_richness := rowSums(.SD), .SDcols = rake_natcols ]

summary(plants_rakeabund_wide$`Potamogeton crispus`)

# rename data for easy plotting
plants_rakeabund_wide <- clean_names(plants_rakeabund_wide)




#ENSpie
point_abund_ENSpie <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat),
              method = "lm", color = "blue")+
  ylab("ENSpie")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat),
              method = "lm", color = "red")+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  ggtitle("INVADED SAMPLES")+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))

#model goes here:
summary(lmer(simpsons_div_nat~potamogeton_crispus+ (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0 & !simpsons_div_nat == Inf]))

summary(lmer(simpsons_div_nat~myriophyllum_spicatum + (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0 & !simpsons_div_nat == Inf]))


#Richness
point_abund_richness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_richness),
              method = "lm", color = "blue")+
  ylab("Richness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_richness),
              method = "lm", color = "red")+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_y_log10()+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))

#models goes here:
summary(glmer(nat_richness~potamogeton_crispus+ (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0], family = poisson()))

summary(glmer(nat_richness~myriophyllum_spicatum+ (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0], family = poisson()))


#evenness?
point_abund_evenness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat/nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat/nat_richness),
              method = "lm", color = "blue")+
  ylab("Evenness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat/nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .08)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat/nat_richness),
              method = "lm", color = "red")+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_y_log10()+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))

#models goes here:
summary(glmer(simpsons_div_nat/nat_richness~potamogeton_crispus + (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0 & !simpsons_div_nat == Inf], family = binomial()))

summary(glmer(simpsons_div_nat/nat_richness~myriophyllum_spicatum + (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0 & !simpsons_div_nat == Inf], family = binomial()))

point_abunds <- ggarrange(point_abund_ENSpie, ggarrange(point_abund_richness, point_abund_evenness,
                                                        ncol = 1,
                                                        labels = c("e","f"),
                                                        label.x = c(0.15,0.15),
                                                        label.y = c(0.86,0.95)),
                          labels = c("d",""),
                          label.x = 0.15,
                          label.y = 0.91)+  border(color = "black", size = 0.8, linetype = NULL)


# point level p/a effects with rake data ----------------------------------

#make a boxplotting dataset
point_boxes_dat <- melt( 
  plants_rakeabund_wide[,.(point_id, nat_div = simpsons_div_nat, nat_richness, nat_evenness = simpsons_div_nat/nat_richness, inv_pot_cri = potamogeton_crispus > 0, inv_myr_spi = myriophyllum_spicatum > 0), ],
  id.vars = c("point_id", "inv_pot_cri", "inv_myr_spi" ),
  measure.vars = patterns("^nat"),
  variable.name = c("metric"),
  value.name = c("value")
  
)

point_boxes_dat <- melt(point_boxes_dat,
                        id.vars = c("point_id", "metric", "value" ),
                        measure.vars = patterns("^inv"),
                        variable.name = c("species"),
                        value.name = c("inv.pres")
)

point_boxes_dat[metric == "nat_div" , .N , .(species, inv.pres)  ]


box1 <- ggplot(data = point_boxes_dat[metric == "nat_div"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus\nAbsent    Present\nn = 60080    n = 21067","Myriophyllum spicatum\nAbsent    Present\nn = 67088   n = 14059"))+
  scale_fill_discrete(limits= c("FALSE", "TRUE"),labels=c("Invader Absent","Invader Present"))+
  xlab(NULL)+
  ylab("Native Species ENSpie")+
  theme_bw()+
  ggtitle("ALL SAMPLES")+
  theme(axis.text.x = element_text( face = "italic"), legend.position = c(.35,.9), legend.title = element_blank(), legend.background = element_blank())

box2 <- ggplot(data = point_boxes_dat[metric == "nat_richness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Richness")+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.position = "none")+
  ggtitle("")+
  theme(axis.text.y.left = element_text(angle = 90))

box3 <- ggplot(data = point_boxes_dat[metric == "nat_evenness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Evenness")+
  theme_bw()+
  theme(axis.text.x = element_text( face = "italic"), legend.position = "none")+
  theme(axis.text.y.left = element_text(angle = 90))

point_boxps <- ggarrange(box1, ggarrange(box2, box3, ncol = 1,
                                         labels = c("b","c"),
                                         label.x = c(0.15,0.15),
                                         label.y = c(0.85,0.93)),
                         labels = c("a", ""),
                         label.x = 0.15,
                         label.y = 0.92)



#individual comparions with tests:
#ENSpie
ggplot(plants_rakeabund_wide[], aes(potamogeton_crispus> 0, simpsons_div_nat) )+
  geom_boxplot()+
  geom_smooth(method = "lm")+
  ylab("ENSpie")+
  ylim(c(0,5.75
  ))
summary(lm(simpsons_div~potamogeton_crispus>0, data = plants_rakeabund_wide))

ggplot(plants_rakeabund_wide, aes(myriophyllum_spicatum>0, simpsons_div_nat) )+
  geom_violin()+
  geom_smooth(method = "lm")+
  ylab("ENSpie")
summary(lm(shannon_div_nat~myriophyllum_spicatum>0, data = plants_rakeabund_wide))


#richness?
ggplot(plants_rakeabund_wide, aes(potamogeton_crispus>0, nat_richness) )+
  geom_boxplot()+
  geom_smooth(method = "lm")
summary(glm(nat_richness~potamogeton_crispus>0, data = plants_rakeabund_wide, family = poisson()))

ggplot(plants_rakeabund_wide, aes(myriophyllum_spicatum>0, nat_richness) )+
  geom_violin()+
  geom_smooth(method = "lm")
summary(glm(nat_richness~myriophyllum_spicatum>0, data = plants_rakeabund_wide, family = poisson()))


#evenness?
ggplot(plants_rakeabund_wide, aes(potamogeton_crispus>0, simpsons_div_nat/nat_richness) )+
  geom_boxplot()+
  geom_smooth(method = "lm")
summary(glm(shannon_div_nat/nat_richness~potamogeton_crispus, data = plants_rakeabund_wide, family = binomial()))

ggplot(plants_rakeabund_wide, aes(myriophyllum_spicatum>0, simpsons_div_nat/nat_richness) )+
  geom_violin()+
  geom_smooth(method = "lm")
summary(glm(shannon_div_nat/nat_richness~myriophyllum_spicatum, data = plants_rakeabund_wide, family = binomial(link = 'logit')))


# Figure 2: Compile boxplots and abund plots

ggarrange(point_boxps,point_abunds, ncol = 1)





# point level p/a effects with all data -------------------------------------------------

plants_occurrence_wide <- dcast(plants, SURVEY_ID + POINT_ID + NO_VEG_FOUND + Secchi_m.mean + DEPTH_FT ~ TAXON, value.var = "INDATABASE", fun.aggregate = last, fill = 0)

#diversity metrics go here
names(plants_occurrence_wide)
point_natcols <- names(plants_occurrence_wide)[!names(plants_occurrence_wide)%in%c(rte[native_status == "I", mn_dnr_scientific_name], "Nitellopsis", "Typha glauca", "SURVEY_ID", "POINT_ID", "richness", "NA", "DEPTH_FT", "Secchi_m.mean", "NO_VEG_FOUND")]
plants_occurrence_wide[ ,  richness := rowSums(.SD), .SDcols = c(7:241) ]
plants_occurrence_wide[ ,  nat_richness := rowSums(.SD), .SDcols = point_natcols ]
# these data are p/a for species at points, meaning we have no evenness values here. 


ggplot(plants_occurrence_wide[NO_VEG_FOUND == F], aes(`Potamogeton crispus`, nat_richness) )+
  geom_boxplot(aes(group = `Potamogeton crispus`))
summary(glm(nat_richness~`Potamogeton crispus`, data = plants_occurrence_wide[NO_VEG_FOUND == F], family = poisson()))


ggplot(plants_occurrence_wide[richness != 0], aes(`Myriophyllum spicatum`, nat_richness) )+
  geom_boxplot(aes(group = `Myriophyllum spicatum`))
summary(glm(nat_richness~`Myriophyllum spicatum`, data = plants_occurrence_wide[NO_VEG_FOUND == F], family = poisson()))

# lake level light/ Secchi ------------------------------------------------
summer_surveys[ , .N , nat_richness][order(nat_richness)]

#whats the relationship to clarity look like in uninvaded lakes
nat_ENSpie <- ggplot(summer_surveys[((potcri_early_vegdfoc == 0 & myrspi_summer_vegdfoc == 0) |
                                       (is.na(potcri_early_vegdfoc) & myrspi_summer_vegdfoc == 0) )&
                                      !is.na(Secchi.m) , ],
                     aes(Secchi.m,simpsons_div_nat))+
  geom_point( alpha = 0.4)+
  geom_point(data = summer_surveys[(potcri_early_vegdfoc > 0 | myrspi_summer_vegdfoc >0) &
                                     !is.na(Secchi.m) , ],
             aes(Secchi.m,simpsons_div_nat),
             alpha = 0.4, color = "red")+
  #adds a line for invaded subset
  geom_smooth(data = summer_surveys[!is.na(Secchi.m) , ],
              aes(Secchi.m,simpsons_div_nat),
              method = "lm")+
  # geom_smooth(method = "lm")+
  xlab("Secchi")+
  ylab("ENSpie")


clp_secchi <- ggplot(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F], aes(Secchi.m, potcri_early_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  ylab("Summer CLP foc within vegetated points")

ewm_secchi <- ggplot(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F ], aes(Secchi.m, myrspi_summer_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth( method = "lm")+
  ylab("EWM foc within vegetated points")

ggarrange(  INV_ENSpie,  ggarrange(nat_ENSpie, clp_secchi, ewm_secchi, ncol = 1))

#groom the secchi data for pretty plotting:

summer_surveys[,groomed_secchi := round(Secchi.m/.5)*.5  , ]
summer_surveys[ , groomed_secchi := as.factor(groomed_secchi) , ]
summer_surveys[Secchi.m >= 4 ,groomed_secchi := "4+"  , ]
summer_surveys[Secchi.m < 0.5 ,groomed_secchi := "<0.5"  , ]
summer_surveys[ , groomed_secchi := as.factor(groomed_secchi) , ]
summer_surveys[ , groomed_secchi := factor(groomed_secchi, levels = c(
  "<0.5", "0.5", "1", "1.5", "2.0", "2.5", "3", "3.5", "4+"
)) , ]



clp_secchi_ENSpie <- ggplot(summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F & !is.na(groomed_secchi)], aes(potcri_early_vegdfoc, simpsons_div_nat))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab(NULL)+
  ylab("ENSpie")+
  ggtitle("Secchi depth (meters)")+
  facet_wrap(~ groomed_secchi, nrow = 1 )+
  theme(axis.text.x = element_blank())


ewm_secchi_ENSpie <- ggplot(summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F & !is.na(groomed_secchi) ], aes(myrspi_summer_vegdfoc, simpsons_div_nat))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Invader Lakewide Prevalence")+
  ylab("ENSpie")+
  ggtitle("Secchi depth (meters)")+
  facet_wrap(~ groomed_secchi, nrow = 1 )+
  theme(axis.text.x = element_text(angle = 90))

ggarrange(clp_secchi_ENSpie, ewm_secchi_ENSpie, ncol = 1, labels = c("Potamogeton crispus", "Myriophyllum spicatum"), label.x = c(-.05,-.05), label.y = c(.7,.7))

#now peel out a partial residual plot of invader effects, accountign for water clarity
# first curlyleaf pw
m.nat.clp <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F & !is.na(Secchi.m)])

summary(m.nat.clp)

m.nat.clp.wc <- lmer(simpsons_div_nat ~ Secchi.m*potcri_early_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & clp_targeted == F] )

summary(m.nat.clp.wc) # here clp becomes non significant

# plot_model(m.nat.clp.wc, type = "pred", terms = "potcri_early_vegdfoc")
# plot_model(m.nat.clp, type = "pred", terms = "potcri_early_vegdfoc")

# AIC(m.nat.clp,m.nat.clp.wc )

#then do the same for EWM
m.nat.ewm <- lmer(simpsons_div_nat ~ myrspi_summer_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F & !is.na(Secchi.m)])

summary(m.nat.ewm)


m.nat.ewm.wc <- lmer(simpsons_div_nat ~ Secchi.m*myrspi_summer_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & ewm_targeted == F] )

summary(m.nat.ewm.wc) # here EWM and the intewraction are non-significant


# plot_model(m.nat.ewm.wc, type = "pred", terms = "myrspi_summer_vegdfoc")
# plot_model(m.nat.ewm, type = "pred", terms = "myrspi_summer_vegdfoc")

AIC(m.nat.clp,m.nat.clp.wc )


# point level light avail -------------------------------------------------

plants_rakeabund_wide[ , proplight := exp(-(log(10)/secchi_m_mean)*(depth_ft/3.2804)) ]

plants_occurrence_wide[ , proplight := exp(-(log(10)/Secchi_m.mean)*(DEPTH_FT/3.2804)) ]



point_nat_light <- ggplot(plants_rakeabund_wide[!is.na(proplight)], aes(proplight, simpsons_div_nat))+
  geom_point( alpha = 0.02, size = 2.5)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("ENSpie")


ggplot(plants_rakeabund_wide[myriophyllum_spicatum>0], aes(proplight, myriophyllum_spicatum))+
  geom_point( alpha = 0.005)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("EWM")

ggplot(plants_rakeabund_wide[potamogeton_crispus>0], aes(proplight, potamogeton_crispus))+
  geom_point( alpha = 0.005)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("CLP")

#we need to ask if the negative point scale rel goes away if we account for light & if that effect varies by species:

# point_inv_data <- melt(plants_occurrence_wide[!is.na(proplight) & richness != 0, .SD, .SDcols = c("Myriophyllum spicatum", "Potamogeton crispus", "nat_richness", "richness", "proplight", "SURVEY_ID", "POINT_ID")], id.vars = c("nat_richness", "richness", "proplight", "SURVEY_ID", "POINT_ID" ), variable.name = "invader", value.name = "invaderpresent")



# test: point level invader eff + water clarity ---------------------------

summary(lmer(simpsons_div_nat~proplight+ (1 | survey_id ), data = plants_rakeabund_wide[!simpsons_div_nat == Inf]))






CLP_eff_point <- glm(shannon_div_nat~potamogeton_crispus, data = plants_rakeabund_wide[potamogeton_crispus > 0])

summary(CLP_eff_point)

CLP_eff_point_light <- glm(shannon_div_nat~potamogeton_crispus*proplight, data = plants_rakeabund_wide[potamogeton_crispus > 0])

summary(CLP_eff_point_light)


EWM_eff_point <- glm(shannon_div_nat~myriophyllum_spicatum, data = plants_rakeabund_wide[myriophyllum_spicatum > 0])

summary(EWM_eff_point)

EWM_eff_point_light <- glm(shannon_div_nat~myriophyllum_spicatum*proplight, data = plants_rakeabund_wide[myriophyllum_spicatum > 0])

summary(EWM_eff_point_light)





# gradients within lake??? ------------------------------------------------



# species pools -----------------------------------------------------------

#' We want to use the pool above each scale as a predictor of the pool at that scale:
#' For obs data: We want lakediv ~ watersheddiv; then pointdiv~lakediv
#' For our expmt: We have boosted species pools by adding seeds & propagules.
#' As such, we want pointdiv ~ lakediv + propagule adds
#'  We also have quad div ~ plot meander richness
#' 
#first add the lake level richness to the point rake abund data
match(plants_rakeabund_wide[ ,survey_id ,], surveys[, SURVEY_ID, ])
plants_rakeabund_wide[ , surveyrichness := surveys[match(plants_rakeabund_wide[ ,survey_id ,], surveys[, SURVEY_ID, ]), nat_richness] ,]

ggplot( data = plants_rakeabund_wide,
        aes(surveyrichness, nat_richness))+
  geom_point()+
  geom_smooth(method = "lm")

point_rich_lake_pool <- glm(nat_richness~surveyrichness, data = plants_rakeabund_wide, family = poisson())
summary(point_rich_lake_pool)

# create a watershed level species matrix:


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
        aes(watershedrichness, nat_richness))+
  geom_point()+
  geom_smooth(method = "lm")

point_rich_watershed_pool <- glm(nat_richness~watershedrichness, data = plants_rakeabund_wide, family = poisson())
summary(point_rich_lake_pool)


surveys[ , watershed := plants[match(surveys[ , SURVEY_ID ,], plants[, SURVEY_ID, ]), watershed] ,]
surveys[ , watershedrichness := watershed_occurrence_wide[match(surveys[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), n_species] ,]

ggplot( data = surveys,
        aes(watershedrichness, nat_richness))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Survey Richness")+
  xlab("HUC-8 Watershed Richness")+
  theme_bw()

survey_rich_watershed_pool <- glm(nat_richness~watershedrichness, data = surveys, family = poisson())
summary(survey_rich_watershed_pool)

#species at points as fn of both scales:


point_rich_watershed_survey_pools <- glm(nat_richness~ watershedrichness + surveyrichness , data = plants_rakeabund_wide, family = poisson())
summary(point_rich_watershed_survey_pools)


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
