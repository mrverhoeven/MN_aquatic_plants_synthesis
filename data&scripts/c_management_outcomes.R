#'---
#' title: "Observational Data -- Management Outcomes"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will examine the effects of control targeting invaders on both
#' the invaders and the native plant community. 
#' 
#' 


#' ## Document Preamble
  #+ warning = FALSE
  # load libraries ------------------------------------------------------------------
# # Load Libraries
library(data.table)
update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
library(stringr)
# library(sf)
# library(vegan)
# library(gridExtra)
# library(dplyr)
library(tidyr)
library(lme4)
library(sjPlot)
library(mediation)
library(ggpubr)
library(EnvStats)
library(lmerTest)
library(merTools)
library(rstanarm)
library(janitor)
library(MatchIt)
library(optmatch)
library("lmtest") #coeftest
library("sandwich") #vcovCL


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
# 
plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
surveys <- fread(file = "data&scripts/data/output/surveys_aqplants.csv")

load("data&scripts/data/output/synthesis_script_datasets.Rdata")

#management data
# source(file = "data&scripts/c1_management_outcomes.R") # will load and clean management data and leave in WS a mgmtdata file

surveys_mgmt <- fread( file = "data&scripts/data/output/survs_mgmt_dateprelabelled.csv")

#other covariates?
#shyams EWM proj Data?:
# here we have depth, GDD, others
b <- fread("data&scripts/data/input/EWMlakeindex.allwatchem.NLDASwtmp2.csv")
# here we have road density w/in 500m buffer
c <- fread(file = "data&scripts/data/input/Lakes_RoadDensity.csv")

#' # Issues to address:
#' 
#' ## Invader p/a and abundance metrics: 
#' 
#' Because of the distinct phenology of CLP, we've got to use estimates of 
#' abundance from early season surveys (during the growth phase of CLP annual
#' life cycle). 
#' 
#' We can do this ONLY for the survey level work... That's b/c at the point 
#' level we don't have the ability to connect two sample locations across time.
#' 
#' So at the lake scale, we will use spring (Mar-June) surveys to come up with a
#' value for CLP abundance and we can link this by lake over to the summer
#' native community surveys. In line with this, we'll have a bunch of NA values 
#' for spring CLP abundance-- these are surveys that have no springtime pair that 
#' could reasonably be considered an assesment of CLP.
#' 
#' At the point scale we need to use the point-level abundance data. Do we just
#' use all data (and model date as a driving factor)? Just summer? 
#' 
#' ## Space for time?
#' 
#' We've got a pile of data, but its a messy pile. We've got lots of repeat
#' sampling, but it's not consistent intervals and there are lots of holes. The
#' way we treated this previously was to run models with lake ID as a random 
#' effect. We can try that again here, or we might consider other options.
#' 
#' ## Pre or post treatment for each survey
#' 
#' Because we have (at best) a fuzzy idea of the dates of treatments, I have
#' manually reviewed each survey with a treatment in that year (~350) and based
#' on all the reported mgmt dates I designated each survey as pre- or post-
#' treatment. This seems like a very shaky conclusion/method of deducing this. 
#' 
#' ## "Managed"
#' 
#' We have a somewhat poor idea of which lakes received management to control
#' EWM or CLP in MN. We've got a dataset that does our best to assess this 
#' question for 2012-2018. My gut feeling (and nothing more than that) tells me we've probably
#' got 80% of the CLP & EWM management efforts in that timframe captured in our dataset.
#' 
#' Then there's the question of what it means to be "managed." You might have
#' managed the entire pop extent in your lake--or maybe you just managed 5% of
#' the pop extent because you didn't have the money. Maybe you chose to use an
#' herbicide control approach that used repeated herbicide in the same areas 
#' (EWM). Or maybe you've used some combination of herb and mechanical control. 
#' Did you control every other year? Every year? Why?
#' 
#' We've got a tiered set of metrics that move from least precise about control
#' (this lake-year was managed for AN invader), to most precise (this lake-year
#' was managed for this invader on this date on X acres--at this point I have 
#' not included method, but could if we dream up a clean way to do that.)
#' 
#' ## Effects of Management
#' 
#' As we showed with the CLP paper, the temporal frame at which we ask if there
#' was an "effect" of management is not straightforward. Previously we asked 
#' about a w/in year effect, a following year effect, and an effect of
#' cumulative yrs of trt.
#' 
#' To-date we have not coded the data this way. Instead, we create a set of
#' management indices. We've got a set of metrics that start with: 
#'  sum(1 / (current year - management year + 1)) eg:
#'  for 2019 survey with 2017 and 2015 management, mgmt index = 
#'  sum( 1/3 + 1/5)
#'  
#'  then I carry this same concept through to acreage, except there we assign
#'  the numerator as acres in management year. So 50 acres this year and 50 
#'  acres last year = 50/1+50/2

#' # Data Prep:

# trim data by year, polish off trt Y/N assignments -----------------------

#clean up the management data: (assign zeros or NAs as appropriate)

names(surveys_mgmt)
# only had trt data from 2012 onward. So exclude pre 2012:
surveys_mgmt[clp_ntrtdates>0 ,hist(year, breaks = 9) , ]
surveys_mgmt[ewm_ntrtdates>0 ,hist(year, breaks = 9) , ]
surveys_mgmt_p2012 <- surveys_mgmt[year>=2012]

#pre- or post- treatment survey timing
surveys_mgmt[!is.na(ewm_ntrtdates) & (ewm_ntrtdates>0) & is.na(ewm_pretrt)    , ewm_pretrt := 0 ,]
surveys_mgmt[!is.na(clp_ntrtdates) & (clp_ntrtdates>0) & is.na(clp_pretrt)    , clp_pretrt := 0 ,]
surveys_mgmt[(clp_ntrtdates == 0)    , clp_pretrt := NA ,]

#lakename from mgmt dat can be dropped:
surveys_mgmt[ , c("lakename","i.lakename","i.lakename.2","i.lakename.1") := NULL]

# mash mgmtdates:
surveys_mgmt[ , mgmtdates:= paste(date1,date2,date3,date4,date5,date6,date7,date8,date9) , ]
surveys_mgmt[, c("date1","date2","date3","date4","date5","date6","date7","date8","date9") := NULL ]

#dump parent dows
surveys_mgmt[ , c("p_dow","i.p_dow","i.p_dow.2","i.p_dow.1") := NULL , ]

#species targeted dates & acres:
surveys_mgmt[ , .N , clp_ntrtdates]
surveys_mgmt[ , .N , ewm_ntrtdates]
surveys_mgmt[ , summary(clp_m) , ]
surveys_mgmt[ , summary(ewm_m) , ]
surveys_mgmt[ , summary(clp_controlacres) , ]
surveys_mgmt[ , summary(ewm_controlacres) , ]
surveys_mgmt[ , summary(clp_acres_m) , ]
surveys_mgmt[ , summary(ewm_acres_m) , ]
surveys_mgmt[ , .N , ntrtdates]
surveys_mgmt[ , summary(m) , ]

surveys <- surveys_mgmt
rm(surveys_mgmt)

#' for many of these, we will be leaving the NA values in. There may be a reason
#' to do this to reflect a lack of info on a givene lake/yr (0 treatment implies
#' something different from, "we didn't find any record of it")
#' 
#' #' Recall seasonality of surveys is important re: phenology of clp

# add a CLP metric to summer surveys where CLP was measured during a reasonable time of year --------

surveys[ , DATESURVEYSTART := as.IDate(DATESURVEYSTART, format = "%m/%d/%Y" ) , ]

#put a CLP index into the summer surveys:
early_clpsurveys <- surveys[month(DATESURVEYSTART) %in% c(3,4,5,6) , .(SURVEY_ID, DOW, year = year(DATESURVEYSTART), LAKE_NAME, SUBBASIN, Potamogeton_crispus,DATESURVEYSTART, n_points_vegetated, tot_n_samp)  , ]

early_clpsurveys[ , potcri_early_vegdfoc := Potamogeton_crispus/n_points_vegetated ]
early_clpsurveys[is.na(potcri_early_vegdfoc), potcri_early_vegdfoc := 0] #here we can assume that the sampling was reasonable timing to find CLP had it been present
early_clpsurveys[ ,.N , potcri_early_vegdfoc>0]

surveys[ , year := year(DATESURVEYSTART) , ]

#thin summer surveys to exclude pre july
summer_surveys <- surveys[yday(DATESURVEYSTART) > 181  , , ]

#append early clp metrics to these summer surveys,:

summer_surveys[ , joindates := DATESURVEYSTART]
early_clpsurveys[ , joindateE := DATESURVEYSTART]


summer_surveys <- early_clpsurveys[summer_surveys, on = .(DOW, year, LAKE_NAME, SUBBASIN, joindateE < joindates), mult = "first" ]

names(summer_surveys)[names(summer_surveys) == "DATESURVEYSTART"] <- "SPRING_DATESURVEYSTART"
names(summer_surveys)[names(summer_surveys) == "SURVEY_ID"] <- "SPRING_SURVEY_ID"
names(summer_surveys)[names(summer_surveys) == "Potamogeton_crispus"] <- "SPRING_Potamogeton_crispus"
names(summer_surveys)[names(summer_surveys) == "n_points_vegetated"] <- "SPRING_n_points_vegetated"
names(summer_surveys)[names(summer_surveys) == "tot_n_samp"] <- "SPRING_tot_n_samp"

names(summer_surveys)[names(summer_surveys) == "i.DATESURVEYSTART"] <- "DATESURVEYSTART"
names(summer_surveys)[names(summer_surveys) == "i.SURVEY_ID"] <- "SURVEY_ID"
names(summer_surveys)[names(summer_surveys) == "i.Potamogeton_crispus"] <- "Potamogeton_crispus"
names(summer_surveys)[names(summer_surveys) == "i.n_points_vegetated"] <- "n_points_vegetated"
names(summer_surveys)[names(summer_surveys) == "i.tot_n_samp"] <- "tot_n_samp"



# Deprecated join method:
# summer_surveys <- merge(summer_surveys, early_clpsurveys, by = c("DOW", "year", "LAKE_NAME", "SUBBASIN"), suffixes = c(".summer", ".spring"), all.x = T)

summer_surveys[ , hist(potcri_early_vegdfoc)]


#verify that summer surveys have clp spring data (NA indicates no spring survey was conducted capable of evaluating CLP abundance):

summer_surveys[ , .N , SPRING_Potamogeton_crispus==0]

#summary stats

#' here an NA indicates a DOW that we don't have any control records for. later
#' we'll make an assumption that no mgmt record means not managed.  
summer_surveys[ , summary(clp_ntrtdates>0) ,] 
summer_surveys[potcri_early_vegdfoc >0 , .N  , ]
summer_surveys[potcri_early_vegdfoc > 0 , summary(clp_ntrtdates>0) ,]
summer_surveys[is.na(potcri_early_vegdfoc) & clp_ntrtdates>0 , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART )  ,]
summer_surveys[ , summary(ewm_ntrtdates>0) ,]
summer_surveys[Myriophyllum_spicatum > 0 , summary(ewm_ntrtdates>0) ,]

#' Here are some cases where our treatment records indicate that the lake was
#' managed for EWM, but we don't have any observed EWM in the survey...
#' Which seems very wrong. Same thing occurs in both EWM and CLP 
#' 
#' 
summer_surveys[Myriophyllum_spicatum == 0 & ewm_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,]

summer_surveys[DOW %in% 
                 summer_surveys[Myriophyllum_spicatum == 0 & ewm_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,][, DOW],
               .(DOW, LAKE_NAME,DATESURVEYSTART, ewm_pretrt, Myriophyllum_spicatum, n_points_vegetated , ewm_ntrtdates), .(SURVEY_ID) ][order(DOW, DATESURVEYSTART)]


summer_surveys[is.na(clp_ntrtdates), .N , potcri_early_vegdfoc>0 ]

summer_surveys[DOW %in% 
                 summer_surveys[potcri_early_vegdfoc == 0 & clp_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,][, DOW],
               .(DOW, LAKE_NAME, SUBBASIN, DATESURVEYSTART, clp_pretrt, potcri_early_vegdfoc, SPRING_DATESURVEYSTART, DATASOURCE , clp_ntrtdates), .(SURVEY_ID) ][order(DOW, DATESURVEYSTART)]

# add GDD and maxdepths, yr of infest, and road densities from Shyam: ---------------------------------------

b[surveys, on = .(DOWLKNUM = DOW)][ ,.N , is.na(mean.gdd_wtr_10c) ]
surveys[b, GDD_10C := mean.gdd_wtr_10c, on = .(DOW = DOWLKNUM)]
surveys[b, max_depth_dow := max_depth, on = .(DOW = DOWLKNUM)]
surveys[b, year_EWM_infes := YEAR_INFESTED, on = .(DOW = DOWLKNUM)]
surveys[c, roads_500m_mperha := buffer500m_roaddensity_density_mperha, on = .(DOW = DOWLKNUM) ]

#' we can assume if no trt records post 2012, that the survey is on a 0 history lake - we'll do this now are re-visualize (this will add tons of zeros along the x axis)
surveys[is.na(clp_m) & year > 2011, clp_m:=0]
surveys[is.na(ewm_m)& year > 2011, ewm_m:=0]
surveys[is.na(clp_ntrtdates) & year > 2011, clp_ntrtdates:=0]
surveys[is.na(ewm_ntrtdates)& year > 2011, ewm_ntrtdates:=0]

surveys[ , ewm_managed := ewm_ntrtdates>0 ,]
surveys[ , clp_managed := clp_ntrtdates>0 ,]

surveys[ , ewm_m_priort := ewm_m-as.numeric(ewm_managed),  ]
surveys[ , clp_m_priort := clp_m-as.numeric(clp_managed),  ]

surveys[ is.na(ewm_m_priort), ewm_m_priort := 0,  ]
surveys[ is.na(clp_m_priort), clp_m_priort := 0,,  ]


surveys[ ewm_pretrt == 1, ewm_pretrt_mod := 1,  ]
surveys[ clp_pretrt == 1, clp_pretrt_mod := 1,  ]

surveys[is.na(ewm_pretrt_mod)| ewm_pretrt_mod != 1, ewm_pretrt_mod := 0  ]
surveys[is.na(clp_pretrt_mod)| clp_pretrt_mod != 1, clp_pretrt_mod := 0 ]

surveys[ , .N , .(clp_pretrt_mod, ewm_pretrt_mod)]



# does management reduce invaders? ----------------------------------------

#within year
ggplot(surveys[Myriophyllum_spicatum>0 & year>2011 & ewm_pretrt_mod == 0], aes(ewm_ntrtdates>0, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_boxplot()
ggplot(surveys[Potamogeton_crispus>0 & year>2011 & clp_pretrt_mod == 0 ], aes(clp_ntrtdates>0, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_boxplot()
# density plots show where our problems may be arising

ggplot(surveys[yday(DATESURVEYSTART) > 151 & Myriophyllum_spicatum > 0  & year > 2011], aes(Myriophyllum_spicatum/n_points_vegetated, color = ewm_managed) ,)+
  geom_density()

ggplot(surveys[yday(DATESURVEYSTART) < 181 & yday(DATESURVEYSTART) > 151 & Potamogeton_crispus > 0 & year > 2011], aes( Potamogeton_crispus/n_points_vegetated, color = clp_managed ))+
  geom_density()

#historical mgmt index
ggplot(surveys[Myriophyllum_spicatum>0 & year>2011 & ewm_pretrt_mod == 0], aes(ewm_m, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[Potamogeton_crispus>0 & year>2011 & clp_pretrt_mod == 0 ], aes(clp_m, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth(method = "lm")



#' Erm... NOPE: we see no strong correlation between more invader management index and less invader. That seems entirely counter intuitive.
#' 
#' How about we consider the magnitude of the treatments?

ggplot(surveys[Myriophyllum_spicatum>0 & ewm_pretrt_mod == 0 & year>2011],
       aes(ewm_controlacres, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[yday(DATESURVEYSTART)<181 & Potamogeton_crispus>0 & clp_pretrt_mod == 0& clp_controlacres<200], aes(clp_controlacres, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth(method = "lm")

surveys[ , hist(clp_controlacres) ,]
surveys[ , hist(log(clp_controlacres)) ,]

surveys[ , hist(ewm_controlacres) ,]
surveys[ , hist(log(ewm_controlacres)) ,]

#' change over time? Is that the key here? NOte that currently each year is a Y/N
#' for treatment, thus it's not entirely clear how these lines/slopes are
#' partitioned into managed/unmaganed where a given lake has multiple tim steps,
#' each varying in the management category...
#' this plot looks really promising 
ggplot(surveys[ Myriophyllum_spicatum>0& year>2011& ewm_pretrt_mod == 0 & yday(DATESURVEYSTART) > 151], aes(DATESURVEYSTART, Myriophyllum_spicatum/n_points_vegetated, color = ewm_managed))+
  geom_path(aes(group = DOW), alpha = .1)+
  geom_smooth(method = "lm", )

#plot these relative to the "infestation" year
ggplot(surveys[Myriophyllum_spicatum>0& year>2011& ewm_pretrt_mod == 0 & yday(DATESURVEYSTART) > 151], aes(year - year_EWM_infes, Myriophyllum_spicatum/n_points_vegetated, color = ewm_managed))+
  geom_path(aes(group = DOW), alpha = .1)+
  geom_smooth(method = "lm", )

#look only at surveys post infestation
ggplot(surveys[Myriophyllum_spicatum>0 & year>2011 & (year - year_EWM_infes)>=0 & ewm_pretrt_mod == 0 & yday(DATESURVEYSTART) > 151 ], aes(year - year_EWM_infes, Myriophyllum_spicatum/n_points_vegetated, color = ewm_managed))+
  geom_path(aes(group = DOW), alpha = .1)+
  geom_smooth(method = "lm", )

#again, make sure we're looking at only post-trt surveys.
ggplot(surveys[Myriophyllum_spicatum>0 & year>2011 & ewm_pretrt_mod == 0 & yday(DATESURVEYSTART) > 151 ], aes(DATESURVEYSTART, Myriophyllum_spicatum/n_points_vegetated, color = ewm_managed))+
  geom_path(aes(group = DOW), alpha = .1)+
  geom_smooth(method = "lm", )

#' I suspect this creates an artificial sense of a decline over time because 
#' there are low time-since-invasion lakes (with low abund EWM) that are showing
#' up in later years.
#' 
#' This idea/concept needs refinement--the mechanism is vague and not clearly
#' articulated. And the figure doesn't really add much help...
#' 
surveys[ , time_since_EWM := year - year_EWM_infes ,]

ggplot(surveys[], aes( time_since_EWM, Myriophyllum_spicatum/n_points_vegetated))+
  geom_point()+
  facet_wrap(~year)


ggplot(surveys[yday(DATESURVEYSTART) < 181 & yday(DATESURVEYSTART) > 151 & year > 2011 & clp_pretrt_mod == 0], aes(DATESURVEYSTART, Potamogeton_crispus/n_points_vegetated, color = clp_managed))+
  geom_path(aes(group = DOW), alpha = .1)+
  geom_smooth(method = "lm", )

#' Alrighty... we can start to see why we need causal inference methods in this
#' analysis. Because treatment decisions are influenced by how much of the
#' invader is there, our most treated lakes are those with the highest
#' abundances of the invader. We need a "counterfactual" that represents how
#' the abundance of invader would have been in lake-year (Xt) had m been zero.
#' If we limit our analyses to ONLY lakes with the invader, the prob starts to
#' go away (an effect that is less drastic for CLP b/c most early season CLP 
#' surveys are done on lakes where we expect CLP to be present):




# trt eff on T+1 ----------------------------------------------------------


surveys[ , sum(clp_managed)  , .(DOW, SUBBASIN)][V1>0]

clp_set <- surveys[ month(DATESURVEYSTART) %in% c(3:6) & year > 2011]

clp_set[, .N ,month(DATESURVEYSTART)]
clp_set[, .N , DOW][N>1, DOW]
clp_set[is.na(clp_managed), clp_managed := F]
clp_set[ , clp_vfoc := Potamogeton_crispus/n_points_vegetated ,]
clp_set[month(DATESURVEYSTART)==6 , surveytiming := "peak",]
clp_set[month(DATESURVEYSTART)!=6 , surveytiming := "early",]


ggplot(data = clp_set[DOW %in% clp_set[, .N , DOW][N>1, DOW]], aes( year,clp_vfoc ))+
  geom_line(aes(color = month(DATESURVEYSTART) == 6))+
  facet_wrap(~DOW)+
  geom_point(aes(year,clp_vfoc, color = clp_managed))


clp_mgmt <- melt(dcast(clp_set, DOW+SUBBASIN~year, value.var = c("clp_managed")), id.vars = c("DOW","SUBBASIN"), variable.name = "year", value.name = "managed")

clp_mgmt[ , managed_last_y := shift(managed, type = "lag"), .(DOW,SUBBASIN)]
clp_mgmt[ , year := as.integer(as.character(year)) ,]

clp_set[clp_mgmt, on = .(year = year, DOW = DOW, SUBBASIN = SUBBASIN), managed_last_y := managed_last_y]


ggplot(clp_set[surveytiming == "early" ], aes( clp_vfoc, color = as.factor(managed_last_y) ))+
  geom_density()

ggplot(clp_set[surveytiming == "early" ], aes( managed_last_y == T, clp_vfoc ) )+
  geom_boxplot()
ggplot(clp_set[surveytiming == "peak" ], aes( managed_last_y == T, clp_vfoc ) )+
  geom_boxplot()

summary(lm(data = clp_set[surveytiming == "early" ], clp_vfoc ~ managed_last_y) )
summary(lm(data = clp_set[surveytiming == "early" ], clp_vfoc ~ managed_last_y + clp_m_priort) )
summary(lm(data = clp_set[surveytiming == "peak" ], clp_vfoc ~ clp_managed + managed_last_y + clp_m_priort ))

#natives response



#and EWM?

ewm_set <- surveys[ year > 2011 & yday(DATESURVEYSTART) > 151, , ]

ewm_mgmt <- melt(dcast(ewm_set, DOW+SUBBASIN~year, value.var = c("ewm_managed")), id.vars = c("DOW","SUBBASIN"), variable.name = "year", value.name = "managed")

ewm

ewm_mgmt[ , managed_last_y := shift(managed, type = "lag"), .(DOW,SUBBASIN)]
ewm_mgmt[ , year := as.integer(as.character(year)) ,]

ewm_set[ewm_mgmt, on = .(year = year, DOW = DOW, SUBBASIN = SUBBASIN), managed_last_y := managed_last_y>0]
ewm_set[ , ewm_vfoc := Myriophyllum_spicatum/n_points_vegetated ,]

ggplot(ewm_set[Myriophyllum_spicatum>0], aes( ewm_vfoc, color = as.factor(managed_last_y) ))+
  geom_density()+scale_x_log10()

ggplot(ewm_set[Myriophyllum_spicatum>0], aes( managed_last_y == 1, ewm_vfoc ) )+
  geom_boxplot()

summary(lm(data = ewm_set[Myriophyllum_spicatum>0], ewm_vfoc ~ managed_last_y) )
summary(lm(data = ewm_set[Myriophyllum_spicatum>0], ewm_vfoc ~ managed_last_y + ewm_m_priort) )
summary(lm(data = ewm_set[Myriophyllum_spicatum>0 & ewm_pretrt_mod==0], ewm_vfoc ~ ewm_managed + managed_last_y + ewm_m_priort ))

# native response - plain jane
summary(lm(data = ewm_set[Myriophyllum_spicatum>0& ewm_pretrt_mod == 1], simpsons_div_nat ~ managed_last_y) )
summary(lm(data = ewm_set[Myriophyllum_spicatum>0& ewm_pretrt_mod == 1], simpsons_div_nat ~ managed_last_y + ewm_m_priort) )
summary(lm(data = ewm_set[Myriophyllum_spicatum>0 & ewm_pretrt_mod == 0], simpsons_div_nat ~ ewm_managed + managed_last_y + ewm_m_priort ))

summary(lm(data = ewm_set[Myriophyllum_spicatum>0& ewm_pretrt_mod == 1], simpsons_div_nat ~ ewm_vfoc) )


# trt eff w/in yr BACI - CLP ----------------------------------------------



clp_set[ , .N , .(DOW,year)][N>1, paste(DOW,year)]


clp_set_baci <- clp_set[paste(DOW, year) %in% clp_set[ , .N , .(DOW,SUBBASIN,year)][N>1, paste(DOW,year)], ][order(DOW, SUBBASIN, DATESURVEYSTART)]

clp_set_baci[ , prev_abund := shift(clp_vfoc) , .(DOW,SUBBASIN,year) ]

clp_set_baci[ ,.(DOW, year, DATESURVEYSTART, clp_vfoc, clp_managed, MULTIPARTSURVEY,SUBBASIN, prev_abund) , ]

clp_set_baci[month(DATESURVEYSTART) == 6, delta_clp := clp_vfoc-prev_abund ]

ggplot(clp_set_baci, aes( delta_clp, color = as.factor(clp_managed) ))+
  geom_density()

ggplot(clp_set_baci[surveytiming == "peak"], aes( delta_clp, as.factor(clp_managed) ))+
  geom_boxplot()

summary(lm(delta_clp~clp_managed, data=clp_set_baci[surveytiming == "peak"]))
summary(lm(delta_clp~clp_managed + managed_last_y, data=clp_set_baci[surveytiming == "peak"]))
summary(lm(delta_clp~clp_managed + managed_last_y + clp_m_priort, data=clp_set_baci[surveytiming == "peak"]))
summary(lm(delta_clp~clp_managed + clp_m_priort, data=clp_set_baci[surveytiming == "peak"]))

# trt eff BACI across yrs -------------------------------------------------


clp_set[surveytiming == "peak", .N]
clp_set[surveytiming == "peak", .N , .(DOW,SUBBASIN)][N>1, paste(DOW,SUBBASIN)]
setorder(clp_set, DOW,SUBBASIN,year)

clp_set_baci_yrs <- clp_set[surveytiming == "peak", , ]


clp_set_baci_yrs[ , previous_abund := shift(clp_vfoc)  , . (DOW,SUBBASIN)]

clp_set_baci_yrs[ , delta_y_abund := clp_vfoc - previous_abund  ,]

ggplot(clp_set_baci_yrs, aes( delta_y_abund, color = as.factor(clp_managed) ))+
  geom_density()

ggplot(clp_set_baci_yrs, aes( delta_y_abund, as.factor(clp_managed) ))+
  geom_boxplot()

summary(lm(data = clp_set_baci_yrs, delta_y_abund~clp_managed))
summary(lm(data = clp_set_baci_yrs, delta_y_abund~clp_managed + managed_last_y))

#EWM

setorder(ewm_set,DOW,SUBBASIN,year)

ewm_set <- ewm_set[!duplicated(ewm_set[ , .(DOW,SUBBASIN,year) ,]) , ,]

ewm_set[ , previous_abund := shift(ewm_vfoc)  , . (DOW,SUBBASIN)]

ewm_set[ , delta_y_abund := ewm_vfoc - previous_abund  ,]

ggplot(ewm_set[ewm_pretrt_mod == 0], aes( delta_y_abund, color = as.factor(ewm_managed) ))+
  geom_density()

ggplot(ewm_set[ewm_pretrt_mod == 0], aes( delta_y_abund, as.factor(ewm_managed) ))+
  geom_boxplot()

summary(lm(data = ewm_set[ewm_pretrt_mod == 0], delta_y_abund~ewm_managed))
summary(lm(data = ewm_set[ewm_pretrt_mod == 0], delta_y_abund~ewm_managed + managed_last_y))




#' To address this problem, we can use an approach called matching. With
#' matching we will choose a set of "control" surveys that represent a
#' sample from the unmanaged set that are chosen to be good "counterfactuals"
#' for the sample of managed lakes.
#' 
#' From: https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html
#' The goal of matching is to produce covariate balance, that is, for the
#' distributions of covariates in the two groups to be approximately equal to
#' each other, as they would be in a successful randomized experiment.
#' 
#' Covariates should be those that cause variation in the outcome and selection
#' into treatment group; these are known as confounding variables. See
#' VanderWeele (2019) for a guide on covariate selection. Ideally these
#' covariates are measured without error and are free of missingness. 
#' 
#' From VanderWeele: control for each covariate that is a cause of the exposure,
#' or of the outcome, or of both; exclude from this set any variable known to be
#' an instrumental variable; and include as a covariate any proxy for an
#' unmeasured variable that is a common cause of both the exposure and the
#' outcome.
#' 
#' 
#' So we need to decide which matching covariates cause var in being chosen for
#' mgmgt and those that cause the invader abund to change. We can see from
#' Shyams paper that MaxDep, SecchiDep, and Lake Area all panned out in the top
#' half of predictors for EWM abund (and P/A, too).
#' 

# No matching; constructing a pre-match matchit object


#run matching for EWM:
surveys[is.na(ewm_managed) , ewm_managed := F  , ]

surveys[ ,.N,  is.na(Secchi_m) ]

m.out0 <- matchit(ewm_managed ~ ewm_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   yday(DATESURVEYSTART) > 151 &
                                   Myriophyllum_spicatum > 0 & 
                                   ewm_pretrt_mod == 0],
                  method = NULL, distance = "glm", link = "probit")


# Checking balance prior to matching
summary(m.out0)

# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(ewm_managed ~ ewm_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   yday(DATESURVEYSTART) > 151 &
                                   Myriophyllum_spicatum > 0 & 
                                   ewm_pretrt_mod == 0],
                  method = "nearest", distance = "glm", link = "probit")

m.out1

# Checking balance after NN matching
summary(m.out1, un = FALSE)

plot(m.out1, type = "jitter", interactive = FALSE)

plot(m.out1, type = "qq", interactive = FALSE,
     which.xs = c("GDD_10C", "Secchi_m", "ewm_m_priort"))
plot(m.out1, type = "qq", interactive = FALSE,
     which.xs = c("max_depth_dow", "roads_500m_mperha"))

# Full matching on a probit PS
m.out2 <- matchit(ewm_managed ~ ewm_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   yday(DATESURVEYSTART) > 151 &
                                   Myriophyllum_spicatum > 0 & 
                                   ewm_pretrt_mod == 0],
                  method = "full", distance = "glm", link = "probit")
m.out2

# Checking balance after full matching
summary(m.out2, un = FALSE)

plot(m.out2, type = "jitter", interactive = FALSE)

plot(m.out2, type = "qq", interactive = FALSE,
     which.xs = c("GDD_10C", "Secchi_m", "ewm_m_priort"))
plot(m.out2, type = "qq", interactive = FALSE,
     which.xs = c("max_depth_dow", "roads_500m_mperha"))

plot(summary(m.out2))

# EWM - Analysis of matched set of survey data ----------------------------------------

m.data1 <- match.data(m.out1)

head(m.data1)

#Test for effect on invader abund:

fit1 <- glm(Myriophyllum_spicatum/n_points_vegetated ~ ewm_managed + ewm_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class)  , data = m.data1, weights = weights, family = binomial(link = "logit"))


coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)

#and recall full match had better balance:

m.data2 <- match.data(m.out2)

fit2 <- glm(Myriophyllum_spicatum/n_points_vegetated ~ ewm_managed + ewm_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class)  , data = m.data2, weights = weights, family = binomial(link = "logit"))

coeftest(fit2, vcov. = vcovCL, cluster = ~subclass)

#repeat matching steps for CLP:

surveys[is.na(clp_managed) , clp_managed := F  , ]

surveys[ ,.N,  Secchi_m ]

m.out0.1 <- matchit(clp_managed ~  clp_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   !is.na(roads_500m_mperha) &
                                   yday(DATESURVEYSTART) < 181 & 
                                   clp_pretrt_mod == 0],
                  method = NULL, distance = "glm", link = "probit")


# Checking balance prior to matching
summary(m.out0.1)

# 1:1 NN PS matching w/o replacement
m.out1.1 <- matchit(clp_managed ~  clp_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   !is.na(roads_500m_mperha)&
                                   yday(DATESURVEYSTART) < 181 & 
                                   ewm_pretrt_mod == 0],
                  method = "nearest", distance = "glm", link = "probit")

m.out1.1

# Checking balance after NN matching
summary(m.out1, un = FALSE)

plot(m.out1.1, type = "jitter", interactive = FALSE)

plot(m.out1.1, type = "qq", interactive = FALSE,
     which.xs = c("GDD_10C", "Secchi_m", "clp_m_priort"))
plot(m.out1.1, type = "qq", interactive = FALSE,
     which.xs = c("max_depth_dow", "roads_500m_mperha"))

# Full matching on a probit PS
m.out2.1 <- matchit(clp_managed ~  clp_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), 
                  data = surveys[!is.na(Secchi_m) &
                                   !is.na(GDD_10C)&
                                   !is.na(max_depth_dow)&
                                   !is.na(roads_500m_mperha)&
                                   yday(DATESURVEYSTART) < 181 & 
                                   ewm_pretrt_mod == 0],
                  method = "full", distance = "glm", link = "probit")
m.out2.1

# Checking balance after full matching
summary(m.out2.1, un = FALSE)

plot(m.out2.1, type = "jitter", interactive = FALSE)

plot(m.out2.1, type = "qq", interactive = FALSE,
     which.xs = c("GDD_10C", "Secchi_m", "clp_m_priort"))
plot(m.out2.1, type = "qq", interactive = FALSE,
     which.xs = c("max_depth_dow", "roads_500m_mperha"))

plot(summary(m.out2.1))

# CLP - Analysis of matched set of survey data ----------------------------------------

m.data1.1 <- match.data(m.out1.1)

head(m.data1.1)

#Test for effect on invader abund:

fit1.1 <- glm(Potamogeton_crispus/n_points_vegetated ~ clp_managed + clp_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), data = m.data1.1, weights = weights, family = binomial(link = "logit"))

coeftest(fit1.1, vcov. = vcovCL, cluster = ~subclass)

#and recall full match had better balance:

m.data2.1 <- match.data(m.out2.1)

fit2.1 <- glm(Potamogeton_crispus/n_points_vegetated ~ clp_managed + clp_m_priort + GDD_10C + max_depth_dow + Secchi_m + roads_500m_mperha + as.factor(lake_class), data = m.data2.1, weights = weights, family = binomial(link = "logit"))

coeftest(fit2.1, vcov. = vcovCL, cluster = ~subclass)

#' Let's take stock of things now: 
#' 1. For both species, matching got us to NULL diffs in trt v. untrt
#' 1b. We chose covariates based on Mine and Shyam's work, and carried those
#' through to both species. My work on CLP prob indicates we could include 
#' winter severity in here.
#' 2. I have tried some other things too, which don't change the results re: treatment effects:
#'  - dropped lake class for both species
#'  - added Date of survey for the CLP set (because of strong phenological effect)
#'  - 
#' 
#' 
#' 
#' 
#' 7. Once we test the relationship of trt on invaders, we'll next want to ask
#' what the relationship of trt on natives is --through the invaders. In order
#' to do this, I imagined an instrumental variable approach. BUT it doesn't 
#' seem like we have a decent instrument (if treatment doesn't reliably change
#' the abundance of the invader, then there's no reason to believe it could
#' change the invader abund enough to drive subsequent changes in the native
#' community ). As such, we're just going to do something similar to the invader
#' control matching work, but now asking if native diversity shows a treatment
#' effect. 
#' 
#' For EWM this is pretty straighforward, the timing of the EWM surveys is great
#' for capturing the native plant abundances. 
#' 
#' For CLP, however, we might need to be clever b/c of timing of surveys.
#' 
#' Our desired result: the Treatment effect on Native Communities (rich, div, even, mvAbund)
#' 
#' To get that, we can look at the raw effect:

# does management help natives? ----------------------------------------
names(surveys)

ggplot(surveys[Myriophyllum_spicatum>0 & yday(DATESURVEYSTART)>151], aes(ewm_m, nat_richness ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[Myriophyllum_spicatum>0 & yday(DATESURVEYSTART)>151], aes(ewm_m, simpsons_div_nat ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[Myriophyllum_spicatum>0 & yday(DATESURVEYSTART)>151], aes(ewm_m, simpsons_div_nat/nat_richness ) ,)+
  geom_point()+ geom_smooth(method = "lm")

ggplot(surveys[yday(DATESURVEYSTART)>151], aes(clp_m, nat_richness ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[yday(DATESURVEYSTART)>151], aes(clp_m, simpsons_div_nat ) ,)+
  geom_point()+ geom_smooth(method = "lm")
ggplot(surveys[yday(DATESURVEYSTART)>151], aes(clp_m, simpsons_div_nat/nat_richness ) ,)+
  geom_point()+ geom_smooth(method = "lm")


#' we can also take a simpler look at a within yr effect separated from previous
#' treatment effects:
#' 

ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(ewm_managed, nat_richness ) ,)+
  geom_boxplot()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(ewm_managed, simpsons_div_nat ) ,)+
  geom_boxplot()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(ewm_managed, simpsons_div_nat/nat_richness ) ,)+
  geom_boxplot()

ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_managed, nat_richness ) ,)+
  geom_boxplot()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_managed, simpsons_div_nat ) ,)+
  geom_boxplot()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_managed, simpsons_div_nat/nat_richness ) ,)+
  geom_boxplot()

ggplot(surveys[yday(DATESURVEYSTART) > 151, ], aes(ewm_m_priort, nat_richness ) ,)+
  geom_point()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(ewm_m_priort, simpsons_div_nat ) ,)+
  geom_point()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(ewm_m_priort, simpsons_div_nat/nat_richness ) ,)+
  geom_point()

ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_m_priort, nat_richness ) ,)+
  geom_point()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_m_priort, simpsons_div_nat ) ,)+
  geom_point()
ggplot(surveys[yday(DATESURVEYSTART) > 151], aes(clp_m_priort, simpsons_div_nat/nat_richness ) ,)+
  geom_point()








names(summer_surveys)



#' #how many CLP lake-years of data (note the NAs are ones where we cant reliably say clp is/is not there )
#' summer_surveys[ , .N , SPRING_Potamogeton_crispus>0]
#' 
#' #how many EWM lake-years?
#' summer_surveys[ , .N , Myriophyllum_spicatum>0]
#' 
#' #and whats the management breakdown?
#' 
#' summer_surveys[!is.na(SPRING_Potamogeton_crispus) , .(.N,mean(potcri_early_vegdfoc)) , clp_managed]
#' 
#' summer_surveys[ , .(.N,mean(myrspi_summer_vegdfoc)) , ewm_managed ]
#' 
#' #' These values highlight some of the big problems with the management data.
#' #' 1. They're super shitty and we've retained barely a sliver of info showing
#' #' which systems have been treated
#' #' - I can work on this on my end, ensuring that we've maximized our data
#' #' retention during the join to mgmt data
#' #' - Part of this will just be a truth of trying to examine mgmt data.
#' #' 2. We've got a system where lakes are selected for control because they have
#' #' an overabundance of the invader. This creates an artificial sense that 
#' #' abundances are HIGHER where the invader is being controlled. 
#' #' 
#' #' 
#' #' 
#' #'     
#' 
#'   #rrreal quick:
#'   ggplot(summer_surveys, aes(potcri_early_vegdfoc, Secchi_m) , )+
#'     geom_point( aes(color= clp_managed))+
#'     geom_smooth(method = "lm")
#'   
#'   ggplot(summer_surveys, aes(jitter(myrspi_summer_vegdfoc), Secchi_m) , )+
#'     geom_point( aes(color= ewm_managed), alpha = .1)+
#'     geom_smooth(method = "lm")
#'   
  







