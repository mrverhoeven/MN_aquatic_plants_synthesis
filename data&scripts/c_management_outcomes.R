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
# update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
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


# load in data -------------------------------------------------
# # 
# plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
# plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
# plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
surveys <- fread(file = "data&scripts/data/output/surveys_aqplants.csv")

load("data&scripts/data/output/synthesis_script_datasets.Rdata")

#management data
source(file = "data&scripts/c1_management_outcomes.R") # will load and clean management data and leave in WS a load of mgmtdata files

surveys_mgmt <- fread( file = "data&scripts/data/output/survs_mgmt_dateprelabelled.csv")#requires synthesis script datasets

#other covariates?
#shyams EWM proj Data?:
# here we have depth, GDD, others
b <- fread("data&scripts/data/input/EWMlakeindex.allwatchem.NLDASwtmp2.csv")
# here we have road density w/in 500m buffer
c <- fread(file = "data&scripts/data/input/Lakes_RoadDensity.csv")


# discussion --------------------------------------------------------------


#' ## Approaches to Analysis:
#' 
#' ### I. Space for time mashup 
#' 
#' Large sample n, simple interpretation (seemingly, but has issue of reverse
#' causality). 
#' 
#' We've got a pile of data, but its a messy pile. We've got lots of repeat
#' sampling, but it's not consistent intervals and there are lots of holes. The
#' way we treated this previously (Verhoeven, Larkin, Newman 2020) was to run
#' models with lake ID as a random effect. We can try that again here, or we
#' might consider other options.
#' 
#' ### II. In year, carryover, and historical control effects
#' 
#' For each survey an associated value for historical control (m--an index of 
#' mgmt = sum(1 / 1+ (current year - management year)) eg:
#' for 2019 survey with 2017 and 2015 management, mgmt index = 
#' sum( 1/3 + 1/5)
#'  
#' Differences among lakes via random slopes and intercepts.
#' 
#' ### III. Matching in space for time mashup
#' 
#' We use an approach designed to land us better counterfactuals for our managed
#' group. This is essentially trying to circumvent the reverse causality peice, 
#' wherein lakes are selected for control based on the fact the invader is
#' abundant. This creates an artificial sense that high invaders are a result of
#' management. 
#' 
#' 
#' ### IV. BACI 
#' 
#' we can assess the treatment effect as measured by differences in change over
#' time of invaders or natives. This approach asks if temporal changes in 
#' managed systems are different from those of unmanaged systems, which is 
#' essentially a BACI design.
#' 
#' We do this as a within year design, and tease with a bit of across y analysis.
 
#' 
#' ## Issues to address:
#' 
#' ### "Managed"
#' 
#' We have a somewhat poor idea of which lakes received management to control
#' EWM or CLP in MN. We've got a dataset that does our best to assess this 
#' question for 2012-2018. My gut feeling (and nothing more than that) tells me 
#' we've probably got 80% of the CLP & EWM management efforts in that timeframe
#' captured in our dataset.
#' 
#' Then there's the question of what it means to be "managed." You might have
#' managed the entire pop extent in your lake--or maybe you just managed 5% of
#' the pop extent because you didn't have the money. Maybe you chose to use an
#' herbicide control approach that used repeated herbicide in the same year 10x 
#' (EWM). Or maybe you've used some combination of herb and mechanical control. 
#' Did you control every other year? Every year? Why?
#' 
#' We've got a tiered set of metrics that move from least precise about control
#' (this lake-year was managed for AN invader), to most precise (this lake-year
#' was managed for this invader on this date on X acres--at this point I have 
#' not included method, but could if we dream up a clean way to do that.). It
#' seems to me that data with no targeted species info is not useful to us. For
#' that reason you'll see we begin with the species specific control data. 
#' 
#' ### Effects of Management
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
#'  
#' Take note as we move through these analyses, which metric is being used 
#' to measure the response. 
#'  
#' 
# data prep ---------------------------------------------------------------

#clean up the management data: (assign zeros or NAs as appropriate)

names(surveys_mgmt)
# only had trt data from 2012 onward. So exclude pre 2012, but recall the the mgmt index only reaches back to 2012... :
surveys_mgmt[clp_ntrtdates>0 ,hist(year, breaks = 9) , ]
surveys_mgmt[ewm_ntrtdates>0 ,hist(year, breaks = 9) , ]

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

surveys[!SURVEY_ID %in% surveys_mgmt[ , SURVEY_ID], .(SURVEY_ID, DOW, LAKE_NAME) , ]

# surveys <- surveys_mgmt
# rm(surveys_mgmt)

# add some covariates (Thanks Shyam!)
b[surveys, on = .(DOWLKNUM = DOW)][ ,.N , is.na(mean.gdd_wtr_10c) ]
surveys[b, GDD_10C := mean.gdd_wtr_10c, on = .(DOW = DOWLKNUM)]
surveys[b, max_depth_dow := max_depth, on = .(DOW = DOWLKNUM)]
surveys[b, year_EWM_infes := YEAR_INFESTED, on = .(DOW = DOWLKNUM)]
surveys[c, roads_500m_mperha := buffer500m_roaddensity_density_mperha, on = .(DOW = DOWLKNUM) ]


#' we can assume if no trt records post 2012, that the survey is on a 0 history lake - we'll do this now are re-visualize (this will add tons of zeros along the x axis)
surveys_mgmt[is.na(clp_m) & year > 2011, clp_m:=0]
surveys_mgmt[is.na(ewm_m)& year > 2011, ewm_m:=0]
surveys_mgmt[is.na(clp_ntrtdates) & year > 2011, clp_ntrtdates:=0]
surveys_mgmt[is.na(ewm_ntrtdates)& year > 2011, ewm_ntrtdates:=0]

surveys_mgmt[ , ewm_managed := ewm_ntrtdates>0 ,]
surveys_mgmt[ , clp_managed := clp_ntrtdates>0 ,]

surveys_mgmt[ , ewm_m_priort := ewm_m-as.numeric(ewm_managed),  ]
surveys_mgmt[ , clp_m_priort := clp_m-as.numeric(clp_managed),  ]

surveys_mgmt[ is.na(ewm_m_priort), ewm_m_priort := 0,  ]
surveys_mgmt[ is.na(clp_m_priort), clp_m_priort := 0,,  ]

surveys_mgmt[ ewm_pretrt == 1, ewm_pretrt_mod := 1,  ]
surveys_mgmt[ clp_pretrt == 1, clp_pretrt_mod := 1,  ]

surveys_mgmt[is.na(ewm_pretrt_mod)| ewm_pretrt_mod != 1, ewm_pretrt_mod := 0  ]
surveys_mgmt[is.na(clp_pretrt_mod)| clp_pretrt_mod != 1, clp_pretrt_mod := 0 ]

surveys_mgmt[ , .N , .(clp_pretrt_mod, ewm_pretrt_mod)]

surveys_mgmt[ ,DATESURVEYSTART := as.IDate(DATESURVEYSTART, format = "%m/%d/%Y") , ]

surveys_mgmt[ , p_dow := round(DOW/100, 0)]

# data familiarization ----------------------------------------------------


#' 


#invader as a fn of mgmt:

#distribution of peak season abunds
surveys[ yday(DATESURVEYSTART) < 181 & yday(DATESURVEYSTART) > 151 & !is.na(DOW) ,
         .(SURVEY_ID,DOW, SUBBASIN, p_dow, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated),
         ][ , hist(clp_vfoc) ,]

surveys[ yday(DATESURVEYSTART)<181 & yday(DATESURVEYSTART)>151 & !is.na(DOW) ,
         .(SURVEY_ID,DOW, SUBBASIN, p_dow, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated),
         ][clp_vfoc>0 , hist(clp_vfoc) ,]


#within year pre-post timed surveys (only reasonable to estimate for CLP):

# BACI clp in yr data prep -------------------------------------------------------


surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN, year)][N>2]

surveys[ DOW == 27011700 , .(DOW, SUBBASIN, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated, SURVEY_ID) , ]
surveys[ DOW == 2000400 , .(DOW, SUBBASIN, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated, SURVEY_ID) , ]
surveys[ DOW == 27019200 , .(DOW, SUBBASIN, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated, SURVEY_ID) , ]

#drop survey #787, 1395, 1275
surveys <- surveys[!SURVEY_ID %in% c(787, 1395, 1275)]
surveys <- surveys[!is.na(DOW)]

surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN, year)][N==2, paste(DOW,SUBBASIN,year)]

surveys[yday(DATESURVEYSTART)<181 &
          paste(DOW,SUBBASIN,year) %in% 
          surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN, year)][N==2, paste(DOW,SUBBASIN,year)],
        .(DOW, SUBBASIN, year, DATESURVEYSTART, clp_vfoc = Potamogeton_crispus/n_points_vegetated, SURVEY_ID) ]



clp_BACI_inyr <- surveys[yday(DATESURVEYSTART)<181 &
                           paste(DOW,SUBBASIN,year) %in% 
                           surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN, year)][N==2, paste(DOW,SUBBASIN,year)],
                         .(DOW, SUBBASIN, year, DATESURVEYSTART, Potamogeton_crispus, n_points_vegetated, SURVEY_ID, simpsons_div_nat, nat_richness) ]

clp_mgmt_BACI_inyr <- s2_mgmt_wdates[species == "clp_targeted" & !is.na(date1), .(dow ,lakename , year , date1)  , ]

clp_mgmt_BACI_inyr[ , date := as.IDate(date1) , ]


# BACI clp in yr fix lakenames in mgmt data ----------------------------------------------
#merge mgmt data on dows & years
clp_mgmt_BACI_inyr[ , p_dow := round(dow/100, 0) ,]

#minnetonka
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

clp_mgmt_BACI_inyr[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
clp_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

clp_mgmt_BACI_inyr[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
clp_mgmt_BACI_inyr[, sort(unique(lakename)),]


clp_mgmt_BACI_inyr[lakename == "koronis_(main_lake)", subbasin := "main lake"]
clp_mgmt_BACI_inyr[lakename == "koronis_(main_lake)", lakename := "koronis"]

clp_mgmt_BACI_inyr[lakename == "island(southbay)", subbasin := "south basin"]
clp_mgmt_BACI_inyr[lakename == "island(southbay)", lakename := "island"]

clp_mgmt_BACI_inyr[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
clp_mgmt_BACI_inyr[lakename == "southeast_anderson", lakename := "anderson"]


clp_mgmt_BACI_inyr[lakename == "zumbra", subbasin := "Zumbra"]
clp_mgmt_BACI_inyr[lakename == "zumbra", lakename := "Zumbra-Sunny"]
clp_mgmt_BACI_inyr[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]


# BACI clp in yr merge clp mgmt and surveys ----------------------------------------------

clp_BACI_inyr[ , p_dow := round(DOW/100, 0) , ]

setkey(clp_mgmt_BACI_inyr, p_dow, subbasin, year )
setkey(clp_BACI_inyr, p_dow, SUBBASIN, year)



clp_BACI_inyr[clp_mgmt_BACI_inyr, on = .(SUBBASIN = subbasin, p_dow, year) ,trt_date := date ]

clp_BACI_inyr <- clp_BACI_inyr[year>2011]

setorder(clp_BACI_inyr, DATESURVEYSTART)



clp_BACI_inyr[ , pre_trt_survey := (DATESURVEYSTART-trt_date) <= 0 , ]
clp_BACI_inyr[ , d_post_trt := (DATESURVEYSTART-trt_date) , ]
clp_BACI_inyr[ , clp_vfoc := Potamogeton_crispus/n_points_vegetated  , ]
clp_BACI_inyr[ , prev_abund := shift(clp_vfoc) , .(DOW, SUBBASIN, year) ]
clp_BACI_inyr[ , prev_survey_date := shift(DATESURVEYSTART) , .(DOW, SUBBASIN, year) ]
clp_BACI_inyr[ , dN := clp_vfoc-prev_abund ,]
clp_BACI_inyr[ , ddays := DATESURVEYSTART-prev_survey_date ,]

clp_BACI_inyr[trt_date<DATESURVEYSTART & trt_date>prev_survey_date , good_BACI := T]
clp_BACI_inyr[!trt_date<DATESURVEYSTART | !trt_date>prev_survey_date , good_BACI := F]
clp_BACI_inyr[is.na(trt_date) , good_BACI := NA]

clp_BACI_inyr[ , nat_evenness := simpsons_div_nat/nat_richness ,]
clp_BACI_inyr[ , prev_even := shift(nat_evenness) , .(DOW, SUBBASIN, year) ]
clp_BACI_inyr[ , prev_rich := shift(nat_richness) , .(DOW, SUBBASIN, year) ]
clp_BACI_inyr[ , prev_div := shift(simpsons_div_nat) , .(DOW, SUBBASIN, year) ]

clp_BACI_inyr[ , ddiv := simpsons_div_nat-prev_div ,]
clp_BACI_inyr[ , drich := nat_richness-prev_rich ,]
clp_BACI_inyr[ , deven := nat_evenness-prev_even ,]

clp_BACI_inyr[!is.na(dN) , .N , good_BACI]


# BACI clp in yr viz & test ------------------------------------------



ggplot(clp_BACI_inyr, aes(!is.na(trt_date), dN))+
  geom_boxplot()+
  xlab("managed")
ggplot(clp_BACI_inyr, aes(dN/ddays, color = is.na(trt_date)))+
  geom_density()+
  scale_color_discrete(labels = c("managed", "unmanaged"))
ggplot(clp_BACI_inyr, aes(good_BACI, dN))+
  geom_boxplot()

summary(lm(data = clp_BACI_inyr, dN ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr[good_BACI == T | is.na(good_BACI)], dN ~ is.na(trt_date) ))

# BACI clp in yr natives --------------------------------------------------


ggplot(clp_BACI_inyr, aes(!is.na(trt_date), ddiv))+
  geom_boxplot()+
  geom_point()+
  xlab("managed")
ggplot(clp_BACI_inyr, aes(ddiv, color = is.na(trt_date)))+
  geom_density()+
  scale_color_discrete(labels = c("managed", "unmanaged"))
ggplot(clp_BACI_inyr, aes(good_BACI, ddiv))+
  geom_boxplot()+
  geom_point()
summary(lm(data = clp_BACI_inyr, ddiv ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr[good_BACI == T | is.na(good_BACI)], ddiv ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr, deven ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr[good_BACI == T | is.na(good_BACI)], deven ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr, drich ~ is.na(trt_date) ))
summary(lm(data = clp_BACI_inyr[good_BACI == T | is.na(good_BACI)], drich ~ is.na(trt_date) ))




# BACI clp in yr underlying data ------------------------------------------



# here's the data underlying this analysis:
surveys[SURVEY_ID %in% clp_BACI_inyr[!is.na(dN) , SURVEY_ID , ] , .(SURVEY_ID, DOW, LAKE_NAME, SUBBASIN, p_dow, year, DATASOURCE)  , ][clp_BACI_inyr[!is.na(dN) , .(SURVEY_ID,trt_date, dN, ddays, ddiv, drich, deven) , ], on = .(SURVEY_ID)][order(DOW,SUBBASIN, year)]
#years from each lake:
surveys[SURVEY_ID %in% clp_BACI_inyr[!is.na(dN) , SURVEY_ID , ] , .(SURVEY_ID, DOW, LAKE_NAME, SUBBASIN, p_dow, year, DATASOURCE)  , ][clp_BACI_inyr[!is.na(dN) , .(SURVEY_ID,trt_date, dN, ddays, ddiv, drich, deven) , ], on = .(SURVEY_ID)][order(DOW,SUBBASIN, year)][ ,.N, .(LAKE_NAME, DOW, SUBBASIN)]

# BACI clp in yr discussion --------------------------------------------------------------
#' ### CLP within year effects
#' 
#' From 3173 surveys, we have 969 that were collected pre-July
#' From these, 258 surveys, or 129 lake years of data have more than one survey
#' and 51 lake years of that is post 2012 data (we can't know mgmt for pre 2012
#' data; note that the CLP project used data from 2006-2015).
#' We've also got 1101 management records for lake-years of control 2012-2018.
#' Of the 51 lake years, our mgmt records show that 16 of those were 
#' treated, and only 11 of the 16 have proper BACI timing for surveys of pcri N.
#' 
#' 


# BACI ewm in yr trt eff -------------------------------------------------------


surveys[year>2011 & yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN, year)][N>1]

surveys[year>2011 & yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN, year)][N>2 ]

surveys[ paste(year, DOW) %in% 
           surveys[year>2011 & yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN, year)][N>2, paste(year, DOW)],
         .(DOW, SUBBASIN, year, DATESURVEYSTART, Myriophyllum_spicatum, n_points_vegetated, ewm_vfoc = Myriophyllum_spicatum/n_points_vegetated, SURVEY_ID, simpsons_div_nat, nat_richness) , ]

surveys <- surveys[!SURVEY_ID == 1840]

surveys[year>2011 & yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN, year)][N==2]

ewm_BACI_inyr <- surveys[paste(DOW,SUBBASIN,year) %in% 
                           surveys[year>2011 & yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN, year)][N>1, paste(DOW,SUBBASIN,year) ],
                         .(DOW, SUBBASIN, year, DATESURVEYSTART, Myriophyllum_spicatum, n_points_vegetated, SURVEY_ID, simpsons_div_nat, nat_richness) ]

ewm_mgmt_BACI_inyr <- s2_mgmt_wdates[species == "ewm_targeted" & !is.na(date1), .(dow ,lakename , year , date1)  , ]

ewm_mgmt_BACI_inyr[ , date := as.IDate(date1) , ]


# BACI ewm in yr fix lakenames in mgmt data ----------------------------------------------

#merge mgmt data on dows & years
ewm_mgmt_BACI_inyr[ , p_dow := round(dow/100, 0) ,]

#minnetonka
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

ewm_mgmt_BACI_inyr[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
ewm_mgmt_BACI_inyr[, sort(unique(lakename)),]


ewm_mgmt_BACI_inyr[lakename == "koronis_(main_lake)", subbasin := "main lake"]
ewm_mgmt_BACI_inyr[lakename == "koronis_(main_lake)", lakename := "koronis"]

ewm_mgmt_BACI_inyr[lakename == "island(southbay)", subbasin := "south basin"]
ewm_mgmt_BACI_inyr[lakename == "island(southbay)", lakename := "island"]

ewm_mgmt_BACI_inyr[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
ewm_mgmt_BACI_inyr[lakename == "southeast_anderson", lakename := "anderson"]


ewm_mgmt_BACI_inyr[lakename == "zumbra", subbasin := "Zumbra"]
ewm_mgmt_BACI_inyr[lakename == "zumbra", lakename := "Zumbra-Sunny"]
ewm_mgmt_BACI_inyr[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]


# BACI ewm in yr merge ewm mgmt and surveys ----------------------------------------------

ewm_BACI_inyr[ , p_dow := round(DOW/100, 0) , ]

setkey(ewm_mgmt_BACI_inyr, p_dow, subbasin, year )
setkey(ewm_BACI_inyr, p_dow, SUBBASIN, year)

#limit to only lakes with EWM
surveys[ , max(Myriophyllum_spicatum) , .(DOW) ][V1>0, DOW]
ewm_BACI_inyr <- ewm_BACI_inyr[ DOW %in% surveys[ , max(Myriophyllum_spicatum) , .(DOW) ][V1>0, DOW]  ]

ewm_BACI_inyr[ewm_mgmt_BACI_inyr, on = .(SUBBASIN = subbasin, p_dow, year) ,trt_date := date ]

setorder(ewm_BACI_inyr, DATESURVEYSTART)

ewm_BACI_inyr[ , d_post_trt := (DATESURVEYSTART-trt_date) , ]
ewm_BACI_inyr[ , ewm_vfoc := Myriophyllum_spicatum/n_points_vegetated  , ]
ewm_BACI_inyr[ , prev_abund := shift(ewm_vfoc) , .(DOW, SUBBASIN, year) ]
ewm_BACI_inyr[ , prev_survey_date := shift(DATESURVEYSTART) , .(DOW, SUBBASIN, year) ]
ewm_BACI_inyr[ , dN := ewm_vfoc-prev_abund ,]
ewm_BACI_inyr[ , ddays := DATESURVEYSTART-prev_survey_date ,]

ewm_BACI_inyr[trt_date<DATESURVEYSTART & trt_date>prev_survey_date , good_BACI := T]
ewm_BACI_inyr[!trt_date<DATESURVEYSTART | !trt_date>prev_survey_date , good_BACI := F]
ewm_BACI_inyr[is.na(trt_date) , good_BACI := NA]

ewm_BACI_inyr[ , nat_evenness := simpsons_div_nat/nat_richness ,]
ewm_BACI_inyr[ , prev_even := shift(nat_evenness) , .(DOW, SUBBASIN, year) ]
ewm_BACI_inyr[ , prev_rich := shift(nat_richness) , .(DOW, SUBBASIN, year) ]
ewm_BACI_inyr[ , prev_div := shift(simpsons_div_nat) , .(DOW, SUBBASIN, year) ]

ewm_BACI_inyr[ , ddiv := simpsons_div_nat-prev_div ,]
ewm_BACI_inyr[ , drich := nat_richness-prev_rich ,]
ewm_BACI_inyr[ , deven := nat_evenness-prev_even ,]

ewm_BACI_inyr[!is.na(dN) , .N , good_BACI]


# BACI ewm in yr viz & test ------------------------------------------



ggplot(ewm_BACI_inyr, aes(!is.na(trt_date), dN))+
  geom_boxplot()+
  geom_point()+
  xlab("managed")
ggplot(ewm_BACI_inyr, aes(dN, color = is.na(trt_date)))+
  geom_density()+
  scale_color_discrete(labels = c("managed", "unmanaged"))
ggplot(ewm_BACI_inyr, aes(good_BACI, dN))+
  geom_boxplot()+
  geom_point()

summary(lm(data = ewm_BACI_inyr, dN ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr[good_BACI == T | is.na(good_BACI)], dN ~ is.na(trt_date) ))


# BACI ewm in yr natives --------------------------------------------------


ggplot(ewm_BACI_inyr, aes(!is.na(trt_date), ddiv))+
  geom_boxplot()+
  geom_point()+
  xlab("managed")
ggplot(ewm_BACI_inyr, aes(ddiv, color = is.na(trt_date)))+
  geom_density()+
  scale_color_discrete(labels = c("managed", "unmanaged"))
ggplot(ewm_BACI_inyr, aes(good_BACI, ddiv))+
  geom_boxplot()+
  geom_point()
summary(lm(data = ewm_BACI_inyr, ddiv ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr[good_BACI == T | is.na(good_BACI)], ddiv ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr, deven ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr[good_BACI == T | is.na(good_BACI)], deven ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr, drich ~ is.na(trt_date) ))
summary(lm(data = ewm_BACI_inyr[good_BACI == T | is.na(good_BACI)], drich ~ is.na(trt_date) ))





# BACI ewm in yr underlying data ------------------------------------------

# here's the data driving this model:
surveys[SURVEY_ID %in% ewm_BACI_inyr[!is.na(dN) , SURVEY_ID , ] , .(SURVEY_ID, DOW, LAKE_NAME, SUBBASIN, p_dow, year, DATASOURCE)  , ][ewm_BACI_inyr[!is.na(dN) , .(SURVEY_ID,trt_date, dN, ddays,drich, deven, ddiv) , ], on = .(SURVEY_ID)][order(trt_date, DOW,SUBBASIN, year)]
# years per lake:
surveys[SURVEY_ID %in% ewm_BACI_inyr[!is.na(dN) , SURVEY_ID , ] , .(SURVEY_ID, DOW, LAKE_NAME, SUBBASIN, p_dow, year, DATASOURCE)  , ][ewm_BACI_inyr[!is.na(dN) , .(SURVEY_ID,trt_date, dN, ddays) , ], on = .(SURVEY_ID)][order(trt_date, DOW,SUBBASIN, year)][ , .N , .(LAKE_NAME,SUBBASIN,DOW)]

# BACI ewm in yr discussion --------------------------------------------------------------

#' ### EWM within year effects
#' 
#' Much like clp, you don't have many cases where good data exist for a BACI
#' analysis of within year management effects. In total, we've got 79 cases of 
#' "controls" where EWM was measured twice and unmanaged, 17 cases of measured 
#' twice and treated in that year, and only 9 cases of clean BACI data. 
#' 
#' 



# BACI clp cross yr data prep ---------------------------------------------------


surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN)][N>1]

clp_BACI_yrs <- surveys[paste(DOW,SUBBASIN) %in% 
                          surveys[ yday(DATESURVEYSTART)<181 , .N , .(DOW, SUBBASIN)][N>1, paste(DOW,SUBBASIN)],
                        ,] 
clp_BACI_yrs[ , clp_vfoc:= Potamogeton_crispus/n_points_vegetated ,]

setorder(clp_BACI_yrs, DATESURVEYSTART)

clp_mgmt_BACI_inyr[ , yr_plus_one := year+1 ,]


# BACI clp cross yr merge to clp surveys ----------------------------------------------------

clp_BACI_yrs[ , p_dow := round(DOW/100, 0) , ]

clp_BACI_yrs[clp_mgmt_BACI_inyr, on = .(p_dow, SUBBASIN = subbasin, year = yr_plus_one), trt_date_last_y := date]

#eliminate pre 2013 data, cant know trt in y-1
clp_BACI_yrs[ , .N , year]
clp_BACI_yrs <- clp_BACI_yrs[year>2012]

# This compares the pops of trt and untrt groups
ggplot(clp_BACI_yrs, aes(!is.na(trt_date_last_y), clp_vfoc))+
  geom_boxplot()+
  xlab("managed last y")

# we want delta_N/yr
#first ditch all but the lastest survey in any given year
#reverse order, making lastest surveys first in order
setorder(clp_BACI_yrs, -DATESURVEYSTART)

clp_BACI_yrs <- clp_BACI_yrs[!duplicated(clp_BACI_yrs[ , paste(DOW, SUBBASIN, year) , ]) ,  , ]

clp_BACI_yrs[ , .N , .(DOW, SUBBASIN, year) ][N>1]

setorder(clp_BACI_yrs, DATESURVEYSTART)

clp_BACI_yrs[ , last_t_vfoc := shift(clp_vfoc) , .(DOW, SUBBASIN) ]
clp_BACI_yrs[ , last_t_date := shift(DATESURVEYSTART) , .(DOW, SUBBASIN) ]
clp_BACI_yrs[ , last_t_year := shift(year) , .(DOW, SUBBASIN) ]

clp_BACI_yrs[ , dN := clp_vfoc-last_t_vfoc ,]
clp_BACI_yrs[ , dyears := year-last_t_year ,]


clp_BACI_yrs[ , hist(dN) , ]
clp_BACI_yrs[ , hist(dyears) , ]


# BACI clp cross yr viz and test ------------------------------------------

# delta T ==2
ggplot(clp_BACI_yrs[dyears==2], aes(!is.na(trt_date_last_y), dN))+
  geom_boxplot()+
  geom_point()+
  xlab("Managed last year")

summary(lm(data = clp_BACI_yrs[dyears==2], dN~!is.na(trt_date_last_y) ))

clp_BACI_yrs[dyears==2, .N, .(!is.na(trt_date_last_y))]

# BACI ewm cross yr data prep ---------------------------------------------------


surveys[ yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN)][N>1]

ewm_BACI_yrs <- surveys[paste(DOW,SUBBASIN) %in% 
                          surveys[ yday(DATESURVEYSTART)>151 , .N , .(DOW, SUBBASIN)][N>1, paste(DOW,SUBBASIN)],
                        ,] 

ewm_BACI_yrs[ , ewm_vfoc:= Myriophyllum_spicatum/n_points_vegetated ,]

setorder(ewm_BACI_yrs, DATESURVEYSTART)

ewm_mgmt_BACI_inyr[ , yr_plus_one := year+1 ,]


# BACI ewm cross yr merge to clp surveys ----------------------------------------------------

ewm_BACI_yrs[ , p_dow := round(DOW/100, 0) , ]

ewm_BACI_yrs[ewm_mgmt_BACI_inyr, on = .(p_dow, SUBBASIN = subbasin, year = yr_plus_one), trt_date_last_y := date]

#eliminate pre 2013 data, cant know trt in y-1
ewm_BACI_yrs[ , .N , year]
ewm_BACI_yrs <- ewm_BACI_yrs[year>2012]

# This compares the pops of trt and untrt groups
ggplot(ewm_BACI_yrs, aes(!is.na(trt_date_last_y), ewm_vfoc))+
  geom_boxplot()+
  xlab("managed last y")

# we want delta_N/yr
#first ditch all but the lastest survey in any given year
#reverse order, making lastest surveys first in order
setorder(ewm_BACI_yrs, -DATESURVEYSTART)

ewm_BACI_yrs <- ewm_BACI_yrs[!duplicated(ewm_BACI_yrs[ , paste(DOW, SUBBASIN, year) , ]) ,  , ]

ewm_BACI_yrs[ , .N , .(DOW, SUBBASIN, year) ][N>1]

setorder(ewm_BACI_yrs, DATESURVEYSTART)

ewm_BACI_yrs[ , last_t_vfoc := shift(ewm_vfoc) , .(DOW, SUBBASIN) ]
ewm_BACI_yrs[ , last_t_date := shift(DATESURVEYSTART) , .(DOW, SUBBASIN) ]
ewm_BACI_yrs[ , last_t_year := shift(year) , .(DOW, SUBBASIN) ]

ewm_BACI_yrs[ , dN := ewm_vfoc-last_t_vfoc ,]
ewm_BACI_yrs[ , dyears := year-last_t_year ,]


ewm_BACI_yrs[ , hist(dN) , ]
ewm_BACI_yrs[ , hist(dyears) , ]


# BACI ewm cross yr viz and test ------------------------------------------

# delta T ==2
ggplot(ewm_BACI_yrs[dyears==2], aes(!is.na(trt_date_last_y), dN))+
  geom_point()+
  xlab("Managed last year")

summary(lm(data = ewm_BACI_yrs[dyears==2], dN~!is.na(trt_date_last_y) ))

ewm_BACI_yrs[dyears==2, .N, .(!is.na(trt_date_last_y))]



# delta T any
ggplot(ewm_BACI_yrs[dyears == 2], aes(!is.na(trt_date_last_y), dN/dyears))+
  geom_boxplot()+
  geom_point()+
  xlab("Managed last year")

summary(lm(data = ewm_BACI_yrs, dN/dyears~!is.na(trt_date_last_y) ))

ewm_BACI_yrs[dyears==2, .N, .(!is.na(trt_date_last_y))]




















s2_trtmatrix_clp


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
#'   
#'   
#'   # add a CLP metric to summer surveys where CLP was measured during a reasonable time of year --------
#' 
#' surveys[ , DATESURVEYSTART := as.IDate(DATESURVEYSTART, format = "%m/%d/%Y" ) , ]
#' 
#' #put a CLP index into the summer surveys:
#' early_clpsurveys <- surveys[month(DATESURVEYSTART) %in% c(3,4,5,6) , .(SURVEY_ID, DOW, year = year(DATESURVEYSTART), LAKE_NAME, SUBBASIN, Potamogeton_crispus,DATESURVEYSTART, n_points_vegetated, tot_n_samp)  , ]
#' 
#' early_clpsurveys[ , potcri_early_vegdfoc := Potamogeton_crispus/n_points_vegetated ]
#' early_clpsurveys[is.na(potcri_early_vegdfoc), potcri_early_vegdfoc := 0] #here we can assume that the sampling was reasonable timing to find CLP had it been present
#' early_clpsurveys[ ,.N , potcri_early_vegdfoc>0]
#' 
#' surveys[ , year := year(DATESURVEYSTART) , ]
#' 
#' #thin summer surveys to exclude pre july
#' summer_surveys <- surveys[yday(DATESURVEYSTART) > 181  , , ]
#' 
#' #append early clp metrics to these summer surveys,:
#' 
#' summer_surveys[ , joindates := DATESURVEYSTART]
#' early_clpsurveys[ , joindateE := DATESURVEYSTART]
#' 
#' 
#' summer_surveys <- early_clpsurveys[summer_surveys, on = .(DOW, year, LAKE_NAME, SUBBASIN, joindateE < joindates), mult = "first" ]
#' 
#' names(summer_surveys)[names(summer_surveys) == "DATESURVEYSTART"] <- "SPRING_DATESURVEYSTART"
#' names(summer_surveys)[names(summer_surveys) == "SURVEY_ID"] <- "SPRING_SURVEY_ID"
#' names(summer_surveys)[names(summer_surveys) == "Potamogeton_crispus"] <- "SPRING_Potamogeton_crispus"
#' names(summer_surveys)[names(summer_surveys) == "n_points_vegetated"] <- "SPRING_n_points_vegetated"
#' names(summer_surveys)[names(summer_surveys) == "tot_n_samp"] <- "SPRING_tot_n_samp"
#' 
#' names(summer_surveys)[names(summer_surveys) == "i.DATESURVEYSTART"] <- "DATESURVEYSTART"
#' names(summer_surveys)[names(summer_surveys) == "i.SURVEY_ID"] <- "SURVEY_ID"
#' names(summer_surveys)[names(summer_surveys) == "i.Potamogeton_crispus"] <- "Potamogeton_crispus"
#' names(summer_surveys)[names(summer_surveys) == "i.n_points_vegetated"] <- "n_points_vegetated"
#' names(summer_surveys)[names(summer_surveys) == "i.tot_n_samp"] <- "tot_n_samp"
#' 
#' 
#' 
#' # Deprecated join method:
#' # summer_surveys <- merge(summer_surveys, early_clpsurveys, by = c("DOW", "year", "LAKE_NAME", "SUBBASIN"), suffixes = c(".summer", ".spring"), all.x = T)
#' 
#' summer_surveys[ , hist(potcri_early_vegdfoc)]
#' 
#' 
#' #verify that summer surveys have clp spring data (NA indicates no spring survey was conducted capable of evaluating CLP abundance):
#' 
#' summer_surveys[ , .N , SPRING_Potamogeton_crispus==0]
#' 
#' #summary stats
#' 
#' #' here an NA indicates a DOW that we don't have any control records for. later
#' #' we'll make an assumption that no mgmt record means not managed.  
#' summer_surveys[ , summary(clp_ntrtdates>0) ,] 
#' summer_surveys[potcri_early_vegdfoc >0 , .N  , ]
#' summer_surveys[potcri_early_vegdfoc > 0 , summary(clp_ntrtdates>0) ,]
#' summer_surveys[is.na(potcri_early_vegdfoc) & clp_ntrtdates>0 , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART )  ,]
#' summer_surveys[ , summary(ewm_ntrtdates>0) ,]
#' summer_surveys[Myriophyllum_spicatum > 0 , summary(ewm_ntrtdates>0) ,]
#' 
#' #' Here are some cases where our treatment records indicate that the lake was
#' #' managed for EWM, but we don't have any observed EWM in the survey...
#' #' Which seems very wrong. Same thing occurs in both EWM and CLP 
#' #' 
#' #' 
#' summer_surveys[Myriophyllum_spicatum == 0 & ewm_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,]
#' 
#' summer_surveys[DOW %in% 
#'                  summer_surveys[Myriophyllum_spicatum == 0 & ewm_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,][, DOW],
#'                .(DOW, LAKE_NAME,DATESURVEYSTART, ewm_pretrt, Myriophyllum_spicatum, n_points_vegetated , ewm_ntrtdates), .(SURVEY_ID) ][order(DOW, DATESURVEYSTART)]
#' 
#' 
#' summer_surveys[is.na(clp_ntrtdates), .N , potcri_early_vegdfoc>0 ]
#' 
#' summer_surveys[DOW %in% 
#'                  summer_surveys[potcri_early_vegdfoc == 0 & clp_ntrtdates>0 , .(year, LAKE_NAME, DOW, DATASOURCE, DATESURVEYSTART, SURVEY_ID)  ,][, DOW],
#'                .(DOW, LAKE_NAME, SUBBASIN, DATESURVEYSTART, clp_pretrt, potcri_early_vegdfoc, SPRING_DATESURVEYSTART, DATASOURCE , clp_ntrtdates), .(SURVEY_ID) ][order(DOW, DATESURVEYSTART)]
#'   







