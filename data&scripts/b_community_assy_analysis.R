#'---
#' title: "Community Assy - Observational Data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull in data for PI surveys (point and aggregated to survey
#' level and up to watershed level), it will grab management data and slice out 
#' surveys from managed lakes. 
#' 
#' Next we evaluate invaders, light, and species pools as drivers of native 
#' community Diversity, Richness, and Evenness. The goal of these analyses is to
#' understand how these factors influence community assembly in aquatic plants.
#' 
#'
#' # Document Preamble

# load libraries ------------------------------------------------------------------
#+ warning = FALSE
# # Load Libraries
library(data.table)
update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
library(ggplot2)
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
library(GGally)
library(VCA)
library(performance)
library(ggsignif)

# library(maps)
# library(rgdal)
# library(ggsn)
# library(moments)
# library(shiny)
# library(plotly)
# library(ggspatial)
# library(broom)
# library(woodson)



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

# this is an observation level dataset (our foundation plant data)
plants <- fread(file = "data&scripts/data/output/plants_env_data.csv")

#here we have aggregated the plants dataset into a typical format with a "wide" config, and a row for each point or rake throw 
plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)

#here we have thinned to only obs with point level abunds and aggregated to "wide" config with a row for each point or rake throw, species matrix contains relative abundances
plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")

# here we have aggregated plants dataset to the survey level, each row is a set of survey statistics (richness, diversity, etc.) and a lake level species matrix
surveys <- fread(file = "data&scripts/data/output/surveys_aqplants.csv")

# other companion data like watershed & lake geodata, MN plants checklist, watershed aggregation of plants database
load("data&scripts/data/output/synthesis_script_datasets.Rdata")

#managemnt data is cleaned in a companion script & leaves data loaded into ws
source(file = "data&scripts/b1_community_assy_analysis.R") # will load and clean management data and leave in WS a mgmtdata file



# data prep ---------------------------------------------------------------



#' # Data Prep: 
#' 
#' Because of the distinct phenology of CLP, we've got to use estimates of 
#' abundance from early season surveys (during the growth phase of CLP annual
#' life cycle). 
#' 
#' We can do this ONLY for the survey level work... That's b/c at the point 
#' level we don't have the ability to connect two sample locations across time.
#' 
#' So at the lake scale, we will use spring (Mar-June) surveys to come up with a
#' value for CLP abundance, then add that to subsequent summer surveys. 
#' 
#' At the point scale we need to use the point-level abundance data. That means
#' that we also need to model the date of observation because of the phenology
#' effect. 
#' 
#' 
    
# CLP metric &  summer surveys ------------------------------------------------

#' ## Spring CLP & Summer surveys:
#'
#' Here we calc a spring CLP value and carry it forward to the summer survey
#' (where native metrics come from). 
#' 

#put a CLP index into the peak surveys:
early_clpsurveys <- surveys[month(DATESURVEYSTART) %in% c(3,4,5,6) , .(SURVEY_ID, DOW, year = year(DATESURVEYSTART), LAKE_NAME, SUBBASIN, Potamogeton_crispus,DATESURVEYSTART, n_points_vegetated, tot_n_samp)  , ]

early_clpsurveys[ , potcri_early_vegdfoc := Potamogeton_crispus/n_points_vegetated ]
early_clpsurveys[is.na(potcri_early_vegdfoc), potcri_early_vegdfoc := 0]
early_clpsurveys[ ,.N , potcri_early_vegdfoc>0]

surveys[ , year := year(DATESURVEYSTART) , ]

#thin summer surveys to exclude pre june 15
summer_surveys <- surveys[yday(DATESURVEYSTART) > 151  , , ]

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

summer_surveys[ , hist(potcri_early_vegdfoc)]
  
summer_surveys[ , .N , SPRING_Potamogeton_crispus==0]

# bring in mgmt data ------------------------------------------------------

#' ## Management Data
#' 
#' check out the management data, and link it up to the surveys. Our goal is to 
#' be able to exclude managed systems from these analyses. 

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

#compress to lake-year
mgmtdata[ , .N , .(downum,year)][N>1]
mgmtdata[records >1]

simp_mgmt <- mgmtdata[ , .N , .(lakename, year, downum) , ]

simp_mgmt[mgmtdata[ewm_targeted==T], ewmtargeted := ewm_targeted, on = .(lakename, year, downum)]

simp_mgmt[mgmtdata[clp_targeted==T], clptargeted := clp_targeted, on = .(lakename, year, downum)]

summary(simp_mgmt)

fullmgmtdata <- mgmtdata

mgmtdata <- simp_mgmt

names(mgmtdata)[names(mgmtdata) == "downum"] <- "DOW"

summer_surveys[ , .N , Myriophyllum_spicatum>0]

summer_surveys[mgmtdata, clp_targeted := clptargeted  , on = .(DOW, year)]
summer_surveys[mgmtdata, ewm_targeted := ewmtargeted  , on = .(DOW, year)]


summer_surveys[ , .N , clp_targeted]
summer_surveys[ , .N , ewm_targeted]

#summary stats

summer_surveys[ , summary(clp_targeted) ,]
summer_surveys[potcri_early_vegdfoc > 0 , summary(clp_targeted) ,]
summer_surveys[is.na(potcri_early_vegdfoc) & clp_targeted == T , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART )  ,]
summer_surveys[ , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum > 0 , summary(ewm_targeted) ,]
summer_surveys[Myriophyllum_spicatum == 0 & ewm_targeted == T , .(year, LAKE_NAME, DATASOURCE, DATESURVEYSTART)  ,]

summer_surveys[is.na(clp_targeted), .N , potcri_early_vegdfoc>0 ]
  
  
  
# invader metrics ---------------------------------------------------------
#' ## Invader Abundance Metrics

# visualize "effects" at lake level
summer_surveys[, myrspi_summer_vegdfoc := Myriophyllum_spicatum/n_points_vegetated]
summer_surveys[is.na(myrspi_summer_vegdfoc) , myrspi_summer_vegdfoc := 0 , ]

summer_surveys[ , .N , n_points_vegetated>0 ]
summer_surveys[ , .N , myrspi_summer_vegdfoc>0]

summer_surveys[, potcri_summer_vegdfoc := Potamogeton_crispus/n_points_vegetated]
summer_surveys[is.na(potcri_summer_vegdfoc) , potcri_summer_vegdfoc := 0 , ]

# native metrics ----------------------------------------------------------
#' ## Native Response Metrics
#' 
#' We have measured Diversity, Richness and Evenness for the native plants. 
#' Lets have a look at the correllations among these responses. In addition, 
#' we'll grab some summary stats for a table in the paper.
#' 
#' I do think that it makes sense to have inv simpsons assigned as zero where it
#' goes to inf for richness 0. That because the effective number of species is 
#' zero when there are zero species (i.e., it seems logical).
#' 
#' I don't think that we want to be coercing the NAs in evenness to zero. It
#' just doesn't make sense. Evenness of a 0 species community is NA. 

#lake level
summer_surveys[ ,summary(simpsons_div_nat) ,]
summer_surveys[ ,summary(nat_richness) ,]
summer_surveys[ ,summary(simpsons_div_nat/nat_richness) ,]
# evenness produces NAs because of zero richness values -- 
summer_surveys[nat_richness == 0 , .N ,  ]
summer_surveys[ , nat_evenness := simpsons_div_nat/nat_richness, ]


#watershed level
names(watershed_occurrence_wide)
watershed_occurrence_wide[ , summary(n_species) ,] # this is total richness

# native richness
natcols <- names(watershed_occurrence_wide)[4:235][!names(watershed_occurrence_wide)[4:235] %in% c(rte[native_status == "I", mn_dnr_scientific_name],
                                                                            "Nitellopsis", "Typha glauca")]
watershed_occurrence_wide[ ,  nat_richness := rowSums(watershed_occurrence_wide[ , .SD, .SDcols = natcols] > 0), ]
watershed_occurrence_wide[ ,summary(nat_richness)]
watershed_occurrence_wide[simpson_div == Inf , simpson_div := 0 ,]
watershed_occurrence_wide[ , summary(simpson_div) ,]
watershed_occurrence_wide[ , nat_evenness := simpson_div/nat_richness, ]
watershed_occurrence_wide[ , summary(nat_evenness), ]
#add area to these
watershed_occurrence_wide[watersheds_huc8, on = .(watershed=major), acres := acres ]


#point level
plants_rakeabund_wide[ , summary(nat_richness) ,]
plants_rakeabund_wide[simpsons_div_nat == Inf , simpsons_div_nat := 0 ,]
plants_rakeabund_wide[ , summary(simpsons_div_nat) , ]
plants_rakeabund_wide[ , nat_evenness := simpsons_div_nat/nat_richness, ]
plants_rakeabund_wide[ , summary(nat_evenness), ]

#median area for each scale
watershed_occurrence_wide[ , summary(acres)]
summer_surveys[ , summary(acres.x) , ]

#sample sizes by scale
watershed_occurrence_wide[!is.na(watershed), .N , ]
summer_surveys[ , .N ,]
plants_rakeabund_wide[ , .N ,]

#correlations and viz.
# plot covariance and correllation coefs (r) from the niche axes

# evenness is zero only where the richness is zero... and evenness of zero might not be a meaningful thing, so lets drop those here 

ggpairs(summer_surveys[!is.na(nat_evenness),.(simpsons_div_nat, nat_richness, nat_evenness ) ,], lower = list(continuous = "smooth"), aes(fill = NULL))

ggpairs(watershed_occurrence_wide[!is.na(nat_evenness) ,.(simpson_div, nat_richness, nat_evenness ) ,], lower = list(continuous = "smooth"), aes(fill = NULL))

ggpairs(plants_rakeabund_wide[!is.na(nat_evenness),.(simpsons_div_nat, nat_richness, nat_evenness ) ,], lower = list(continuous = "smooth"), aes(fill = NULL))


#and we'll use a VCA to examine where diversity is coming from in out data

# names(plants_rakeabund_wide)
# 
# varPlot(form = simpsons_div_nat~DOW/watershed, 
#        Data=plants_rakeabund_wide)    
# 
# fitVCA(form=proplight~lake/site/plot + year, 
#        Data=community[!quadrat == "m" ])




# data analysis -----------------------------------------------------------

#' # Data Analysis
#' 
#' Now we will have a look at how invaders, light, and species pools are related
#' to the Diversity Richness & Evenness (DRE) of native plant communities.

# viz lake level invader effects ----------------------------------------------
#' ## Competitive Interactions
#' 
#' We're going to take a look at the lake and point scale influence of invaders
#' on native species. Some things to keep an eye out for in these analyses
#' 1. We're looking at only invaders as the independent variable
#' 2. We have 3 metrics for native responses (DRE)
#' 3. We're interested in how the two species of invader's differ in these
#' relationships to natives.
#' 4. We'll be doing a two-step style analysis, first asking how the presence of
#' the invader influences natives, then asking how increasing invader abundance 
#' is related to natives
#' 
#' 
#' Before we do our analyses, lets visualize the relationships that are of
#' interest to us. 
#' 
#' ### Lake Level - Viz

# presence absence lake scale boxplots ------------------------------------
# grab out just the rows where each invader could be estimated & make a plot table
clpboxplot <- summer_surveys[!is.na(potcri_early_vegdfoc), "inv.pres" := potcri_early_vegdfoc > 0 ] # here we exclude summer surveys with no early clp survey
clpboxplot[ , species := "CLP" ,]
clpboxplot <- clpboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]

ewmboxplot <- summer_surveys[!is.na(myrspi_summer_vegdfoc),"inv.pres" := myrspi_summer_vegdfoc > 0 ]
ewmboxplot[ , species := "EWM" ,]
ewmboxplot <- ewmboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]

boxplot_data <- rbind(ewmboxplot,clpboxplot)
rm(clpboxplot, ewmboxplot)

boxplot_data[ , evenness := simpsons_div_nat/nat_richness]
# boxplot_data[is.na(evenness), evenness := 0] 

boxplot_data <- melt(boxplot_data, id.vars = c("inv.pres","species"), variable.name = "metric", value.name = "value")
boxplot_data[ , metric:= factor(metric, levels = c("simpsons_div_nat", "nat_richness", "evenness")),]

box1 <- ggplot(data = boxplot_data[metric == "simpsons_div_nat"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("CLP", "EWM"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  scale_fill_discrete(limits= c("FALSE", "TRUE"),labels=c("Invader Absent","Invader Present"))+
  xlab(NULL)+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  theme_bw()+
  theme(axis.text.x = element_text( face = "italic"), legend.position = c(.35,.9), legend.title = element_blank(), legend.background = element_blank())+
  ggtitle("All Lake Surveys")

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

# abundance lake scale regressions ----------------------------------------


legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
INV_ENSpie <- ggplot()+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0  ],
             aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
             alpha = 0.1)+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0],
             aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
             alpha = 0.1)+
  geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 ],
              aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
              method = "loess", lty = 2, se = F)+
  geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0  ],
              aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
              method = "loess", lty = 2, se = F)+
  xlab("Invader Lakewide Prevalence")+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  ggtitle("INVADED LAKE SURVEYS")+
  labs(color = NULL) +
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"))



INV_richness <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 ],
             aes(potcri_early_vegdfoc, nat_richness),
             alpha = 0.1, color = "red")+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0  ],
             aes(myrspi_summer_vegdfoc, nat_richness),
             alpha = 0.1, color = "blue")+
  geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 ],
              aes(potcri_early_vegdfoc, nat_richness),
              method = "loess", color = "red", lty = 2, se = F)+
  geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0  ],
              aes(myrspi_summer_vegdfoc, nat_richness),
              method = "loess", color = "blue", lty = 2, se = F)+
  xlab(NULL)+
  ylab("Richness")+
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y.left = element_text(angle = 90))

INV_evenness <- ggplot()+
  geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
             aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
             alpha = 0.1, color = "red")+
  geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
             aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
             alpha = 0.1, color = "blue")+
  geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
              aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
              method = "loess", color = "red", lty = 2, se =F)+
  geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
              aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
              method = "loess", color = "blue", lty = 2, se =F)+
  xlab("Invader Lakewide Prevalence")+
  ylab("Evenness")+
  theme_bw()+
  theme(axis.text.y.left = element_text(angle = 90))

# ggarrange(INV_ENSpie, ggarrange(INV_richness, INV_evenness, ncol = 1))


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

# tests lake level invader effects ----------------------------------------

#' From these plots we can see that the use of abundances of the natives (i.e
#' going from richness to evenness) doesn't seem to change too much. However, we
#' do see an important change when we use abundance of invader as the predictor:
#' the invader - richness relationship seems to reverse!
#' 
#' The other thing we can see here is that both CLP & EWM seem to have really 
#' similar relationships to native species (not visually much interesting going
#' on there).
#' 
#' ### Lake Level - Tests
#' 
#' Here we'll make three tests (DRE) for each species (EWM/CLP) and data type
#' (p/a v. abund). 
#' 
summer_surveys[ ,summary(dowlknum)] 
summer_surveys[ , DOW := as.factor(DOW) ,]

#Diversity
D_pa_ewm_lake <- lmer(simpsons_div_nat ~ (myrspi_summer_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys)
summary(D_pa_ewm_lake)

D_pa_clp_lake <- lmer(simpsons_div_nat ~ (potcri_early_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys)
summary(D_pa_clp_lake)

#Richness
R_pa_ewm_lake <- glmer(nat_richness ~ (myrspi_summer_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys, family = poisson())
summary(R_pa_ewm_lake)

R_pa_clp_lake <- glmer(nat_richness ~ (potcri_early_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys, family = poisson())
summary(R_pa_clp_lake)

#Evenness
E_pa_ewm_lake <- glmer(nat_evenness ~ (myrspi_summer_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys, family = binomial())
summary(E_pa_ewm_lake)

E_pa_clp_lake <- glmer(nat_evenness ~ (potcri_early_vegdfoc>0) + (1|DOW) + (1|year),   data = summer_surveys, family = binomial())
summary(E_pa_clp_lake)

# ABUNDANCE 
#diversity
EWMabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0] )
summary(EWMabund_nat_lake_ENSpie)
plot(EWMabund_nat_lake_ENSpie)

CLPabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)])
summary(CLPabund_nat_lake_ENSpie)
plot(CLPabund_nat_lake_ENSpie)

#Richness
EWMabund_nat_lake_richness <- glmer(nat_richness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 ], family = poisson())
summary(EWMabund_nat_lake_richness)
plot(EWMabund_nat_lake_richness)

CLPabund_nat_lake_richness <- glmer(nat_richness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 ] , family = poisson())
summary(CLPabund_nat_lake_richness)
plot(CLPabund_nat_lake_richness)

# Evenness
#EWM Abund
EWMabund_nat_lake_evenness<- glmer(nat_evenness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0], family = binomial())
summary(EWMabund_nat_lake_evenness)
plot(EWMabund_nat_lake_evenness)

#CLP Abund
CLPabund_nat_lake_evenness <- glmer(nat_evenness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 ], family = binomial())
summary(CLPabund_nat_lake_evenness)
plot(CLPabund_nat_lake_evenness)


# Compile Model Tables ----------------------------------------------------

summary(D_pa_clp_lake)$coef
summary(R_pa_clp_lake)$coef
summary(E_pa_clp_lake)$coef

inv_mods <- rbindlist(
  list(data.table(summary(D_pa_ewm_lake)$coef, keep.rownames = T),
       data.table(summary(R_pa_ewm_lake)$coef, keep.rownames = T),
       data.table(summary(E_pa_ewm_lake)$coef, keep.rownames = T), 
       data.table(summary(D_pa_clp_lake)$coef, keep.rownames = T),
       data.table(summary(R_pa_clp_lake)$coef, keep.rownames = T),
       data.table(summary(E_pa_clp_lake)$coef, keep.rownames = T),
       data.table(summary(EWMabund_nat_lake_ENSpie)$coef, keep.rownames = T),
       data.table(summary(EWMabund_nat_lake_richness)$coef, keep.rownames = T),
       data.table(summary(EWMabund_nat_lake_evenness)$coef, keep.rownames = T),
       data.table(summary(CLPabund_nat_lake_ENSpie)$coef, keep.rownames = T),
       data.table(summary(CLPabund_nat_lake_richness)$coef, keep.rownames = T),
       data.table(summary(CLPabund_nat_lake_evenness)$coef, keep.rownames = T)
  ), fill = TRUE
)

inv_mods[ , species := rep(c(rep("Mspi", 6),rep("Pcri", 6)), 2) ]
inv_mods[ , response := rep(c(rep("Div", 2),rep("Rich", 2), rep("Even", 2)), 4) ]

inv_mods[response == "Rich", Estimate_backtrans := exp(Estimate)]
inv_mods[response == "Even", Estimate_backtrans := plogis(Estimate)]

inv_mods[response == "Rich", confint_lwr := exp(Estimate - 1.96*`Std. Error`)]
inv_mods[response == "Rich", confint_upr := exp(Estimate + 1.96*`Std. Error`)]

inv_mods[response == "Even", confint_lwr := plogis(Estimate - 1.96*`Std. Error`)]
inv_mods[response == "Even", confint_upr := plogis(Estimate + 1.96*`Std. Error`)]

inv_mods[response == "Div", confint_lwr := Estimate - 1.96*`Std. Error`]
inv_mods[response == "Div", confint_upr := Estimate + 1.96*`Std. Error`]

# fwrite(inv_mods, file = "data&scripts/data/output/obs_comp_mods.csv")

#' One crummy thing about using mixed effects models is that they tell me it's 
#' challenging to make model predictions of confidence intervals that do a good
#' job of incorporating the fixed and random effects variances (or something 
#' like that).
#' Since thats the case, we're just going to skip that, and instead we'll just 
#' plot the estimates, and denote which have significant effects. The code to
#' model confidence intervals is given in the first prediction, both via bootMer
#' and mertools:predictInterval. 
#' 
#' ### Lake Level - Results Viz


# visualizations with test results ---------------------------------------------------------

#new data spanning abund to predict models from
# EWM
newdat1<-data.frame(myrspi_summer_vegdfoc = seq(min(summer_surveys[myrspi_summer_vegdfoc > 0 , myrspi_summer_vegdfoc]), max(summer_surveys[myrspi_summer_vegdfoc > 0 , myrspi_summer_vegdfoc]), length.out = nrow(summer_surveys[myrspi_summer_vegdfoc > 0 ])),
                    year = summer_surveys[myrspi_summer_vegdfoc > 0 , year],
                    DOW = summer_surveys[myrspi_summer_vegdfoc > 0 , DOW])
# CLP
newdat2<-data.frame(potcri_early_vegdfoc = seq(min(summer_surveys[potcri_early_vegdfoc > 0 , potcri_early_vegdfoc]), max(summer_surveys[potcri_early_vegdfoc > 0 , potcri_early_vegdfoc]), length.out = nrow(summer_surveys[potcri_early_vegdfoc > 0 ])),
                    year = summer_surveys[potcri_early_vegdfoc > 0 , year],
                    DOW = summer_surveys[potcri_early_vegdfoc > 0 , DOW])

# ENSPie ~ Abund: 
#EWM
newdat1$fittedD <- predict(EWMabund_nat_lake_ENSpie, newdat1, re.form = NA)
  
  # #mertools() conf ints, from an estimation process I dont understand :)
  # preds <- predictInterval(EWMabund_nat_lake_ENSpie, newdat1, which = "fixed", n.sims = 9999, include.resid.var = F)
  # newdat1 <- cbind(newdat1,preds)
  # 
  # #fitted confints from bootstrap -- this is the "goldest" standard (aparrently these are all challenging appproximations)
  # boot_pred1 <- bootMer(EWMabund_nat_lake_ENSpie, predict, nsim = 1000, re.form = NA)
  # str(boot_pred1
  # newdat1$lci <- apply(boot_pred1$t, 2, quantile, 0.025)
  # newdat1$uci <- apply(boot_pred1$t, 2, quantile, 0.975)

#CLP
newdat2$fittedD <- predict(CLPabund_nat_lake_ENSpie, newdat2, re.form = NA)
  
##Richness~Abund: 
#EWM
newdat1$fittedR <- exp(predict(EWMabund_nat_lake_richness, newdat1, re.form = NA))
#CLP
newdat2$fittedR <- exp(predict(CLPabund_nat_lake_richness, newdat2, re.form = NA))

#Evenness~Abund: 
#EWM
newdat1$fittedE <- plogis(predict(EWMabund_nat_lake_evenness, newdat1, re.form = NA))
#CLP
newdat2$fittedE <- plogis(predict(CLPabund_nat_lake_evenness, newdat2, re.form = NA))
  
#Diversity Plot
legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
INV_ENSpie <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 ],
               aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
               alpha = 0.2)+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
               alpha = 0.2)+
    geom_line(data = newdat1, 
              aes(myrspi_summer_vegdfoc,fittedD), color = "red", size = 1.5)+
    # geom_ribbon(data = newdat1, aes(x = myrspi_summer_vegdfoc, ymin=lci, ymax=uci), color = NA , fill = "red", alpha = .15)+
    geom_line(data = newdat2,
              aes(potcri_early_vegdfoc,fittedD), color = "blue", size = 1.5, lty = 2)+
    # geom_ribbon(data = newdat2, aes(x = potcri_early_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "blue", alpha = .15)+
    xlab("Invader lakewide prevalence")+
    ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
    ggtitle("Invaded Lake Surveys")+
    labs(color = NULL) +
    scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
    theme_bw()+
    theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))
  
#Richness plot:
INV_richness <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 ],
               aes(potcri_early_vegdfoc, nat_richness),
               alpha = 0.2, color = "red")+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0  ],
               aes(myrspi_summer_vegdfoc, nat_richness),
               alpha = 0.2, color = "blue")+
    geom_line(data = newdat1, 
              aes(myrspi_summer_vegdfoc,fittedR), color = "red", size = 1.5)+
    geom_line(data = newdat2, 
              aes(potcri_early_vegdfoc,fittedR), color = "blue", size = 1.5)+
    xlab(NULL)+
    ylab("Richness")+
    ggtitle("")+
    theme_bw()+
    theme(axis.text.y.left = element_text(angle = 90))
  
INV_evenness <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
               alpha = 0.2, color = "red")+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
               alpha = 0.2, color = "blue")+
    geom_line(data = newdat1, 
              aes(myrspi_summer_vegdfoc,fittedE), color = "red", size = 1.5, lty = 2)+
    geom_line(data = newdat2, 
              aes(potcri_early_vegdfoc,fittedE), color = "blue", size = 1.5)+
    xlab("Invader lakewide prevalence")+
    ylab("Evenness")+
    theme_bw()+
    theme(axis.text.y.left = element_text(angle = 90))

box1 <- box1+
  geom_signif(
    y_position = c(-0.5, -0.5), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("0.144", "0.063"), tip_length = 0, size = 1
  )

box2 <- box2+
  geom_signif(
    y_position = c(-2, -2), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("0.997", "<0.001"), tip_length = 0, size = 1
  )

box3 <- box3+
  geom_signif(
    y_position = c(0.05, 0.05), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("0.313", "<0.001"), tip_length = 0, size = 1
  )
  
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

#' ### Lake Level - Discuss: 
#' 
#' It's surprising to me that these trends are significant, but I suppose that 
#' is owed partly to the large sample sizes we've got. In addition, it should be
#' noted that not all of our model diagnostics were very satisfactory (check out
#' the binomial glmer for evenness. 
#' 
#' ### Point Level - Viz


# viz point level rake abund --------------------------------------------------

#ENSpie
names(plants_rakeabund_wide) <- names(clean_names(plants_rakeabund_wide))

point_abund_ENSpie <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat, group = survey_id),
              method = "lm", geom = 'line', se =F, color = "blue", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat),
              method = "lm", color = "blue", size = 1.5)+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat),
              method = "lm", color = "red", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  ggtitle("INVADED SAMPLES")+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))


#Richness
point_abund_richness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_richness),
              method = "lm", color = "blue", size = 1.5)+
  ylab("Richness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_richness),
              method = "lm", color = "red", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_y_log10()+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  ggtitle("")+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))


#evenness?
point_abund_evenness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat/nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat/nat_richness),
              method = "lm", color = "blue", size = 1.5)+
  ylab("Evenness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat/nat_richness, group = survey_id),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+
  geom_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat/nat_richness),
              method = "lm", color = "red", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_y_log10()+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))

point_abunds <- ggarrange(point_abund_ENSpie, ggarrange(point_abund_richness, point_abund_evenness,
                                                        ncol = 1,
                                                        labels = c("e","f"),
                                                        label.x = c(0.15,0.15),
                                                        label.y = c(0.86,0.95)),
                          labels = c("d",""),
                          label.x = 0.15,
                          label.y = 0.91)+  border(color = "black", size = 0.8, linetype = NULL)

# viz point level p/a (only rake data) ----------------------------------

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


box1p <- ggplot(data = point_boxes_dat[metric == "nat_div"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  scale_fill_discrete(limits= c("FALSE", "TRUE"),labels=c("Invader Absent","Invader Present"))+
  xlab(NULL)+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  theme_bw()+
  ggtitle("All sample points")+
  theme(axis.text.x = element_text( face = "italic"), legend.position = c(.35,.9), legend.title = element_blank(), legend.background = element_blank())

box2p <- ggplot(data = point_boxes_dat[metric == "nat_richness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Richness")+
  theme_bw()+
  theme(axis.text.x = element_blank(), legend.position = "none")+
  ggtitle("")+
  theme(axis.text.y.left = element_text(angle = 90))

box3p <- ggplot(data = point_boxes_dat[metric == "nat_evenness"], aes(species, value, fill = inv.pres))+
  geom_boxplot()+
  scale_x_discrete(limits= c("inv_pot_cri", "inv_myr_spi"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
  xlab(NULL)+
  ylab("Evenness")+
  theme_bw()+
  theme(axis.text.x = element_text( face = "italic"), legend.position = "none")+
  theme(axis.text.y.left = element_text(angle = 90))

box1bp <- ggarrange(box2p, box3p, ncol = 1,
          labels = c("b","c"),
          label.x = c(0.15,0.15),
          label.y = c(0.85,0.93))

point_boxpsp <- ggarrange(box1p, box1bp,
                         labels = c("a", ""),
                         label.x = 0.15,
                         label.y = 0.92)

# Figure 2: Compile boxplots and abund plots

ggarrange(point_boxpsp,point_abunds, ncol = 1)


#' ### Point Level - Test
#' 
#' This is an important spot to pause and talk a bit about the nuance of these
#' figures (and the tests we'll write to evaluate the relationships). At the 
#' point level, we don't have any way to connect sample locations across time
#' at least not very many of them. As such, we need to use each survey and
#' consider in our model the influence of the phenology component of the systems
#' members. We do that by modeling a polynomial date effect. 

#  test point level rake abund --------------------------------------------

#Div:
D_ab_clp_point <- lmer(simpsons_div_nat~ poly(yday(survey_date), 2)+  potamogeton_crispus+ (1|dow), data = plants_rakeabund_wide[potamogeton_crispus> 0 , ])
summary(D_ab_clp_point)

D_ab_ewm_point <- lmer(simpsons_div_nat~poly(yday(survey_date), 2)+ myriophyllum_spicatum + (1 | dow ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0] )
summary(D_ab_ewm_point)


#Rich:
R_ab_clp_point <- glmer(nat_richness~ poly(yday(survey_date), 2) + potamogeton_crispus + (1| dow ), data = plants_rakeabund_wide[potamogeton_crispus> 0], family = poisson())
summary(R_ab_clp_point)
plot(R_ab_clp_point)

R_ab_ewm_point <- glmer(nat_richness~ poly(yday(survey_date), 2) + myriophyllum_spicatum+ (1| dow ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0], family = poisson())
summary(R_ab_ewm_point)

#evenness
E_ab_clp_point <- lmer(nat_evenness ~ poly(yday(survey_date), 2) + potamogeton_crispus + (1 | dow ), data = plants_rakeabund_wide[potamogeton_crispus> 0])
summary(E_ab_clp_point)
plot(E_ab_clp_point)

E_ab_ewm_point <- lmer(nat_evenness~ poly(yday(survey_date), 2) + myriophyllum_spicatum + (1|dow ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0])

summary(E_ab_ewm_point)
plot(E_ab_ewm_point)

plants_rakeabund_wide[ , hist(exp(nat_evenness)) , ]

# point level presence/abs ------------------------------------------------
#Div:
D_pa_clp_point <- lmer(simpsons_div_nat~ poly(yday(survey_date), 2)+  (potamogeton_crispus>0)+ (1|dow), data = plants_rakeabund_wide)
summary(D_pa_clp_point)

D_pa_ewm_point <- lmer(simpsons_div_nat~poly(yday(survey_date), 2)+ (myriophyllum_spicatum>0) + (1 | dow ), data = plants_rakeabund_wide )
summary(D_pa_ewm_point)


#Rich:
R_pa_clp_point <- glmer(nat_richness~ poly(yday(survey_date), 2) + (potamogeton_crispus>0) + (1 | dow ), data = plants_rakeabund_wide, family = poisson(link = "log"))
summary(R_pa_clp_point)
plot(R_pa_clp_point)

R_pa_ewm_point <- glmer(nat_richness~ poly(yday(survey_date), 2) + (myriophyllum_spicatum>0) + (1| dow ), data = plants_rakeabund_wide, family = poisson(link = "log"))
summary(R_pa_ewm_point)

#evenness
E_pa_clp_point <- lmer(nat_evenness ~ poly(yday(survey_date), 2) + (potamogeton_crispus>0) + (1 | dow ), data = plants_rakeabund_wide)
summary(E_pa_clp_point)
plot(E_pa_clp_point)

E_pa_ewm_point <- lmer(nat_evenness~ poly(yday(survey_date), 2) + (myriophyllum_spicatum>0) + (1 |dow ), data = plants_rakeabund_wide)
summary(E_pa_ewm_point)
plot(E_pa_ewm_point)



#' #### Interpret
#' 
#' What these results "look" like, is that there is a sig effect in each of the
#' native response metrics (almost, ewm evenness is marginal).
#' 
#' Some notworthy bits here. We've modeled evenness as a gaussian error process
#' because the modlels are failing somehow when we assess using binomial
#' distributions. 
#' 
#' Now export these model results:


# Compile Model Tables - Point --------------------------------------------

inv_mods_point <- rbindlist(
  list(data.table(summary(D_pa_ewm_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(R_pa_ewm_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(E_pa_ewm_point)$coef[c(1,4),], keep.rownames = T), 
       data.table(summary(D_pa_clp_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(R_pa_clp_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(E_pa_clp_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(D_ab_ewm_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(R_ab_ewm_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(E_ab_ewm_point)$coef[c(1,4),], keep.rownames = T), 
       data.table(summary(D_ab_clp_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(R_ab_clp_point)$coef[c(1,4),], keep.rownames = T),
       data.table(summary(E_ab_clp_point)$coef[c(1,4),], keep.rownames = T)
  ), fill = TRUE
)

inv_mods_point[ , species := rep(c(rep("Mspi", 6),rep("Pcri", 6)), 2) ]
inv_mods_point[ , response := rep(c(rep("Div", 2),rep("Rich", 2), rep("Even", 2)), 4) ]

inv_mods_point[response == "Rich", Estimate_backtrans := exp(Estimate)]
inv_mods_point[response == "Even", Estimate_backtrans := plogis(Estimate)]

inv_mods_point[response == "Rich", confint_lwr := exp(Estimate - 1.96*`Std. Error`)]
inv_mods_point[response == "Rich", confint_upr := exp(Estimate + 1.96*`Std. Error`)]

inv_mods_point[response == "Even", confint_lwr := plogis(Estimate - 1.96*`Std. Error`)]
inv_mods_point[response == "Even", confint_upr := plogis(Estimate + 1.96*`Std. Error`)]

inv_mods_point[response == "Div", confint_lwr := Estimate - 1.96*`Std. Error`]
inv_mods_point[response == "Div", confint_upr := Estimate + 1.96*`Std. Error`]

rbindlist(list(inv_mods,inv_mods_point))


# fwrite(rbindlist(list(inv_mods,inv_mods_point)), file = "data&scripts/data/output/obs_comp_mods.csv")



# visualizations with tests -----------------------------------------------

#' Predict from models:

#' ### Point Level - Results Viz
#' 
#' 

# predictions on abund ----------------------------------------------------


#new data spanning abund to predict models from
# EWM
newdat3<-data.frame(myriophyllum_spicatum = seq(1,3, length.out = nrow(plants_rakeabund_wide[myriophyllum_spicatum > 0 ])),
                    survey_date = plants_rakeabund_wide[myriophyllum_spicatum > 0 , mean(survey_date)],
                    dow = plants_rakeabund_wide[myriophyllum_spicatum > 0 , dow])
# CLP
newdat4<-data.frame(potamogeton_crispus = seq(1,3, length.out = nrow(plants_rakeabund_wide[potamogeton_crispus > 0 ])),
                    survey_date = plants_rakeabund_wide[potamogeton_crispus > 0 , mean(survey_date)],
                    dow = plants_rakeabund_wide[potamogeton_crispus > 0 , dow])

# ENSPie ~ Abund: 
#EWM
newdat3$fittedD <- predict(D_ab_ewm_point, newdat3, re.form = NA)

#CLP
newdat4$fittedD <- predict(D_ab_clp_point, newdat4, re.form = NA)

##Richness~Abund: 
#EWM
newdat3$fittedR <- predict(R_ab_ewm_point, newdat3, re.form = NA,type = "response" )
#CLP
newdat4$fittedR <- predict(R_ab_clp_point, newdat4, re.form = NA, type = "response")

#Evenness~Abund: 
#EWM
newdat3$fittedE <- predict(E_ab_ewm_point, newdat3, re.form = NA)
#CLP
newdat4$fittedE <- predict(E_ab_clp_point, newdat4, re.form = NA)



#ENSpie
names(plants_rakeabund_wide) <- names(clean_names(plants_rakeabund_wide))

legend_colors <- c("potamogeton_crispus" = "blue", "myriophyllum_spicatum" = "red")
point_abund_ENSpie <- ggplot()+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, simpsons_div_nat, color = "potamogeton_crispus", group = dow),
              method = "lm", geom = 'line', se =F,  alpha = .04)+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, simpsons_div_nat,color = "myriophyllum_spicatum", group = dow),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+
  geom_line(data = newdat4, 
            aes(potamogeton_crispus,fittedD), color = "blue", size = 1.5)+
  geom_line(data = newdat3, 
            aes(myriophyllum_spicatum,fittedD), color = "red", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  ggtitle("Invaded sample points")+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9),legend.title = element_blank(), legend.background = element_blank(), legend.text = element_text(face = "italic"))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


#Richness
point_abund_richness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_richness, group = dow),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .04)+
  ylab("Richness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_richness, group = dow),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+
  geom_line(data = newdat3, 
            aes(myriophyllum_spicatum,fittedR),
            color = "red", size = 1.5)+
  geom_line(data = newdat4, 
            aes(potamogeton_crispus,fittedR),
            color = "blue", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  ggtitle("")+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))+
  theme(axis.text.y.left = element_text(angle = 90))


#evenness?
point_abund_evenness <- ggplot()+
  # geom_boxplot(aes(group = potamogeton_crispus))+
  stat_smooth(data = plants_rakeabund_wide[potamogeton_crispus> 0],
              aes(potamogeton_crispus, nat_evenness, group = dow),
              method = "lm", geom = 'line', se = F, color = "blue", alpha = .04)+
  ylab("Evenness")+
  # theme(legend.position = "none")+
  stat_smooth(data = plants_rakeabund_wide[myriophyllum_spicatum> 0],
              aes(myriophyllum_spicatum, nat_evenness, group = dow),
              method = "lm", geom = 'line', se = F, color = "red", alpha = .04)+  
  geom_line(data = newdat3, aes(myriophyllum_spicatum,fittedE), color = "red", size = 1.5)+
  geom_line(data = newdat4, 
            aes(potamogeton_crispus,fittedE),
            color = "blue", size = 1.5)+
  theme_bw()+
  xlab("Relative Abundance of Invader")+
  scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))+
  theme(axis.text.y.left = element_text(angle = 90))+
  ylim(c(0.7, 1.05))

point_abunds <- ggarrange(point_abund_ENSpie, ggarrange(point_abund_richness, point_abund_evenness,
                                                        ncol = 1,
                                                        labels = c("e","f"),
                                                        label.x = c(0.15,0.15),
                                                        label.y = c(0.86,0.95)),
                          labels = c("d",""),
                          label.x = 0.15,
                          label.y = 0.91)+  border(color = "black", size = 0.8, linetype = NULL)

# signific. on boxplots ---------------------------------------------------

#' Add significance levels to boxplots

box1p <- box1p+
  geom_signif(
    y_position = c(-0.7, -0.7), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("<0.001", "<0.001"), tip_length = 0, size = 1
  )

box2p <- box2p+
  geom_signif(
    y_position = c(-2, -2), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("<0.001", "0.003"), tip_length = 0, size = 1
  )

box3p <- box3p+
  geom_signif(
    y_position = c(0.7, 0.7), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
    annotation = c("<0.001", "0.060"), tip_length = 0, size = 1
  )+ylim(c(0.7, 1.05))


box1bp <- ggarrange(box2p, box3p, ncol = 1,
                   labels = c("b","c"),
                   label.x = c(0.15,0.15),
                   label.y = c(0.85,0.93))

point_boxpsp <- ggarrange(box1p, box1bp,
                         labels = c("a", ""),
                         label.x = 0.15,
                         label.y = 0.92)

# Figure 2: Compile boxplots and abund plots

ggarrange(point_boxpsp,point_abunds, ncol = 1)

#' #' ### Point Level - Discuss
#' 
#' A few things I don't love: 
#' 1. I can't see the medians in the boxplots
#' 2. I can't tell whats happening in the evenness plots
#' 3. There's got to be a #3, I just can't think of one right now.
#' 
#' ### Methodological Conclusions: p/s vs. abund
#' 
#' Up to this point we have treated these (p/a and abund) as two models. A
#' glance at the plots shows you that p/a at the lake scale is problematic
#' because the effects of invader presence are generally opposite the effects of
#' invader abundance. 
#' 
#' From here forward we use only the present locs for the lake scale.
#' 
#' On the other hand, the point level observations tend to show same trends in
#' p/a and abund, so we could consider them as one.
#' 
#' From here forward, we use all the point data including zeros and abunds. 
#' 
#' Diversity as a metric allows us to incorporate both richness and evenness in
#' our native community metric, so let's use that moving forward, but we'll 
#' model & print all the metrics (DRE). 
#' 
# lake level light/ Secchi ------------------------------------------------
#' 
#' The next thing we want to investigate is how light, is driving some of the 
#' native community and invader relationship. 
#' 
#' ## Environmental Conditions (Light)
#' 
#' ### Lake Level - Viz


#whats the relationship to clarity look like in uninvaded lakes

summer_surveys[ , invaded := (potcri_early_vegdfoc > 0 | myrspi_summer_vegdfoc > 0)|
                  (is.na(potcri_early_vegdfoc) & myrspi_summer_vegdfoc > 0),]
summer_surveys[ is.na(potcri_early_vegdfoc) , invaded := myrspi_summer_vegdfoc > 0 ,]
#check work:
summer_surveys[ , .N , .(myrspi_summer_vegdfoc>0,potcri_early_vegdfoc>0, invaded ) ]

ggpairs(summer_surveys[invaded == T , .(Secchi_m, potcri_early_vegdfoc, myrspi_summer_vegdfoc, simpsons_div_nat, nat_richness,nat_evenness)], lower = list(continuous = "smooth"), aes(fill = NULL))

nat_ENSpie <- ggplot(summer_surveys[!is.na(Secchi_m) & Secchi_m >0.2 , ],
                     aes(Secchi_m,simpsons_div_nat, color = invaded))+
  geom_point( alpha = 0.2)+
  geom_smooth(method = "lm", se = F)+
  xlab("Secchi (m)")+
  ylab(bquote('Native diversity ('~ENS[PIE]~')'))+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())+
  scale_x_log10()

nat_rich <- ggplot(summer_surveys[!is.na(Secchi_m) , ],
                     aes(Secchi_m,nat_richness, color = invaded))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Secchi")+
  ylab("Richness")+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())+
  scale_x_log10()


nat_even <- ggplot(summer_surveys[!is.na(Secchi_m) , ],
                     aes(Secchi_m,nat_evenness, color = invaded))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Secchi (m)")+
  ylab("Evenness")+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())+
  scale_x_log10()


clp_secchi <- ggplot(summer_surveys[!is.na(potcri_early_vegdfoc), , ], aes(Secchi_m, potcri_early_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "loess")+
  ylab("CLP Abundance")+
  theme_bw()+
  scale_x_log10()

ewm_secchi <- ggplot(summer_surveys[myrspi_summer_vegdfoc>0], aes(Secchi_m, myrspi_summer_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth( method = "loess")+
  ylab("EWM Abundance")+
  theme_bw()+
  scale_x_log10()

ggarrange(nat_ENSpie, clp_secchi, ewm_secchi, ncol = 1)

#' 
#' ### Lake Level - Test
#' 
#' 
#now we want to ask if the relationship between the invaders abund and native species sticks when we consider WQ:

#CLP
summary(CLPabund_nat_lake_ENSpie)
summary(CLPabund_nat_lake_richness)
summary(CLPabund_nat_lake_evenness)

m.div.clp.wc <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)] )

m.ric.clp.wc <- glmer(nat_richness ~ potcri_early_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 ], family = poisson())

m.evn.clp.wc <- lmer(nat_evenness ~ potcri_early_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 ] )

summary(m.div.clp.wc) # here everything becomes non significant
summary(m.ric.clp.wc) # here clp still has a negative effect
summary(m.evn.clp.wc) # here everything becomes non significant

#plot_model(m.nat.clp.wc, type = "pred", terms = "potcri_early_vegdfoc")
# plot_model(m.nat.clp, type = "pred", terms = "potcri_early_vegdfoc")

#EWM
summary(EWMabund_nat_lake_ENSpie)
summary(EWMabund_nat_lake_richness)
summary(EWMabund_nat_lake_evenness)

m.div.ewm.wc <- lmer(simpsons_div_nat ~ myrspi_summer_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 ])

m.ric.ewm.wc <- glmer(nat_richness ~ myrspi_summer_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 ], family = poisson())

m.evn.ewm.wc <- lmer(nat_evenness ~ myrspi_summer_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 ])

summary(m.div.ewm.wc)# here everything becomes non significant
summary(m.ric.ewm.wc)# here everything becomes non significant
summary(m.evn.ewm.wc)# here everything becomes non significant

# plot_model(m.nat.ewm.wc, type = "pred", terms = "myrspi_summer_vegdfoc")
# plot_model(m.nat.ewm, type = "pred", terms = "myrspi_summer_vegdfoc")

#do the invaders exhibit negative relationships to secchi?
clp_secchi_test <- glmer(potcri_early_vegdfoc ~ log(Secchi_m) + (1|DOW) + (1| year), data = summer_surveys[!is.na(potcri_early_vegdfoc) ], family = binomial(), weights = SPRING_n_points_vegetated)
summary(clp_secchi_test)
plot(clp_secchi_test)

ewm_secchi_test <- glmer(myrspi_summer_vegdfoc ~ log(Secchi_m) + (1|DOW) + (1| year), data = summer_surveys[ myrspi_summer_vegdfoc>0], family = binomial(), weights = n_points_vegetated)
summary(ewm_secchi_test)
plot(ewm_secchi_test)

# we could also ask if the decline in the positive secchi effect is significant:
ggplot(summer_surveys[!is.na(Secchi_m) & Secchi_m >0.05 & simpsons_div_nat > 0 , ],
       aes(Secchi_m,simpsons_div_nat, color = invaded))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Secchi")+
  ylab("ENSpie")+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())+scale_x_log10()+scale_y_log10()

secchi_nat_inv <- lmer(log(simpsons_div_nat)~invaded*log(Secchi_m) + (1|DOW) + (1| year) , 
                       data = summer_surveys[!is.na(Secchi_m) & Secchi_m >0.05 & simpsons_div_nat > 0 , ])
summary(secchi_nat_inv)

#' ### Lake Level - Discuss
#' 
#' 
#' 
#' ### Point Level - Viz
#' 


# point level light avail -------------------------------------------------

point_nat_light <- ggplot(plants_rakeabund_wide[!is.na(proplight)], aes(proplight, simpsons_div_nat))+
  geom_point( alpha = 0.02, size = 2.5)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("ENSpie")


ggplot(plants_rakeabund_wide[!is.na(proplight)], aes(proplight, myriophyllum_spicatum))+
  geom_point( alpha = 0.005)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("EWM")

ggplot(plants_rakeabund_wide[!is.na(proplight)], aes(proplight, potamogeton_crispus))+
  geom_point( alpha = 0.005)+
  geom_smooth(method = "lm")+
  xlab("Proplight")+
  ylab("CLP")

#' We'll use a VCA to examine where light var is coming from in our data: 
#' This is a nice solution to Dan's question about how much in-lake variation in
#' depth can even contribute to the overall variance in proplight. 
#' 
#' Currently not sucessful in implememnting this because of memory allocation.

# 
# names(plants)
# 
# varPlot(form = proplight~watershed/dow/survey_id/point_id,
#        Data=plants_rakeabund_wide)
# 
# fitVCA(form=proplight~watershed/DOW/SURVEY_ID/POINT_ID,
#        Data=plants[!is.na(proplight) & !is.na(watershed)])
# 

#'
#' ### Point Level - Test
#'
#' Does negative point scale rel goes away if we account for
#' light & if that effect varies by species:


# test: point level invader eff + water clarity ---------------------------

D_ab_clp_point

D_clp_point <- lmer(simpsons_div_nat ~ potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(D_clp_point)

D_clp_point.wc <- lmer(simpsons_div_nat ~ proplight*potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(D_clp_point.wc)

R_clp_point <- glmer(nat_richness ~ potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide, family = poisson())
summary(R_clp_point)

R_clp_point.wc <- glmer(nat_richness ~ proplight*potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide, family = poisson())
summary(R_clp_point.wc)

E_clp_point <- lmer(nat_evenness ~ potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(E_clp_point)

E_clp_point.wc <- lmer(nat_evenness ~ proplight*potamogeton_crispus + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(E_clp_point.wc)

#EWM
D_ewm_point <- lmer(simpsons_div_nat ~ myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(D_ewm_point)

D_ewm_point.wc <- lmer(simpsons_div_nat ~ proplight*myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(D_ewm_point.wc)

R_ewm_point <- glmer(nat_richness ~ myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide, family = poisson())
summary(R_ewm_point)

R_ewm_point.wc <- glmer(nat_richness ~ proplight*myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide, family = poisson())
summary(R_ewm_point.wc)

E_ewm_point <- lmer(nat_evenness ~ myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(E_ewm_point)

E_ewm_point.wc <- lmer(nat_evenness ~ proplight*myriophyllum_spicatum + poly(yday(survey_date), 2) +  (1 | dow), data = plants_rakeabund_wide)
summary(E_ewm_point.wc)

# species pools -----------------------------------------------------------


#' ## Species Pools
#'
#' We want to use the pool above each scale as a predictor of the pool at that scale:
#' For obs data: We want lakediv ~ watersheddiv; then pointdiv~lakediv + watershed div
#' 
#' 
#' ### Both scales - Viz & Test 

ggplot( data = plants_rakeabund_wide, aes( surveyrichness, simpsons_div_nat))+
  geom_point()+
  geom_smooth(method = "lm")

# watershed richness as the predictor of a neighborhood: 
ggplot( data = plants_rakeabund_wide, aes( watershedrichness, simpsons_div_nat))+
  geom_point()+
  geom_smooth(method = "lm")

point_div_pools <- lm(simpsons_div_nat ~ surveyrichness + watershedrichness, data = plants_rakeabund_wide)
summary(point_div_pools)

point_ric_pools <- lm(nat_richness ~ surveyrichness + watershedrichness, data = plants_rakeabund_wide)
summary(point_ric_pools)

point_evn_pools <- lm(nat_evenness ~ surveyrichness + watershedrichness, data = plants_rakeabund_wide)
summary(point_evn_pools)

# watershed richness as the predictor of a lake: 
ggplot( data = summer_surveys,
        aes(watershedrichness, simpsons_div_nat))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Survey ENSpie")+
  xlab("HUC-8 Watershed Richness")+
  theme_bw()

survey_div_watershed_pool <- lm(simpsons_div_nat~watershedrichness, data = summer_surveys)
summary(survey_div_watershed_pool)
survey_rich_watershed_pool <- glm(nat_richness~watershedrichness, data = summer_surveys, family = poisson())
summary(survey_rich_watershed_pool)
survey_even_watershed_pool <- lm(nat_evenness~watershedrichness, data = summer_surveys)
summary(survey_even_watershed_pool)

#redo all species pool plots with ENSpie:
point_pools <- ggplot( data = plants_rakeabund_wide,
                       aes(surveyrichness, simpsons_div_nat))+
  geom_point(shape = 1)+
  # geom_smooth(method = "lm")+
  ylab(bquote('Point-scale native diversity ('~ENS[PIE]~')'))+
  xlab("Lake-scale richness")+
  theme_bw()

lake_pools <- ggplot( data = summer_surveys,
                      aes(watershedrichness, simpsons_div_nat))+
  geom_point(shape = 1)+
  # geom_smooth(method = "lm")+
  ylab(bquote('Lake-scale native diversity ('~ENS[PIE]~')'))+
  xlab("Watershed-scale richness")+
  theme_bw()

ggarrange(
  point_pools,
  lake_pools
)

#'
#'
#'
#'
#'
#' ## Integrating Component Drivers:
#' 
#'  Diversity ~ InvaderAbund + Light + Invader*Light + SpeciesPool 
#'  
# one bigass test: --------------------------------------------------------


#Lake level:
#CLP-D
D_clp_full_lake <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(D_clp_full_lake)

#CLP-R
R_clp_full_lake <- lmer(nat_richness ~ potcri_early_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(R_clp_full_lake)

#CLP-E
E_clp_full_lake <- lmer(nat_evenness ~ potcri_early_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(E_clp_full_lake)

#EWM-D
D_ewm_full_lake <- lmer(simpsons_div_nat ~ myrspi_summer_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(D_ewm_full_lake)

#EWM-R
R_ewm_full_lake <- lmer(nat_richness ~ myrspi_summer_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(R_ewm_full_lake)

#EWM-E
E_ewm_full_lake <- lmer(nat_evenness ~ myrspi_summer_vegdfoc*Secchi_m + watershedrichness + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & !is.na(Secchi_m)] )
summary(E_ewm_full_lake)



#point level
#CLP_Div
D_clp_full <- lmer(simpsons_div_nat ~ potamogeton_crispus*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(D_clp_full)

#CLP_Ric
R_clp_full <- lmer(nat_richness ~ potamogeton_crispus*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(R_clp_full)

#CLP_Evn
E_clp_full <- lmer(nat_evenness ~ potamogeton_crispus*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(E_clp_full)

#EWM_Div
D_ewm_full <- lmer(simpsons_div_nat ~ myriophyllum_spicatum*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(D_ewm_full)

#EWM_Ric
R_ewm_full <- lmer(nat_richness ~ myriophyllum_spicatum*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(R_ewm_full)

#EWM_Evn
E_ewm_full <- lmer(nat_evenness ~ myriophyllum_spicatum*proplight + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide)
summary(E_ewm_full)


#' ### Interpretation:
#' 
#' At the lake scale, the addition of species pool into the model results in a
#' model with only watershed as a significant predictor. This is not surprising
#' given that we had noting sig from light & invader after just the additons of 
#' light (Secchi in section XXXXXXXXX). 
#' 
#' At the point scale, our models look very suspicious, with every predictor 
#' except the nested lake-watershed richness one being highly significant. 
#' 
#' Interestingly, we see from these last two models that the impact of CLP is 
#' ~1/2 that of EWM. 
#' 
#' Next, as light gets higher, the impact of EWM is more (~tripled!) while the
#' impact of CLP is less (~ halved!).
#' 
#' 
#' 
#' 

# Compile Model Tables - Point --------------------------------------------

inv_mods_full <- rbindlist(
  list(data.table(summary(D_clp_full_lake)$coef, keep.rownames = T),
       data.table(summary(R_clp_full_lake)$coef, keep.rownames = T),
       data.table(summary(E_clp_full_lake)$coef, keep.rownames = T), 
       data.table(summary(D_ewm_full_lake)$coef, keep.rownames = T),
       data.table(summary(R_ewm_full_lake)$coef, keep.rownames = T),
       data.table(summary(E_ewm_full_lake)$coef, keep.rownames = T),
       data.table(summary(D_clp_full)$coef[c(1:3,6:8),], keep.rownames = T),
       data.table(summary(R_clp_full)$coef[c(1:3,6:8),], keep.rownames = T),
       data.table(summary(E_clp_full)$coef[c(1:3,6:8),], keep.rownames = T), 
       data.table(summary(D_ewm_full)$coef[c(1:3,6:8),], keep.rownames = T),
       data.table(summary(R_ewm_full)$coef[c(1:3,6:8),], keep.rownames = T),
       data.table(summary(E_ewm_full)$coef[c(1:3,6:8),], keep.rownames = T)
  ), fill = TRUE
)

inv_mods_full[ , species := c(rep("Pcri", 15), rep("Mspi", 15), rep("Pcri", 18), rep("Mspi", 18)) ]
inv_mods_full[ , response := c(rep(c(rep("Div", 5),rep("Rich", 5), rep("Even", 5)), 2),
                               rep(c(rep("Div", 6),rep("Rich", 6), rep("Even", 6)), 2))]

inv_mods_full[, confint_lwr := Estimate - 1.96*`Std. Error`]
inv_mods_full[, confint_upr := Estimate + 1.96*`Std. Error`]



# fwrite(inv_mods_full, file = "data&scripts/data/output/integrated_mods.csv")







# invaders and light as filters of the pool -------------------------------

ggplot( data = plants_rakeabund_wide, aes( surveyrichness, nat_richness/surveyrichness))+
  geom_point()+
  geom_smooth(method = "lm")
#examples
# 
# #Lake level:
# #CLP-D
# D_clp_full_lake <- lmer(nat_richness/watershedrichness ~ potcri_early_vegdfoc*Secchi_m  + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & !is.na(Secchi_m)] )
# summary(D_clp_full_lake)
# 
# 
# #point level
# #CLP_Div
# D_clp_full <- glmer(nat_richness/surveyrichness ~ proplight*potamogeton_crispus + poly(yday(survey_date), 2) + surveyrichness + watershedrichness +  (1 | dow), data = plants_rakeabund_wide, family = binomial)
# summary(D_clp_full)
# 
# 








#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # Footer
#' 
#' ## Session Info:
sessionInfo()
#' 
#' 
#' ## Cite Packages:
citation("data.table") 
citation("lme4") 
citation("GGally") 




