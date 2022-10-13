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
#' level), + management data. 
#' 
#' We then exclude all (this is a hard "all" to figure out)  managed (invaders?, 
#' drawdowns?, harvested?, watershed WQ efforts?) systems from the data (should
#' I be doing this?), and analyze invader effects. 
#' 
#' evaluate light, invader competition, and species pools of aquatic plants in
#' MN Lakes as drivers of community assembly. 
#' 
#' 
#' 
#' 
#' 3. Cut managed lakes & redo?
#'



#' ## Document Preamble
  #+ warning = FALSE
  # load libraries ------------------------------------------------------------------
  # # Load Libraries
  library(data.table)
    update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
  # library(ggplot2)
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
  
  # this is an observation level dataset (our foundation plant data)
  plants <- fread(file = "data&scripts/data/output/plants_env_data.csv", drop = 1)
    
    #here we have aggregated the plants dataset into a typical format with a "wide" config, and a row for each point or rake throw 
    plants_occurrence_wide <- fread(file = "data&scripts/data/output/plants_env_data_wide.csv", drop = 1)
    
    #here we have thinned to only obs with point level abunds and aggregated to "wide" config with a row for each point or rake throw, species matrix contains relative abundances
    plants_rakeabund_wide <- fread(file = "data&scripts/data/output/plants_abund_env_data_wide.csv")
  
    # here we have aggregated plants dataset to the survey level, each row is a set of survey statistics (richness, diversity, etc.) and a lake level species matrix
    surveys <- fread(file = "data&scripts/data/output/surveys_aqplants.csv")
  
    # other companion data like watershed & lake geodata, MN plants checklist, watershed aggregation of plants database
<<<<<<< HEAD
    load("data&scripts/data/output/synthesis_script_datasets.Rdata")
=======
    load("synthesis_script_datasets.Rdata")
>>>>>>> 66ffd98ffdee3519f940daac17772883775fcfa2

  #managemnt data is cleaned in a companion script & leaves data loaded into ws
  source(file = "data&scripts/b1_community_assy_analysis.R") # will load and clean management data and leave in WS a mgmtdata file

#' 
#' 
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
#' ## Spring CLP & Summer surveys:
#'
#' Here we calc a spring CLP value and carry it forward to the summer survey
#' (where native metrics come from). 
#' 
  
  # add a CLP metric to summer surveys where CLP was measured during a reasonable time of year --------

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
  
  
  
  # Deprecated join method:
  # summer_surveys <- merge(summer_surveys, early_clpsurveys, by = c("DOW", "year", "LAKE_NAME", "SUBBASIN"), suffixes = c(".summer", ".spring"), all.x = T)
  
  summer_surveys[ , hist(potcri_early_vegdfoc)]
  
  # #any dups? Y
  # sum(duplicated(summer_surveys[ ,SURVEY_ID ,]))
  # 
  # #get those survey IDs
  # summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer]
  # 
  # #now all rows with those?
  # summer_surveys[SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], .(DATESURVEYSTART.spring, potcri_early_vegdfoc) , SURVEY_ID.summer]
  # 
  # #grab the date of the latest clp survey (many times we have multiple pre June surveys)
  # keepers <- summer_surveys[SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], .("DATESURVEYSTART.spring"=max(DATESURVEYSTART.spring)) , SURVEY_ID.summer]
  # 
  # keepers <- summer_surveys[keepers]
  # 
  # keepers[ , DATESURVEYSTART.spring := as.Date(DATESURVEYSTART.spring) ,]
  # keepers[ , year := as.integer(year) ]
  # 
  # #dump all duplicated summer surveys
  # summer_surveys <- summer_surveys[!SURVEY_ID.summer %in% summer_surveys[duplicated(summer_surveys[ ,SURVEY_ID.summer ,]), ][,SURVEY_ID.summer], ]
  # 
  # #re-add in those keepers
  # summer_surveys <- rbind(summer_surveys, keepers)
  # 
  # rm(keepers)
  
  #verify that summer surveys has clp spring data (NA indicates no spring survey was conducted capable of evaluating CLP abundance):
  
  summer_surveys[ , .N , SPRING_Potamogeton_crispus==0]

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
  
  
  
# viz lake level invader effects ----------------------------------------------

#' ## Lake - P/A - Abund - Inv Eff - Viz

  # visualize "effects" at lake level
  summer_surveys[, myrspi_summer_vegdfoc := Myriophyllum_spicatum/n_points_vegetated]
  summer_surveys[is.na(myrspi_summer_vegdfoc) , myrspi_summer_vegdfoc := 0 , ]
  
  summer_surveys[ , .N , n_points_vegetated>0 ]
  summer_surveys[ , .N , myrspi_summer_vegdfoc>0]
  
  summer_surveys[, potcri_summer_vegdfoc := Potamogeton_crispus/n_points_vegetated]
  summer_surveys[is.na(potcri_summer_vegdfoc) , potcri_summer_vegdfoc := 0 , ]
  
  # figure 1: combined lake level CLP & EWM abundance effects
  
  legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
  INV_ENSpie <- ggplot()+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
               alpha = 0.4)+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
               alpha = 0.4)+
    # geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
    #             aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
    #             method = "lm")+
    # geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
    #             aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
    #             method = "lm")+
    xlab("Invader Lakewide Prevalence")+
    ylab("Native Species ENSpie")+
    ggtitle("INVADED LAKE SURVEYS")+
    labs(color = NULL) +
    scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
    theme_bw()+
    theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"))
  
  
  
  INV_richness <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, nat_richness),
               alpha = 0.4, color = "red")+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, nat_richness),
               alpha = 0.4, color = "blue")+
    # geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
    #             aes(potcri_early_vegdfoc, nat_richness),
    #             method = "lm", color = "red")+
    # geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
    #             aes(myrspi_summer_vegdfoc, nat_richness),
    #             method = "lm", color = "blue")+
    xlab(NULL)+
    ylab("Richness")+
    ggtitle("")+
    theme_bw()+
    theme(axis.text.y.left = element_text(angle = 90))
  INV_evenness <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
               alpha = 0.4, color = "red")+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
               alpha = 0.4, color = "blue")+
    geom_smooth(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
                aes(potcri_early_vegdfoc, simpsons_div_nat/nat_richness),
                method = "lm", color = "red", lty = 2, se =F)+
    geom_smooth(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
                aes(myrspi_summer_vegdfoc, simpsons_div_nat/nat_richness),
                method = "lm", color = "blue", lty = 2, se =F)+
    xlab("Invader Lakewide Prevalence")+
    ylab("Evenness")+
    theme_bw()+
    theme(axis.text.y.left = element_text(angle = 90))
  
  ggarrange(INV_ENSpie, ggarrange(INV_richness, INV_evenness, ncol = 1))
  
  
  #presence absence lake scale boxplots
  
  clpboxplot <- summer_surveys[!is.na(potcri_early_vegdfoc) & is.na(clp_targeted),
                               "inv.pres" := potcri_early_vegdfoc > 0 ]
  clpboxplot[ , species := "CLP" ,]
  clpboxplot <- clpboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]
  
  ewmboxplot <- summer_surveys[!is.na(myrspi_summer_vegdfoc) & is.na(ewm_targeted),
                               "inv.pres" := myrspi_summer_vegdfoc > 0 ]
  ewmboxplot[ , species := "EWM" ,]
  ewmboxplot <- ewmboxplot[!is.na(inv.pres) , .SD , .SDcols = c("inv.pres", "species", "nat_richness", "simpsons_div_nat")]
  
  boxplot_data <- rbind(ewmboxplot,clpboxplot)
  rm(clpboxplot, ewmboxplot)
  
  boxplot_data[ , evenness := simpsons_div_nat/nat_richness]
  boxplot_data[is.na(evenness), evenness := 0]
  
  
  
  boxplot_data <- melt(boxplot_data, id.vars = c("inv.pres","species"), variable.name = "metric", value.name = "value")
  
  boxplot_data[ , metric:= factor(metric, levels = c("simpsons_div_nat", "nat_richness", "evenness")),]
  
  box1 <- ggplot(data = boxplot_data[metric == "simpsons_div_nat"], aes(species, value, fill = inv.pres))+
    geom_boxplot()+
    scale_x_discrete(limits= c("CLP", "EWM"),labels=c("Potamogeton crispus","Myriophyllum spicatum"))+
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



# tests lake level invader effects ----------------------------------------
  ## Presence/Abs
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



  ## ABUNDANCE 
    ####ENSPie
      #EWM Abund
      EWMabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)] )
      summary(EWMabund_nat_lake_ENSpie)
      plot(EWMabund_nat_lake_ENSpie)


      #CLP Abund
      CLPabund_nat_lake_ENSpie <- lmer(simpsons_div_nat~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)])
      summary(CLPabund_nat_lake_ENSpie)
      plot(CLPabund_nat_lake_ENSpie)

    ####Richness
      #EWM Abund
      EWMabund_nat_lake_richness <- glmer(nat_richness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)], family = poisson())
      summary(EWMabund_nat_lake_richness)
      plot(EWMabund_nat_lake_richness)
      
      #CLP Abund
      CLPabund_nat_lake_richness <- glmer(nat_richness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)] , family = poisson())
      summary(CLPabund_nat_lake_richness)
      plot(CLPabund_nat_lake_richness)
    
    #### Evenness
      #EWM Abund
      EWMabund_nat_lake_evenness<- glmer(simpsons_div_nat/nat_richness~myrspi_summer_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)], family = binomial())
      summary(EWMabund_nat_lake_evenness)
      plot(EWMabund_nat_lake_evenness)
      
      #CLP Abund
      CLPabund_nat_lake_evenness <- glmer(simpsons_div_nat/nat_richness~potcri_early_vegdfoc+ (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)] , family = binomial())
      summary(CLPabund_nat_lake_evenness)
      plot(CLPabund_nat_lake_evenness)


# visualizations from tests ---------------------------------------------------------

  # ENSPie ~ Abund: EWM
  nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)])
  ## [1] 438
  newdat1<-data.frame(myrspi_summer_vegdfoc = seq(min(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), myrspi_summer_vegdfoc]), max(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), myrspi_summer_vegdfoc]), length.out = nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)])),
                      year = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), year],
                      DOW = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), DOW])
  #fitted values
  
  newdat1$fitted <- predict(EWMabund_nat_lake_ENSpie, newdat1, re.form = NA)
  
  preds <- predictInterval(EWMabund_nat_lake_ENSpie, newdat1, which = "fixed", n.sims = 9999)
  
  newdat1 <- cbind(newdat1,preds)
  # newdat1$fitted<-predict(EWMabund_nat_lake_ENSpie, newdat=newdat1, se.fit = TRUE)$fit
  # 
  # newdat1$se<-predict(EWMabund_nat_lake_ENSpie, newdat=newdat1, se.fit = TRUE)$se.fit
  
  # ENSPie ~ Abund: CLP
  nrow(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)])
  ## [1] 503
  newdat2<-data.frame(potcri_early_vegdfoc = seq(min(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), potcri_early_vegdfoc]), max(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), potcri_early_vegdfoc]), length.out = nrow(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)])),
                      year = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), year],
                      DOW = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), DOW])
  #fitted values
  
  newdat2$fitted <- predict(CLPabund_nat_lake_ENSpie, newdat2, re.form = NA)
  
  preds2 <- predictInterval(CLPabund_nat_lake_ENSpie, newdat2, which = "fixed", n.sims = 9999)
  
  newdat2 <- cbind(newdat2,preds2)
  
  # newdat2$fitted<-predict(CLPabund_nat_lake_ENSpie, newdat=newdat2, se.fit = TRUE)$fit
  # 
  # newdat2$se<-predict(CLPabund_nat_lake_ENSpie, newdat=newdat2, se.fit = TRUE)$se.fit
  
  
  
  legend_colors <- c("potcri_early_vegdfoc" = "blue", "myrspi_summer_vegdfoc" = "red")
  INV_ENSpie <- ggplot()+
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, simpsons_div_nat, color = "potcri_early_vegdfoc"),
               alpha = 0.4)+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, simpsons_div_nat, color = "myrspi_summer_vegdfoc"),
               alpha = 0.4)+
    geom_line(data = newdat1, 
              aes(myrspi_summer_vegdfoc,fit), color = "red", size = 1)+
    geom_ribbon(data = newdat1, aes(x = myrspi_summer_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "red", alpha = .15)+
    geom_line(data = newdat2, 
              aes(potcri_early_vegdfoc,fitted), color = "blue", size = 1, lty = 2)+
    # geom_ribbon(data = newdat2, aes(x = potcri_early_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "blue", alpha = .15)+
    xlab("Invader Lakewide Prevalence")+
    ylab("Native Species ENSpie")+
    ggtitle("INVADED LAKE SURVEYS")+
    labs(color = NULL) +
    scale_color_manual(values = legend_colors, labels = c("Potamogeton crispus","Myriophyllum spicatum"))+
    theme_bw()+
    theme(legend.position = c(0.6, 0.9), legend.background = element_blank(), legend.text = element_text(face = "italic"))
  
  
  #Richness~Abund: EWM
  
  nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)])
  ## [1] 483
  newdat1<-data.frame(myrspi_summer_vegdfoc = seq(min(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), myrspi_summer_vegdfoc]), max(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), myrspi_summer_vegdfoc]), length.out = nrow(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)])),
                      year = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), year],
                      DOW = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted), DOW])
  #fitted values
  
  newdat1$fitted <- exp(predict(EWMabund_nat_lake_richness, newdat1, re.form = NA))
  
  preds <- exp(predictInterval(EWMabund_nat_lake_richness, newdat1, which = "fixed", n.sims = 9999))
  
  newdat1 <- cbind(newdat1,preds)
  
  # newdat1$fitted<-exp(predict(EWMabund_nat_lake_richness, newdat=newdat1, se.fit = TRUE)$fit)
  # 
  # newdat1$se<-exp(predict(EWMabund_nat_lake_richness, newdat=newdat1, se.fit = TRUE)$se.fit)
  
  #Richness~Abund: CLP
  nrow(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)])
  ## [1] 488
  newdat2<-data.frame(potcri_early_vegdfoc = seq(min(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), potcri_early_vegdfoc]), max(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), potcri_early_vegdfoc]), length.out = nrow(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)])),
                      year = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), year],
                      DOW = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted), DOW]
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
    geom_point(data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)],
               aes(potcri_early_vegdfoc, nat_richness),
               alpha = 0.4, color = "red")+
    geom_point(data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ],
               aes(myrspi_summer_vegdfoc, nat_richness),
               alpha = 0.4, color = "blue")+
    geom_line(data = newdat1, 
              aes(myrspi_summer_vegdfoc,fit), color = "red", size = 1)+
    geom_ribbon(data = newdat1, aes(x = myrspi_summer_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "red", alpha = .15)+
    geom_line(data = newdat2, 
              aes(potcri_early_vegdfoc,fit), color = "blue", size = 1)+
    geom_ribbon(data = newdat2, aes(x = potcri_early_vegdfoc, ymin=lwr, ymax=upr), color = NA , fill = "blue", alpha = .15)+
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



# viz point level rake abund --------------------------------------------------

#ENSpie
names(plants_rakeabund_wide) <- names(clean_names(plants_rakeabund_wide))

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

# Figure 2: Compile boxplots and abund plots

ggarrange(point_boxps,point_abunds, ncol = 1)


#  test point level rake abund --------------------------------------------


#Div:
summary(lmer(simpsons_div_nat~potamogeton_crispus+ (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0 & !simpsons_div_nat == Inf]))

summary(lmer(simpsons_div_nat~myriophyllum_spicatum + (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0 & !simpsons_div_nat == Inf]))


#Rich:
summary(glmer(nat_richness~potamogeton_crispus+ (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0], family = poisson()))

summary(glmer(nat_richness~myriophyllum_spicatum+ (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0], family = poisson()))

#evenness - NEEDS TROUBLESHOOTING
summary(glmer(simpsons_div_nat/nat_richness~potamogeton_crispus + (1 + potamogeton_crispus| survey_id ), data = plants_rakeabund_wide[potamogeton_crispus> 0 & !simpsons_div_nat == Inf], family = binomial()))

summary(glmer(simpsons_div_nat/nat_richness~myriophyllum_spicatum + (1 + myriophyllum_spicatum| survey_id ), data = plants_rakeabund_wide[myriophyllum_spicatum> 0 & !simpsons_div_nat == Inf], family = binomial()))




# test point level p/a (only rake data) -----------------------------------


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
  geom_boxplot()+
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
  geom_violin()+
  geom_smooth(method = "lm")
summary(glm(simpsons_div_nat/nat_richness~potamogeton_crispus, data = plants_rakeabund_wide[!simpsons_div_nat == Inf], family = binomial()))

ggplot(plants_rakeabund_wide, aes(myriophyllum_spicatum>0, simpsons_div_nat/nat_richness) )+
  geom_violin()+
  geom_smooth(method = "lm")
summary(glm(simpsons_div_nat/nat_richness~myriophyllum_spicatum, data = plants_rakeabund_wide[!simpsons_div_nat == Inf], family = binomial(link = 'logit')))







# viz & test point level p/a (all occ) ------------------------------------------------



#individual comparions with tests:
#ENSpie
# not possible, no evenness metrics with only local p/a data

#richness?
#clp
ggplot(plants_occurrence_wide[NO_VEG_FOUND == F], aes(`Potamogeton crispus`, nat_richness) )+
  geom_boxplot()
summary(glm(nat_richness~`Potamogeton crispus`, data = plants_occurrence_wide[NO_VEG_FOUND == F], family = poisson()))
#ewm
ggplot(plants_occurrence_wide[NO_VEG_FOUND == F], aes(`Myriophyllum spicatum`, nat_richness) )+
  geom_boxplot(aes(group = `Myriophyllum spicatum`))
summary(glm(nat_richness~`Myriophyllum spicatum`, data = plants_occurrence_wide[NO_VEG_FOUND == F], family = poisson()))

#evenness?
#see note for ENSpie


# lake level light/ Secchi ------------------------------------------------
summer_surveys[ , .N , nat_richness][order(nat_richness)]

#whats the relationship to clarity look like in uninvaded lakes

summer_surveys[ , invaded := (potcri_early_vegdfoc > 0 | myrspi_summer_vegdfoc > 0)|
                  (is.na(potcri_early_vegdfoc) & myrspi_summer_vegdfoc > 0),]
summer_surveys[ is.na(potcri_early_vegdfoc) , invaded := myrspi_summer_vegdfoc > 0 ,]
#check work:
summer_surveys[ , .N , .(myrspi_summer_vegdfoc>0,potcri_early_vegdfoc>0, invaded ) ]


nat_ENSpie <- ggplot(summer_surveys[!is.na(Secchi_m) , ],
                     aes(Secchi_m,simpsons_div_nat, color = invaded))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Secchi")+
  ylab("ENSpie")+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())


clp_secchi <- ggplot(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)], aes(Secchi_m, potcri_early_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  ylab("CLP Abundance")+
  theme_bw()

ewm_secchi <- ggplot(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) ], aes(Secchi_m, myrspi_summer_vegdfoc))+
  geom_point( alpha = 0.4)+
  geom_smooth( method = "lm")+
  ylab("EWM Abundance")+
  theme_bw()

ggarrange(nat_ENSpie, clp_secchi, ewm_secchi, ncol = 1)

#groom the secchi data for pretty plotting:

summer_surveys[,groomed_secchi := round(Secchi_m/.5,0)*.5  , ]
summer_surveys[ , groomed_secchi := as.factor(groomed_secchi) , ]
summer_surveys[Secchi_m >= 4 ,groomed_secchi := "4+"  , ]
summer_surveys[Secchi_m < 0.5 ,groomed_secchi := "<0.5"  , ]
summer_surveys[ , groomed_secchi := factor(groomed_secchi) , ]
summer_surveys[ , unique(groomed_secchi)]
summer_surveys[ , groomed_secchi := factor(groomed_secchi, levels = c(
  "<0.5", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4+"
)) , ]

summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted) & !is.na(Secchi_m), .N, groomed_secchi]

clp_secchi_ENSpie <- ggplot(summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted) & !is.na(groomed_secchi)], aes(potcri_early_vegdfoc, simpsons_div_nat))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab(NULL)+
  ylab("ENSpie")+
  ggtitle("Secchi depth (meters)")+
  facet_wrap(~ groomed_secchi, nrow = 1 )+
  theme(axis.text.x = element_blank())


ewm_secchi_ENSpie <- ggplot(summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) & !is.na(groomed_secchi) ], aes(myrspi_summer_vegdfoc, simpsons_div_nat))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Invader Lakewide Prevalence")+
  ylab("ENSpie")+
  ggtitle("Secchi depth (meters)")+
  facet_wrap(~ groomed_secchi, nrow = 1 )+
  theme(axis.text.x = element_text(angle = 90))

ggarrange(clp_secchi_ENSpie, ewm_secchi_ENSpie, ncol = 1, labels = c("Potamogeton crispus", "Myriophyllum spicatum"), label.x = c(-.05,-.05), label.y = c(.7,.7))

#now we want to ask if the relationship between the invaders abund and native species sticks when we consider WQ:
m.nat.clp <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted) & !is.na(Secchi_m)])

summary(m.nat.clp)

m.nat.clp.wc <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc * Secchi_m + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)& !is.na(Secchi_m)] )

summary(m.nat.clp.wc) # here everything becomes non significant

# plot_model(m.nat.clp.wc, type = "pred", terms = "potcri_early_vegdfoc")
# plot_model(m.nat.clp, type = "pred", terms = "potcri_early_vegdfoc")

# AIC(m.nat.clp,m.nat.clp.wc )

#then do the same for EWM
m.nat.ewm <- lmer(simpsons_div_nat ~ myrspi_summer_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) & !is.na(Secchi_m)])

summary(m.nat.ewm)


m.nat.ewm.wc <- lmer(simpsons_div_nat ~ Secchi_m * myrspi_summer_vegdfoc + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted) & !is.na(Secchi_m)] )

summary(m.nat.ewm.wc) # here EWM and the interaction are non-significant


# plot_model(m.nat.ewm.wc, type = "pred", terms = "myrspi_summer_vegdfoc")
# plot_model(m.nat.ewm, type = "pred", terms = "myrspi_summer_vegdfoc")

# we could also ask if the decline in the positive secchi effect is significant:
ggplot(summer_surveys[!is.na(Secchi_m) , ],
       aes(Secchi_m,simpsons_div_nat, color = invaded))+
  geom_point( alpha = 0.4)+
  geom_smooth(method = "lm")+
  xlab("Secchi")+
  ylab("ENSpie")+
  scale_color_manual(values = c("black", "red"), labels = c("Uninvaded","Invaded"))+
  theme_bw()+
  theme(legend.position = c(0.6, 0.8), legend.background = element_blank(), legend.text = element_text(face = "italic"), legend.title = element_blank())

secchi_nat_inv <- lmer(simpsons_div_nat~invaded*Secchi_m + (1|DOW) + (1| year) , 
                       data = summer_surveys[!is.na(Secchi_m) , ])

summary(secchi_nat_inv)


# point level light avail -------------------------------------------------

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

ggplot( data = plants_rakeabund_wide,
        aes(surveyrichness, nat_richness))+
  geom_point()+
  geom_smooth(method = "lm")

point_rich_lake_pool <- glm(nat_richness~surveyrichness, data = plants_rakeabund_wide, family = poisson())
summary(point_rich_lake_pool)

# watershed richness as the predictor of a community : 

ggplot( data = plants_rakeabund_wide,
        aes(watershedrichness, nat_richness))+
  geom_point()+
  geom_smooth(method = "lm")

point_rich_watershed_pool <- glm(nat_richness~watershedrichness, data = plants_rakeabund_wide, family = poisson())
summary(point_rich_watershed_pool)


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


#redo all species pool plots with ENSpie:
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

#one bigass test:

#CLP
clp_full <- lmer(simpsons_div_nat ~ potcri_early_vegdfoc*Secchi_m + watershedsimpson + (1|DOW) + (1| year), data = summer_surveys[potcri_early_vegdfoc > 0 & is.na(clp_targeted)& !is.na(Secchi_m)] )

summary(clp_full)


#EWM
ewm_full <- lmer(simpsons_div_nat ~ myrspi_summer_vegdfoc*Secchi_m + watershedsimpson + (1|DOW) + (1| year), data = summer_surveys[myrspi_summer_vegdfoc > 0 & is.na(ewm_targeted)& !is.na(Secchi_m)] )

summary(ewm_full)





# cursor catcher
