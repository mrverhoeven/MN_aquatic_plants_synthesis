#'---
#' title: "Reviewing Rake Abundances "
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This code will clean the relative rake density data from the whole PI dataset, 
#' shifting all to a 0,1,2,3 scale. This code was developed in the
#' surveycollation project.
#' 
#' 
#' COntingencies:
#'  - data loaded from MNsynthesis_dataprep.R script
#'  - data georef done via MNsynthesis_dataprep_georef.R script
#' 
#' 

#' ##'Load Workspace

# load libraries ----------------------------------------------------------

# library(data.table)
# library(ggplot2)
# library(gridExtra)


# load in data ------------------------------------------------------------

## data loaded from MNsynthesis_dataprep.R script

# grab records with rake abundance data ----------------------------------------

# implement changes by survey -------------------------------------------

#drop surveys with max vals of 1s and 1-2s
rakes1 <- plants[RAKE_SCALE_USED %in% c(3,4,5), ]

rakes1[ , .N , REL_ABUND]


#how many surveys in these categories?
rakes1[  , .N  , SURVEY_ID] #982

#Now shift/ realign data per discussion above

#1-4 survey shifted to 1-3
rakes1[RAKE_SCALE_USED == 4 & REL_ABUND == 3 ,
       REL_ABUND := 2 ]
rakes1[RAKE_SCALE_USED == 4 & REL_ABUND == 4 ,
       REL_ABUND := 3 ]

#1-5 surveys shifted to 1-3
rakes1[RAKE_SCALE_USED == 5 & (REL_ABUND == 3 |REL_ABUND == 4) ,
       REL_ABUND := 2 ]
rakes1[RAKE_SCALE_USED == 5 & REL_ABUND == 5 ,
       REL_ABUND := 3 ]


# #check that the max vals are all 3's
# hist(rakes1[ !is.na(REL_ABUND) , max(REL_ABUND) , SURVEY_ID  ][,V1])
# 
# #and all data are distributed in 1-3 rake density framework
# hist(rakes1[ !is.na(REL_ABUND) , REL_ABUND ,  ])
# 
# # count the number of surveys we've got
# rakes1[ , .N , SURVEY_ID ] # N points per survey (includes NA's--points where no species were observed)


# put the corrected rake scales back into the plants db  -------------------

# plants[ , , ] 
# rakes1[ , .N , OBS_ID][N>1]

rakes1[is.na(OBS_ID) , ,]

#where people told us the rake scale but data to-date not in db:
rakes1 <- rakes1[!is.na(OBS_ID)]

plants[OBS_ID %in% rakes1$OBS_ID , REL_ABUND_CORRECTED := rakes1$REL_ABUND  , ]

#clea out intermediates
rm(rakes1)


