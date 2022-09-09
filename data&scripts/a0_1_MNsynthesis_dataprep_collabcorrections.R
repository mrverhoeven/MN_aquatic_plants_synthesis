#'---
#' title: "Statewide Synthesis - Collaboator data corrections"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull in data for PI surveys and the data that were
#' collected by our collaborators in the data check/inventory surveys. 
#' 



#' ## Document Preamble
#+ warning = FALSE
# load libraries ------------------------------------------------------------------
# Load Libraries
# library(data.table) # remotes::install_github("Rdatatable/data.table")
# library(ggplot2)
# library(stringr)
# library(sf)
# library(maps)
# library(rgdal)
# library(ggsn)
# library(moments)
# library(shiny)
# library(plotly)
# library(ggspatial)
# library(broom)
# library(woodson)
# library(vegan)


# load in functions -------------------------------------------------------


# load in data -------------------------------------------------
  #dependent on master script: a_MNsynthesis_dataprep.R


# check survey ID alignment

sum(!coll_edits[, SURVEY_ID, ] %in% plants[ , SURVEY_ID]) #100% of collaborator input has a match in plants

names(coll_edits)[1] <- "feedback"


coll_edits[ , .N , feedback ]


# deletions ---------------------------------------------------------------

coll_edits[ str_detect(feedback, "delete",), SURVEY_ID ,    ]#which are marked for deletion?

plants <- plants[  !SURVEY_ID %in% 
           coll_edits[ str_detect(feedback, "delete",), SURVEY_ID ,    ]#this drops about 10k observations from the dataset
         , , ]

# tag for reimport --------------------------------------------------------
# these surveys need to be reimported and any current data deleted. We have these data in our files. 

coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ]

plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ], .N ,  SURVEY_ID]

sel <- plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ],  ,  ] #peel off those reimport data

sel <- sel[!duplicated(sel[ , SURVEY_ID , ]),] #compress to one row where botched import produced some data

plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ], .N ,  SURVEY_ID]

# drop in modify some cols to reflect bad import
sel[ , c("STA_NBR_DATASOURCE", "DEPTH_FT", "NO_VEG_FOUND", "REL_ABUND", "WHOLE_RAKE_REL_ABUND","SUBSTRATE", "SURVEYOR", "TAXON", "SAMPLE_NOTES", "SURFACE_GROWTH", "POINT_LVL_SECCHI", "X", "Y", "NORTHING", "EASTING", "LATITUDE", "LONGITUDE", "UTMX", "UTMY", "POINT_ID", "OBS_ID") := NA ,]

sel[ , INDATABASE := FALSE]

plants <- plants[  !SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ],  ,  ] #drops ~900 obs

plants <- rbind(plants, sel)

plants[ SURVEY_ID %in% sel[, SURVEY_ID] , SURVEY_FEEDBACK := "reimport required" , ]

# data available from collaborator ----------------------------------------

coll_edits[ feedback %in% c("data available", "missing metadata") , SURVEY_ID , ]

plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("data available", "missing metadata") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "data available from collaborator" ,   ]

plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("rake density data available") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "rake density data available from collaborator" ,   ]


# no data available -------------------------------------------------------

plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("no data available") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "no data available" ,   ]

plants[SURVEY_FEEDBACK == "no data available" , .N , SURVEY_ID] 


# one-offs ----------------------------------------------------------------

coll_edits[feedback == "Point 69 has Lmin entered as 5 - change to 2", SURVEY_ID]

plants[SURVEY_ID == coll_edits[feedback == "Point 69 has Lmin entered as 5 - change to 2", SURVEY_ID] & 
         STA_NBR_DATASOURCE == 69 &
         REL_ABUND == 5,
      REL_ABUND := 2]


# preferred datasource name -----------------------------------------------

coll_edits[ , .N , EDIT_DATASOURCE]

plants[ , SURVEY_DATASOURCE := coll_edits[match(plants$DATASOURCE, coll_edits$DATASOURCE),  EDIT_DATASOURCE ] ,  ]

plants[ , .N ,  SURVEY_DATASOURCE]

plants[SURVEY_DATASOURCE == "", .N , DATASOURCE ]

plants[DATASOURCE == "DNR Lakes and Rivers", SURVEY_DATASOURCE := "DNR Lakes and Rivers"]

plants[DATASOURCE == "DNR Fisheries", SURVEY_DATASOURCE := "DNR Fisheries"]

plants[DATASOURCE == "Rantala TIP", SURVEY_DATASOURCE := "DNR Fisheries"]

plants[DATASOURCE == "Muthukrishnan Et al", SURVEY_DATASOURCE := "DNR Shallow Lakes" , ]

plants[SURVEY_DATASOURCE == "DNR Fisheries Research" , SURVEY_DATASOURCE := "DNR Fisheries"]

plants[ , length(unique(SURVEY_ID)) , SURVEY_DATASOURCE ]

# lake name corrections --------------------------------------------------

plants[ , NEW_LAKE_NAME := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_LAKE_NAME ] ,  ]

plants[NEW_LAKE_NAME %in% c("lake of the isles", "clear", "bde maka ska"), LAKE_NAME := NEW_LAKE_NAME ]

plants[ , NEW_LAKE_NAME := NULL ,]



# surveyor corrections ----------------------------------------------------


plants[ , NEW_SURVEYOR := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_SURVEYOR ] ,  ]

plants[, .N, NEW_SURVEYOR ]

plants[!NEW_SURVEYOR == "" & !is.na(NEW_SURVEYOR), SURVEYOR := NEW_SURVEYOR , ]

plants[ , NEW_SURVEYOR := NULL ,]


# date corrections --------------------------------------------------------

plants[ , NEW_DATE := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_DATE ] ,  ]

plants[, .N, NEW_DATE ]

plants[!NEW_DATE == "" & !is.na(NEW_DATE), .N , NEW_DATE ]

plants[!NEW_DATE == "" & !is.na(NEW_DATE) , SURVEY_DATE := as.Date(NEW_DATE, format = "%d%b%Y") ,]

plants[ , NEW_DATE := NULL ,]


# input rake density scales -----------------------------------------------
#overwrite any bad rake scales:
coll_edits[!is.na(`EDITED_SCALE_RAKE_DENS (0-X)`) , `SCALE_RAKE_DENS (0-X)` := `EDITED_SCALE_RAKE_DENS (0-X)`  ]
coll_edits[ ,.N , `SCALE_RAKE_DENS (0-X)`  ]
coll_edits[ `SCALE_RAKE_DENS (0-X)` %in% c(1,2) , `SCALE_RAKE_DENS (0-X)` := NA  ]

#push over to plants DB:
plants[ , RAKE_SCALE_USED := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  `SCALE_RAKE_DENS (0-X)` ] ,  ]

plants[ , max(REL_ABUND, na.rm = T) , RAKE_SCALE_USED]

plants[REL_ABUND>RAKE_SCALE_USED,  ]
plants[SURVEY_ID == 1418 , RAKE_SCALE_USED := 5]
plants[SURVEY_ID == 3128 , RAKE_SCALE_USED := 5]



# clean up WS -------------------------------------------------------------

rm(coll_edits, sel)







