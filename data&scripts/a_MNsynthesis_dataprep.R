#'---
#' title: "Minnesota Statewide Aquatic Plants and Associated Data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' This script will pull in plant observation data from PI surveys, Secchi
#' clarity, lake/watershed geodata, species statuses, and collaborator feedback.
#' Then we'll sync them into our dataset, creating a full macrophyte obs and env
#' dataset for MN Lakes


#' ## Document Preamble
#+ warning = FALSE


# load libraries ------------------------------------------------------------------

#' ### Libraries

  library(data.table) 
    update_dev_pkg()# remotes::install_github("Rdatatable/data.table")
  library(ggplot2)
  library(stringr)
  library(sf)
  library(vegan)
  library(gridExtra)
  library(dplyr)
  library(tidyr)
  library(janitor)
  # library(lme4)
  # library(sjPlot)
  library(mediation)
  library(ggpubr)
  # library(EnvStats)
  # library(lmerTest)
  # library(merTools)
  # library(rstanarm)
  library(ggsn)


# load in functions -------------------------------------------------------
#' ### Functions

  f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}


# load in data -------------------------------------------------

#' ### Data
#' 
#' The plants observations data and collaborator corrections are datasets that
#' we have generated.
#' 
#' Secchi data have been aggregated from public sources by
#' Kelsey Vitense. 
#' 
#' Hydrography (https://gisdata.mn.gov/dataset/water-dnr-hydrography; 5April2022) and 
#' watershed (https://gisdata.mn.gov/dataset/geos-dnr-watersheds; 10Aug2022) data were
#' retrieved from the MN geospatial commons. 
#' 
#' Species statuses were retrieved from the MN DNR 
#' (https://www.dnr.state.mn.us/eco/mcbs/plant_lists.html; 5April2022).
#' 
#' 

  #plants observation dataset:
  plants <- fread(input = "data&scripts/data/input/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers
  
  #collaborator corrections and feedback:
  coll_edits <- fread(input = "data&scripts/data/input/Edited_post_contrib_feedback.csv")
  
  #secchi data:
  secchi <- fread(input = "data&scripts/data/input/AllSecchi_plus_ShallowLakesSecchi.csv") #import, dropping the exported row numbers
  
  #hydrography & watersheds
  pwi_l <- st_read(dsn = "data&scripts/data/input/shp_water_dnr_hydrography", layer = "dnr_hydro_features_all")
  watersheds_huc8 <- st_read(dsn = "data&scripts/data/input/shp_geos_dnr_watersheds", layer = "dnr_watersheds_dnr_level_04_huc_08_majors")
  
  #species statuses
  rte <- fread(input = "data&scripts/data/input/2013_dnr_plant_checklist_web.csv")



# collaborator corrections ------------------------------------------------

#' ## Collaborator Corrections
#' This section uses the collaborator feedback to revise the dataset.

  # check survey ID alignment
  #sum(!coll_edits[, SURVEY_ID, ] %in% plants[ , SURVEY_ID]) #100% of collaborator input has a match in plants
  names(coll_edits)[1] <- "feedback"
  coll_edits[ , .N , feedback ]
  
  # deletions 
  # coll_edits[ str_detect(feedback, "delete",), SURVEY_ID ,    ]#which are marked for deletion?
  plants <- plants[  !SURVEY_ID %in% 
                       coll_edits[ str_detect(feedback, "delete",), SURVEY_ID ,    ]#this drops about 10k observations from the dataset
                     , , ]
  
  # tag for reimport
  # these surveys need to be reimported and any current data deleted. We have these data in our files. 
  
  # coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ]
  # plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ], .N ,  SURVEY_ID]
  sel <- plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ],  ,  ] #peel off those reimport data
  sel <- sel[!duplicated(sel[ , SURVEY_ID , ]),] #compress to one row where botched import produced some data
  # plants[ SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ], .N ,  SURVEY_ID]
  
  # drop or modify some cols to reflect bad import
  sel[ , c("STA_NBR_DATASOURCE", "DEPTH_FT", "NO_VEG_FOUND", "REL_ABUND", "WHOLE_RAKE_REL_ABUND","SUBSTRATE", "SURVEYOR", "TAXON", "SAMPLE_NOTES", "SURFACE_GROWTH", "POINT_LVL_SECCHI", "X", "Y", "NORTHING", "EASTING", "LATITUDE", "LONGITUDE", "UTMX", "UTMY", "POINT_ID", "OBS_ID") := NA ,]#in sel, dump these columns
  sel[ , INDATABASE := FALSE]#mark these as not in database
  plants <- plants[  !SURVEY_ID %in% coll_edits[str_detect(feedback, "import",) , SURVEY_ID  , ],  ,  ] #drops ~900 obs
  plants <- rbind(plants, sel)
  plants[ SURVEY_ID %in% sel[, SURVEY_ID] , SURVEY_FEEDBACK := "reimport required" , ]
  
  # data available from collaborator
  # coll_edits[ feedback %in% c("data available", "missing metadata") , SURVEY_ID , ]
  plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("data available", "missing metadata") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "data available from collaborator" ,   ]
  plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("rake density data available") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "rake density data available from collaborator" ,   ]
  
  
  # no data available 
  plants[ SURVEY_ID %in% coll_edits[ feedback %in% c("no data available") , SURVEY_ID , ]  , SURVEY_FEEDBACK := "no data available" ,   ]
  nrow(plants[SURVEY_FEEDBACK == "no data available" , .N , SURVEY_ID] )# how many cases with data unavailable/ not known where raw data are?
  
  
  # one-offs
  # coll_edits[feedback == "Point 69 has Lmin entered as 5 - change to 2", SURVEY_ID]
  plants[SURVEY_ID == coll_edits[feedback == "Point 69 has Lmin entered as 5 - change to 2", SURVEY_ID] & 
           STA_NBR_DATASOURCE == 69 &
           REL_ABUND == 5,
         REL_ABUND := 2]
  
  # Taxa naming problem:
  plants[TAXON == "Mitellopsis", TAXON := "Nitellopsis"]
  
 
  # preferred datasource name
  # coll_edits[ , .N , EDIT_DATASOURCE]
  plants[ , SURVEY_DATASOURCE := coll_edits[match(plants$DATASOURCE, coll_edits$DATASOURCE),  EDIT_DATASOURCE ] ,  ]
  # plants[ , .N ,  SURVEY_DATASOURCE]
  # plants[SURVEY_DATASOURCE == "", .N , DATASOURCE ]
  plants[DATASOURCE == "DNR Lakes and Rivers", SURVEY_DATASOURCE := "DNR Lakes and Rivers"]
  plants[DATASOURCE == "DNR Fisheries", SURVEY_DATASOURCE := "DNR Fisheries"]
  plants[DATASOURCE == "Rantala TIP", SURVEY_DATASOURCE := "DNR Fisheries"]
  plants[DATASOURCE == "Muthukrishnan Et al", SURVEY_DATASOURCE := "DNR Shallow Lakes" , ]
  plants[SURVEY_DATASOURCE == "DNR Fisheries Research" , SURVEY_DATASOURCE := "DNR Fisheries"]
  
  # check contribution # of surveys by new named datasources
  plants[ , length(unique(SURVEY_ID)) , SURVEY_DATASOURCE ]
  
  
  # lake name corrections 
  plants[ , NEW_LAKE_NAME := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_LAKE_NAME ] ,  ]
  plants[NEW_LAKE_NAME %in% c("lake of the isles", "clear", "bde maka ska"), LAKE_NAME := NEW_LAKE_NAME ]
  plants[ , NEW_LAKE_NAME := NULL ,]
  
  
  # surveyor corrections
  plants[ , NEW_SURVEYOR := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_SURVEYOR ] ,  ]
  # plants[, .N, NEW_SURVEYOR ]
  plants[!NEW_SURVEYOR == "" & !is.na(NEW_SURVEYOR), SURVEYOR := NEW_SURVEYOR , ]
  plants[ , NEW_SURVEYOR := NULL ,]
  
  
  # date corrections
  plants[ , NEW_DATE := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  EDIT_DATE ] ,  ]
  # plants[, .N, NEW_DATE ]
  # plants[!NEW_DATE == "" & !is.na(NEW_DATE), .N , NEW_DATE ]
  plants[!NEW_DATE == "" & !is.na(NEW_DATE) , SURVEY_DATE := as.Date(NEW_DATE, format = "%d%b%Y") ,]
  plants[ , NEW_DATE := NULL ,]
  
  
  # input rake density scales
  #overwrite any bad rake scales:
  coll_edits[!is.na(`EDITED_SCALE_RAKE_DENS (0-X)`) , `SCALE_RAKE_DENS (0-X)` := `EDITED_SCALE_RAKE_DENS (0-X)`  ]
  # coll_edits[ ,.N , `SCALE_RAKE_DENS (0-X)`  ]
  coll_edits[ `SCALE_RAKE_DENS (0-X)` %in% c(1,2) , `SCALE_RAKE_DENS (0-X)` := NA  ]# these aren't real abundance scales--they should be marked as NA, to indicate only pres-abs data are useable.
  
  #push over to plants DB:
  plants[ , RAKE_SCALE_USED := coll_edits[match(plants$SURVEY_ID, coll_edits$SURVEY_ID),  `SCALE_RAKE_DENS (0-X)` ] ,  ]
  
  
  # for these surveys, we can see that our collaborators inputs on rake scale was not correct:
  plants[REL_ABUND>RAKE_SCALE_USED, .N , .(SURVEY_ID)  ]
  
  #two more left now to manually change to max scale observed rather than reported:
  plants[SURVEY_ID == 1418 , RAKE_SCALE_USED := 5]
  plants[SURVEY_ID == 3128 , RAKE_SCALE_USED := 5]
  
  # check process:
  plants[ , .("max_observed_in_data" = max(REL_ABUND, na.rm = T)) , RAKE_SCALE_USED]
  
  # clean up WS
  rm(coll_edits, sel)


# georeference data -------------------------------------------------------

#' ## Georeference Data
#' This section uses MN hydrography geodata to add direct geodata into the
#' dataset. After run, pwi_l and plants can be linked on the shared "order_ID"
#' column, and HUC-8 level watersheds are included in the dataset
  
  # merge geospatial files
  #change sf data.frame to a data.table
  setDT(pwi_l)
  
  # linking plants db to spatial reference:
  #shapefile dows need to be made numeric (drops leading zeros)
  # pwi_l[ , dowlknum , ]
  pwi_l[ , dow_main := round(as.numeric(dowlknum)/100,0)*100 , ]
  
  #there's a lot of junk in there, work towards a 1:1 of plants dows and pwi_l dows
  pwi_l <- pwi_l[!is.na(dowlknum)]# drops many polygons that aren't lakes (islands, rivers, etc)
  pwi_l[  , order_ID:= .I , ]#adds a key
  #drop non-mn shapes
  pwi_l <- pwi_l[!outside_mn == "Y"]
  #which dows are duplicated in the shapes?
  pwi_l[pwi_l[, duplicated(dowlknum),], dowlknum]
  #lets review those data and see if we can devise any cleaning ideas
  #pwi_l[dowlknum %in% pwi_l[pwi_l[, duplicated(dowlknum),], dowlknum],]
  # we can just use the first instance of these duplicated waterbodies:
  # we'll do that by dropping the subsequent duplicates!
  pwi_l <- pwi_l[!pwi_l[, duplicated(dowlknum),], , ]
  
  # missing matches in the plants data to shapefile dows
  sum(is.na(match(plants[ , unique(DOW) ,], unique(pwi_l[ , dowlknum , ]))))
  # missing matches in the plants data to shapefile mainlake dows
  sum(is.na(match(plants[ , unique(DOW) ,], unique(pwi_l[ , dow_main , ]))))
  
  #append a polygon value to the plants data (here we'll use our order_ID from above)
  plants[ , order_ID := pwi_l[ match( plants[ , DOW ,], pwi_l[ , as.numeric(dow_main) , ]) , order_ID , ]  ]
  
  #and any that didn't match on that, try the basin specific
  plants[ is.na(order_ID) , order_ID := pwi_l[ match(plants[ is.na(order_ID) , DOW ,], pwi_l[ , as.numeric(dowlknum) , ]), order_ID , ]  ]
  
  # now to navigate these last non-compliant ones...
  plants[is.na(order_ID) & !is.na(DOW), .N ,  .(LAKE_NAME, DOW, DATASOURCE)]
  
  pwi_l[ dowlknum == "40000200", order_ID]
  plants[ DOW == 40000201, order_ID := pwi_l[ dowlknum == "40000200", order_ID] ] #Upper Sakatah polygon
  
  pwi_l[ dowlknum == "68000500", order_ID]
  plants[ DOW %in% c(68000501,68000502), order_ID := pwi_l[ dowlknum == "68000500", order_ID] ] #Roseau River WMA
  
  pwi_l[ dowlknum == "70005000", order_ID]
  plants[ DOW == 70050000, order_ID := pwi_l[ dowlknum == "70005000", order_ID] ] #Carls Lake
  
  pwi_l[ dowlknum == "82011800", order_ID]
  plants[ DOW == 82009999, order_ID := pwi_l[ dowlknum == "82011800", order_ID] ] #Katherine Abbott Pond
  
  # the plants datset lakes with no geodata in the hydrography layer we used:
  plants[is.na(order_ID) , .N ,  .(LAKE_NAME, DOW, DATASOURCE)]
  # in total, this is 18 surveys and 3611 observations without ANY geolocation
  plants[, summary(order_ID)]
  
  
  # fix up local geospatial info 
  #check for weird X,Y vals in th UTM-looking columns
  plants[!is.na("X"), summary(X) ,]
  plants[!is.na("Y"), summary(Y) ,]
  
  #some are clearly lat/longs
  plants[X < 4600, X ,]
  plants[X < 4600, LATITUDE := X ,]
  plants[X < 4600, X := NA ,]
  
  plants[Y<10000, summary(Y) ,]
  plants[Y<10000 & Y>0,  LATITUDE := Y ,]
  plants[Y<10000 & Y>0,  Y := NA ,]
  
  plants[Y<10000, LONGITUDE := Y  ,]
  plants[Y<10000, Y := NA  ,]
  
  #whatever the heck is leftover here is weeeeeird and muddled.
  plants[!is.na("X"), summary(X) ,]
  plants[!is.na("Y"), summary(Y) ,]
  
  # we need to delete these non-UTM vals form X & Y
  # plants[Y<4800000, .N, DATASOURCE ]
  plants[ Y < 4800000, c("X","Y") := NA,  ]
  
  #looks clean, now move into the UTM slots?  
  plants[!is.na(UTMY) , summary(UTMY) ,]
  plants[!is.na(UTMX) , summary(UTMX) ,]
  #any conflicts with UTM loc data?
  plants[!is.na(UTMX) & !is.na(X)]
  plants[!is.na(UTMY) & !is.na(Y)]
  #move X, Y to UTMs
  plants[!is.na(X) , UTMX := X ,  ]
  plants[!is.na(Y) , UTMY := Y ,  ]
  
  plants[ , c("X", "Y") := NULL , ]
  
  
  #now Northing Easting, which happen to look like clean UTM data
  plants[!is.na(NORTHING), summary(NORTHING) ,]
  plants[!is.na(EASTING), summary(EASTING) ,]
  
  #overlap/ conflict?
  plants[!is.na(UTMX) & !is.na(NORTHING)]
  plants[!is.na(UTMY) & !is.na(EASTING)]
  
  #move Northing and easting to UTMs
  plants[!is.na(NORTHING), UTMY := NORTHING  ,]
  plants[!is.na(EASTING), UTMX := EASTING  ,]
  
  plants[ , c("NORTHING", "EASTING") := NULL , ]
  
  #now get all into same CRS:
  #conflicts?
  plants[!is.na(UTMX) & !is.na(LONGITUDE)]
  plants[!is.na(UTMY) & !is.na(LATITUDE)]
  
  #here we'll split into a non, UTM, and LL georef set, then convert ref'd to sf objects, then merge all back together 
  # plants complete x,y in one CRS or another? NOPE... Oh well. moving on.
  # plants[!is.na(UTMX) & is.na(UTMY)]
  # # plants[]
  # 
  # plants[!is.na(LATITUDE) & is.na(LONGITUDE)]
  # plants[is.na(LATITUDE) & !is.na(LONGITUDE)]
  
  #Conversion of data frame to sf object (note we've assumed NAD1983, Z15N for UTMs)
  plants_UTMS <- st_as_sf(x = plants[!is.na(UTMX)],                         
                          coords = c("UTMX", "UTMY"),
                          crs = "+proj=utm +zone=15")
  
  #Projection transformation
  plants_U_LL = st_transform(plants_UTMS, crs = "+proj=longlat +datum=WGS84")
  
  setDT(plants_U_LL)
  
  #Conversion of data frame to sf object
  plants_LLS <- st_as_sf(x = plants[!is.na(LONGITUDE)],                         
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "+proj=longlat +datum=WGS84")
  
  setDT(plants_LLS)
  
  #drop unusedCRS cols from each:
  plants_U_LL[ , c("LATITUDE", "LONGITUDE") := NULL , ]
  plants_LLS[ , c("UTMX","UTMY") := NULL, ]
  
  plants2 <- rbindlist(list(plants_LLS, plants_U_LL))
  
  plants2 <- cbind(plants2, st_coordinates(st_as_sf(plants2)))
  
  plants2[ , geometry := NULL ,]
  
  names(plants2)[names(plants2)%in% c("X","Y")] <- c("Longitude","Latitude")
  
  #merge back to plants (check dims to ensure no duplications or overlaps):
  dim(plants)
  
  plants[is.na(UTMX) & is.na(LONGITUDE) , .N , ]+
    plants2[ , .N ,]
  
  plants1 <- plants[is.na(UTMX) & is.na(LONGITUDE), ]
  
  plants1[ , c("UTMX", "UTMY", "LATITUDE", "LONGITUDE") := NULL ,]
  
  plants1[ , c("Longitude", "Latitude") := NA, ]
  
  nrow(plants1)+nrow(plants2)
  
  plants <- rbindlist(list(plants1, plants2))
  
  rm(plants_LLS,plants_U_LL, plants1, plants2)
  
  # label all pwi_l with watershed names
  
  # st_join(pwi_l, watersheds_huc8)
  # 
  # st_crs(plants_UTMS) <- st_crs(watersheds_huc8)
  # 
  # plantsUTMS <- st_join(plants_UTMS, left = TRUE, watersheds_huc8)
  
  pwi_l <- st_sf(pwi_l)
  st_crs(pwi_l) <- st_crs(watersheds_huc8) #ignore warning, no re-projection needed in this case, we do this because I lost the crs in some of my data manipulation
  pwi_l <- st_join(pwi_l, left = TRUE, watersheds_huc8)
  setDT(pwi_l)
  rm(plants_pts, plants_UTMS)

  
# rake scale normalization -------------------------------------------
  
#' ## Rake Abundance Normalization
#' This code will clean the relative rake density data from the whole PI dataset, 
#' shifting all to a 0,1,2,3 scale. This code was developed in the
#' surveycollation project, but is implemented here (post-collaborator feedback) 
#' to allow the collabs to specify what the rake scale they used was.
  
  #drop surveys with max vals of 1s and 1-2s
  rakes1 <- plants[RAKE_SCALE_USED %in% c(3,4,5), ]
  # rakes1[ , .N , REL_ABUND]
  #how many surveys in these categories?
  # rakes1[  , .N  , SURVEY_ID] #982
  
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
  
  
  # put the corrected rake scales back into the plants db
  # plants[ , , ] 
  # rakes1[ , .N , OBS_ID][N>1]
  
  # rakes1[is.na(OBS_ID) , ,]
  
  #where people told us the rake scale but data to-date not in db:
  rakes1 <- rakes1[!is.na(OBS_ID)]
  
  #pop these corrected rake scale data into the plants dataset
  plants[OBS_ID %in% rakes1$OBS_ID , REL_ABUND_CORRECTED := rakes1$REL_ABUND  , ]
  
  #clean out intermediates
  rm(rakes1)
  
# add in secchi data ------------------------------------------------------
  
#' ## Secchi Data Join
#'
#' This code will conduct an eval of the fuzzy join of Secchi to plants data,
#' calculate Secchi metrics based on the chosen fuzzy join, then excute the join.
#' The code includes a solution adapted from a script written by Dan Larkin for
#' the niches project (https://conservancy.umn.edu/handle/11299/218009).
#'  
#' Assign a Secchi to each observation based either via:
#' 
#'    1. use the closest Secchi temporally (currently active code)
#'    2. use an averaging method to grab close Secchi values (commented out code)
#'    
#' ### Closest Temporal Secchi Obs
#' For each plant observation, we'll append the Secchi observation from that DOW
#' that was closest in time to the plant obs.
#'    

  # number of observations
  hist(secchi[,year(Date)])
  hist(secchi[,month(Date)])
  secchi[,.N,Source]
  
  # and for the plants data?
  hist(plants[ ,.N , .(SURVEY_ID,YEAR) ][ , YEAR,])
  
  #clean some data in prep for join
  secchi[, YEAR := year(Date)]
  secchi[, MONTH := month(Date) ]
  
  secchi[ , old_DOW := DOW]
  secchi[, DOW := as.integer(DOW)]
  secchi[ is.na(DOW) , old_DOW ]
  
  
  #how many survey DOW's have a secchi for the lake (ever)?
  summary(plants[ , unique(DOW) , ]%in%secchi[ ,DOW ,])
  
  #how many surveys have a secchi for that year?
  summary(plants[ , .N ,.(DOW,YEAR) ][,paste(DOW,YEAR, sep = "_"),] %in% secchi[ ,paste(DOW,YEAR, sep = "_") ,])
  
  #how many surveys have a secchi for that month?
  summary(plants[ , .N ,.(DOW,YEAR,MONTH) ][,paste(paste(DOW,YEAR, sep = "_"),MONTH, sep = "_"),] %in% 
            secchi[ ,paste(paste(DOW,YEAR, sep = "_"),MONTH, sep = "_")  ,])
  
  
  
  plants[ , date := SURVEY_DATE]
  
  
  
  #consolidate to the DOW-Date level
  secchi <- secchi[ , .("Secchi_m" = mean(Secchi_m))  , .(DOW, Date) ]
  secchi[ , SECCHI_DATE := Date]
  secchi <- secchi[!is.na(DOW)]
  
  plants <- secchi[plants,  , on = .(DOW, Date = date),  roll='nearest' ]
  
  #drop the Date field (now a dup of SURVEY_DATE)
  plants[ , Date := NULL]
  
  #how far apart are plant and secchi obs?
  hist(plants[,SURVEY_DATE-SECCHI_DATE,])
  
  #keep only Secchi obs within a month (date+/-30d)
  hist(plants[ , abs(yday(SECCHI_DATE) - yday(SURVEY_DATE)), ])
  
  plants[abs(yday(SECCHI_DATE) - yday(SURVEY_DATE))<30 &
           abs(year(SECCHI_DATE) - year(SURVEY_DATE))<1, SECCHI_m_ACCEPTED := Secchi_m ]

  #cleanup:
  rm(secchi)

#' ### Avg Multiple Secchi measurements for each obs:
#' Assign a Secchi to each observation based on a few rules (EDIT to fit needs):
#' 
#'    1. Use only Secchi from July-Sep
#'    2. Create a mean for that time pd. for each lake-year of secchi data
#'    3. Use summer means in same year (or +/- 1 year if no w/in year available) 
# an option for fuzzy temporal joins with averaging: (+/- 0 to 3yr) 
  
  # #add month to secchi
  # secchi[ , MONTH := month(Date)] 
  # 
  # # Survey data (survey information columns from plants)
  # surveys <- plants[ , .N, .(DOW, SURVEY_DATE, YEAR, MONTH, SURVEY_ID)]
  # 
  # # Joining secchi and surveys tables
  # surveys.secchi <- merge(surveys, secchi, by.x = "DOW", by.y = "DOW", allow.cartesian=TRUE)
  # 
  # # Subsetting to summer secchi readings
  # surveys.secchi.summer <- surveys.secchi[MONTH.y %in% 7:9]
  # 
  # # Same-year matches
  # surveys.secchi.summer.sameYr <- surveys.secchi.summer[YEAR.x == YEAR.y]
  # 
  # # +/- 1-year matches
  # surveys.secchi.summer.within1Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-1, YEAR.y+1)]
  # 
  # # +/- 2-year matches
  # surveys.secchi.summer.within2Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-2, YEAR.y+2)]
  # 
  # # +/- 3-year matches
  # surveys.secchi.summer.within3Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-3, YEAR.y+3)]
  # 
  # matching.summ <- 
  #   data.frame(Year.diff = c(0, 1, 2, 3), 
  #              Matched.surveys = c(length(unique(surveys.secchi.summer.sameYr$SURVEY_ID)),
  #                                  length(unique(surveys.secchi.summer.within1Yr$SURVEY_ID)),
  #                                  length(unique(surveys.secchi.summer.within2Yr$SURVEY_ID)),
  #                                  length(unique(surveys.secchi.summer.within3Yr$SURVEY_ID))),
  #              Total.surveys = dim(surveys)[1]
  #   )
  # 
  # (matching.summ <- transform(matching.summ, Prop.matched = round(Matched.surveys/Total.surveys, digits = 3)))
  # 
  # 
  # # choose a timeframe for join
  # # Secchi data to use: +/- 1-year matches w/ multiple secchi readings averaged
  # surveys.secchi.final <- surveys.secchi.summer.within1Yr
  # surveys.secchi.final[, Source := NULL]
  # setnames(surveys.secchi.final, old = c("YEAR.x", "YEAR.y", "MONTH.x", "Date", "MONTH.y"), 
  #          new = c("YEAR.SURVEY", "YEAR.SECCHI", "MONTH.SURVEY", "DATE.SECCHI", "MONTH.SECCHI"), skip_absent=TRUE)  
  # 
  # 
  # # calculate metrics for selected data 
  # 
  # # calc useable values and sum stats from each secchi set used by each survey linking to them
  # surveys.secchi.final <- surveys.secchi.final[, .(Secchi_m.mean=mean(Secchi_m), YEAR.SECCHI.mean=mean(YEAR.SECCHI), MONTH.SECCHI.mean=mean(MONTH.SECCHI),
  #                                                  Secchi_m.min=min(Secchi_m), YEAR.SECCHI.min=min(YEAR.SECCHI), MONTH.SECCHI.min=min(MONTH.SECCHI),
  #                                                  Secchi_m.max=max(Secchi_m), YEAR.SECCHI.max=max(YEAR.SECCHI), MONTH.SECCHI.max=max(MONTH.SECCHI),
  #                                                  Secchi_m.sd=sd(Secchi_m), Secchi_m.length=length(Secchi_m)), 
  #                                              .(DOW, SURVEY_DATE, YEAR.SURVEY, MONTH.SURVEY, SURVEY_ID)]  
  # surveys.secchi.final[, Secchi_m.se := Secchi_m.sd/sqrt(Secchi_m.sd)]
  # surveys.secchi.final[Secchi_m.se == "NaN", Secchi_m.se := 0]
  # 
  # surveys.secchi.final[, .(Secchi_m.length) ]
  # surveys.secchi.final[, .(Secchi_m.sd) ]
  # 
  # 
  # 
  # # link these to the plants dataset 
  # 
  # # merge by lake and date
  # plants <- merge(plants,surveys.secchi.final, by = c("SURVEY_ID","DOW", "SURVEY_DATE"), all.x = T)
  # 
  # #cleanup intermediate products:
  # 
  # rm(secchi,surveys,surveys.secchi, surveys.secchi.summer, surveys.secchi.summer.sameYr, surveys.secchi.final, surveys.secchi.summer.within1Yr, surveys.secchi.summer.within2Yr, surveys.secchi.summer.within3Yr, matching.summ)
  # 

#' ### Calculate a stat of light availability at the point level  
  # calculate point level light avail 
  plants[ , proplight := exp(-(log(10)/SECCHI_m_ACCEPTED)*(DEPTH_FT/3.2804)) ]
  nrow(plants[!is.na(proplight) , .N , POINT_ID])/ #how many points can we do this for?
  nrow(plants[, .N , POINT_ID])#out of total n points
  plants[ ,hist(proplight, breaks = 100, main = "78% coverage for light availability")]


# plant status information ------------------------------------------------

  # native diversity must exclude invasives/introduced species
  rte <- clean_names(rte)
  
  plants[ , unique(TAXON) , ][!(plants[ , unique(TAXON) , ] %in% c(rte[native_status == "I", mn_dnr_scientific_name],
                                     "Nitellopsis", "Typha glauca"))]
  
  #we'll use this to select columns as we calculate diversty metrics!
  natcols <- plants[ , unique(TAXON) , ][!(plants[ , unique(TAXON) , ] %in% c(rte[native_status == "I", mn_dnr_scientific_name],
                                                                              "Nitellopsis", "Typha glauca"))]

 
# summarize plants dataset ---------------------------------------------------
#' ## Review Data Summaries
#' 
#' Review current data status and ouline changes needed for 
#' 
#' 
  str(plants) #what data formats?
  names(plants) #field names
  
  plants[ , length(unique(SURVEY_ID)) , ] #how many surveys in all?
  plants[ INDATABASE == T , length(unique(SURVEY_ID))] #how many surveys do we have the data in our db for?
  plants[ , length((unique(DOW))) , ] #how many lake in all?
  plants[ , length(unique(YEAR)) , ] #how many years of data?
  plants[ , length(unique(POINT_ID)),] #how samples pulled from the lake?
  plants[!is.na(TAXON) , length(unique(OBS_ID))] # how many times was a plant identified in these data? 


#' Lets see how many surveys (then number of points) we have been given by each contributor:
  
  plants[ , unique(SURVEY_DATASOURCE) ,] 
  
  # survey contribution viz
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

  missing_data_surveys <- plants[ INDATABASE == F] 
  plants <- plants[INDATABASE == T]
  
  
# drop zeros --------------------------------------------------------------
  
  #' We dropped surveys with no depth data in an early cleaning step. This
  #' happened before we merged datasets from the MN DNR into the database, meaning
  #' that we've still got to do a purge of 0 and NA depths to be sure we've
  #' handled DNR and other collaborator data consistently: 
  
  #any remaining points with depth == NA or 0? They need to be dropped to be consistent in the handling of all no depth sampled points (currently only MNDNR ):
  # plants[is.na(DEPTH_FT)|DEPTH_FT == 0 , ][ , .N , .(SURVEY_ID, DATASOURCE)]
  
  plants[is.na(DEPTH_FT), .N, .(SURVEY_ID, DATASOURCE)][, unique(DATASOURCE)] #these only still remain in the DNR data--thats because we did the DNR data merge after cleaning up the other data 
  
  sum(plants[ SURVEY_ID %in% plants[is.na(DEPTH_FT), .N, .(SURVEY_ID, DATASOURCE)][,SURVEY_ID], .N , .(SURVEY_ID, DATASOURCE)
  ][ , N]) #counts all points in those surveys
  
  #drop them:
  plants <- plants[!is.na(DEPTH_FT) ]
  

# duplicated entries ------------------------------------------------------

#' It's become aparrent to me that when we casted the data to long format in the
#'  survey collation project, we ended up with many cases of multiple
#'  "observations" of the same thing from within a single point. Here we clean
#'  up this issue. I found the cause of it by opening up the surveycollation 
#'  project  
  #drop duplicated entries:
  names(plants)
  
  plants[ , .N , .(SURVEY_ID,
                   POINT_ID ,
                   NO_VEG_FOUND ,
                   proplight ,
                   DEPTH_FT ,
                   SUBSTRATE ,
                   SURVEYOR, TAXON)  ][N>1 , hist(N) , ]
  
  sum(duplicated(plants$OBS_ID))
  
  names(plants[ , .SD , .SDcols = !c("OBS_ID") ])
  
  sum(duplicated(plants[ , .SD , .SDcols = !c("OBS_ID") ]))
  
  plants <- plants[!duplicated(plants[ , .SD , .SDcols = !c("OBS_ID") ]) , , ]
  
  # still a bunch of dups leftover where we've got:
  # two abunds for one species or two of something for one species...
  # with a littel sleuthing, I can see that these are a whole mix of things. For
  # example, James Johnson submitted one survey with two samples for point 213...
  # the solution I'll use is to allow these obs to stay (assuming that both obs 
  # are real, and the data entry resulted in a bad point ID for one of them).
  # because of this, when we agg to the point level, we'll have to choose an obs
  # to use that taxon. You'll see this play out in the species matrix
  # construction below:
  
  
  plants[ , .N , .(SURVEY_ID,
                   POINT_ID ,
                   NO_VEG_FOUND ,
                   proplight ,
                   DEPTH_FT ,
                   SUBSTRATE ,
                   SURVEYOR, TAXON)  ][N>1 , .N , ]
  plants[ , .N , .(SURVEY_ID,
                   POINT_ID ,
                   NO_VEG_FOUND ,
                   proplight ,
                   DEPTH_FT ,
                   SUBSTRATE ,
                   SURVEYOR, TAXON)  ][N>1 , unique(POINT_ID) , ]


#' ## The point observation dataset:
#' 
#' Now "plants" is only those surveys for which we were able to gather and
#' collate the data. Below we organize these data 3 ways: 
#' 
#' 1. As long format: each row is a species observation within a point (multiple
#' rows per point) including all fields retained through cleaning processes
#' 2. As a wide format of occurrences: each row is a point record, and the columns include a 
#' species observation (presence/absence) matrix. Here we keep only fields we 
#' 3. As a wide format with species abundances (a subset of "2.") where each row
#' is a point record , and the columns include a species abundance matrix

  #how many surveys and how many points were sampled in each?
  hist(plants[ ,length(unique(POINT_ID)) , SURVEY_ID]$V1, breaks= 100, main = "N points per survey", xlab = "Npoints")
  
  #how many unique TAXA?
  unique(plants$TAXON)
  # N taxa per survey:
  plants[ , .("Ntaxa" = length(unique(TAXON))) , SURVEY_ID] #if you want to name cols on the fly you need to wrap in .() which makes list from them 
  hist(plants[ , length(unique(TAXON)) , SURVEY_ID][ , V1], main = "N taxa per survey", xlab = "N taxa")
  hist(plants[ , length(unique(TAXON)) , POINT_ID][ , V1], main = "N taxa per point", xlab = "N taxa")
  
 
# point level p/a  -------------------------------------------------
  plants[ ,REL_ABUND] 
  
  plants_occurrence_wide <- dcast(plants, 
                                  SURVEY_ID+ 
                                    POINT_ID +
                                    NO_VEG_FOUND +
                                    proplight +
                                    DEPTH_FT + 
                                    SUBSTRATE +
                                    SURVEYOR ~ TAXON, fun.aggregate = last,  value.var = "INDATABASE", fill = 0)
  
  #diversity metrics (only have richness with p/a, no "evenness", no "diversity"):
  point_natcols <- names(plants_occurrence_wide)[names(plants_occurrence_wide)%in%natcols]
  names(plants_occurrence_wide)
  plants_occurrence_wide[ ,  richness := rowSums(.SD > 0), .SDcols = c(9:239) ]
  plants_occurrence_wide[ ,  nat_richness := rowSums(.SD > 0), .SDcols = point_natcols ]  
  
  #bring all survey level variables back into the dataset 
  #check join
  # nrow(plants[plants_occurrence_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ])
  # names(plants[plants_occurrence_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ])
  # 
  plants_occurrence_wide <- plants[plants_occurrence_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ]
  

  #and drop unneeded cols & those with loss of meaning through munging:
  plants_occurrence_wide[ , c("STA_NBR_DATASOURCE", "REL_ABUND", "REL_ABUND_CORRECTED", "WHOLE_RAKE_REL_ABUND", "SURVEY_ID_DATASOURCE", "SAMPLE_NOTES", "SURFACE_GROWTH", "POINT_LVL_SECCHI", "OLD_SURVEY_ID", "DATESURVEYSTART", "COHORT", "DATEINFO", "MONTH", "DAY", "YEAR", "INVENTORY_STAFF", "INVENTORY_STAFFDATE", "INVENTORY_NOTES", "USEABLE", "CLEANED", "INDATABASE", "SUBMISSION_STAFF", "SUBMISSION_STAFFDATE", "SUBMISSION_NOTES", "SURVEY_FEEDBACK", "DATASOURCE", "RAKE_SCALE_USED") := NULL , ]
  
  #check to make sure I didn't dump something critical:
  # sum(duplicated(plants_occurrence_wide))
  
# point level rake abund --------------------------------------------------
  
  plants_rakeabund_wide <- dcast(plants[!is.na(REL_ABUND_CORRECTED)], SURVEY_ID+ 
                                   POINT_ID +
                                   NO_VEG_FOUND +
                                   proplight +
                                   DEPTH_FT + 
                                   SUBSTRATE +
                                   SURVEYOR ~ TAXON, value.var = c("REL_ABUND_CORRECTED"), fun.aggregate = last, fill = 0)
  
  #calculate diversity metrics for each rake throw
  rake_natcols <- names(plants_rakeabund_wide)[names(plants_rakeabund_wide)%in%natcols]
  names(plants_rakeabund_wide)
  plants_rakeabund_wide[ , shannon_div := diversity(plants_rakeabund_wide[ , c(8:145)],index = "shannon") ]
  plants_rakeabund_wide[ , simpsons_div := diversity(plants_rakeabund_wide[ , c(8:145)],index = "invsimpson") ]
  plants_rakeabund_wide[ , shannon_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "shannon") ]
  plants_rakeabund_wide[ , simpsons_div_nat := diversity(plants_rakeabund_wide[,.SD, .SDcols = rake_natcols],index = "invsimpson") ]
  
  
  plants_rakeabund_wide[ ,  richness := rowSums(.SD > 0), .SDcols = c(8:145) ]
  plants_rakeabund_wide[ ,  nat_richness := rowSums(.SD > 0), .SDcols = rake_natcols ]
  
  summary(plants_rakeabund_wide$`Potamogeton crispus`)
  
  
  #bring all survey level variables back into the dataset 
  #check join
  # nrow(plants[plants_rakeabund_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ])
  # names(plants[plants_rakeabund_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ])
  # 
  plants_rakeabund_wide <- plants[plants_rakeabund_wide, on = .(POINT_ID, SURVEY_ID, NO_VEG_FOUND, proplight,DEPTH_FT,SUBSTRATE,SURVEYOR), mult = "last" , ]
  
  
  #and drop unneeded cols & those with loss of meaning through munging:
  plants_rakeabund_wide[ , c("STA_NBR_DATASOURCE", "REL_ABUND", "REL_ABUND_CORRECTED", "WHOLE_RAKE_REL_ABUND", "SURVEY_ID_DATASOURCE", "SAMPLE_NOTES", "SURFACE_GROWTH", "POINT_LVL_SECCHI", "OLD_SURVEY_ID", "DATESURVEYSTART", "COHORT", "DATEINFO", "MONTH", "DAY", "YEAR", "INVENTORY_STAFF", "INVENTORY_STAFFDATE", "INVENTORY_NOTES", "USEABLE", "CLEANED", "INDATABASE", "SUBMISSION_STAFF", "SUBMISSION_STAFFDATE", "SUBMISSION_NOTES", "SURVEY_FEEDBACK", "DATASOURCE", "RAKE_SCALE_USED") := NULL , ]
  

  
  #cleanup:
  rm(point_natcols,rake_natcols)
  
  #CHOP ME OUT!
#' That is a good intro to the structure of these data. Some things to keep in 
#' mind:
#'  - Each row is an observation of a species at a point. 
#'  - If there was no species observed, the row is a placeholder for that point,
#'  and that row will have NO_VEG_FOUND set to TRUE, but TAXON and density will 
#'  be blank
#'  - If there are no data for a survey, that row is a placeholder for the
#'  survey, and all of the point-level data will be NAs
#'  

  
  
  
  
  
  
  
  
  
  
  
  
#'  ## Survey Level Data

# survey level stats -------------------------------------------
  surveys <- plants[ , .(tot_n_samp = length(unique(POINT_ID)))  , SURVEY_ID ]
  
  #add richness to the surveys dataset
  surveys[  , taxa_richness := #take the "taxon count" and subtract one if the survey includes NAs (see next two lines)
              plants[ , length(unique(TAXON))   , SURVEY_ID ][ , V1]-# ("total richness", but counts NAs as a taxon) minus
              plants[ , ifelse(sum(is.na(TAXON))== 0, 0, 1), SURVEY_ID][,V1],]#  (each survey get a 0 if no NAs or a 1 if contains NA's)
  
  # extent of vegetation in survey (proportion vegetated)
  surveys <- merge(surveys,plants[!is.na(TAXON), .(n_points_vegetated=length(unique(POINT_ID))) , SURVEY_ID ], by = "SURVEY_ID", all.x = TRUE)[is.na(n_points_vegetated), n_points_vegetated := 0 ]
  surveys[ , prop_veg := n_points_vegetated/tot_n_samp ,]
  
  
  #create a plant observation matrix (species abund by survey)
  survey_species_matrix <- dcast(plants[!is.na(TAXON) , .("count" = length(unique(POINT_ID))) , .(SURVEY_ID,TAXON)], SURVEY_ID ~ TAXON, value.var = "count", fill = 0) #note that this line creates the matrix ONLY for surveys that had species observations (~70 surveys had no species observed)

  #diversity indicies:
  # species names:
  natcols <- names(survey_species_matrix)[names(survey_species_matrix) %in% natcols]
  
  # total diversity
  survey_species_matrix[ , shannon_div := diversity(survey_species_matrix[,2:232],index = "shannon") ]
  survey_species_matrix[ , simpsons_div := diversity(survey_species_matrix[,2:232],index = "invsimpson") ]
  
  # native diversity
  survey_species_matrix[ , shannon_div_nat := diversity(survey_species_matrix[,.SD, .SDcols = natcols],index = "shannon") ]
  survey_species_matrix[ , simpsons_div_nat := diversity(survey_species_matrix[,.SD, .SDcols = natcols],index = "invsimpson") ]
  survey_species_matrix[simpsons_div_nat == Inf, simpsons_div_nat := 0]
  
  # native richness
  survey_species_matrix[ ,  nat_richness := rowSums(survey_species_matrix[ , .SD, .SDcols = natcols] > 0), ]

  # depth stats
  # depth surveyed stats:
  surveys <- surveys[plants[ !is.na(DEPTH_FT), .("max_depth_surveyed" = max(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
  surveys <- surveys[plants[ !is.na(DEPTH_FT), .("min_depth_surveyed" = min(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
  surveys <- surveys[plants[ !is.na(DEPTH_FT), .("mean_depth_surveyed" = mean(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
  surveys <- surveys[plants[ !is.na(DEPTH_FT), .("median_depth_surveyed" = median(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
  surveys <- surveys[plants[ !is.na(DEPTH_FT), .("IQR_depth_surveyed" = IQR(DEPTH_FT)) , SURVEY_ID], on = "SURVEY_ID" , ]
  
  #vegetated depths data
  #max depth vegetated within survey:
  
  #some of these might warrant removal, depending on whats being done with the data
  plants[NO_VEG_FOUND == F & DEPTH_FT>50, length(POINT_ID) ,  .(DATASOURCE, DOW, SUBBASIN, DATESURVEYSTART, LAKE_NAME)]
  
  plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT)) , SURVEY_ID]
  surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("max_depth_vegetated" = max(DEPTH_FT, na.rm = T)) , SURVEY_ID] , by = "SURVEY_ID" , all.x =TRUE )
  #other depth vegetated stats:
  surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("min_depth_vegetated" = min(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
  surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("mean_depth_vegetated" = mean(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
  surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("median_depth_vegetated" = median(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )
  surveys <- merge( surveys , plants[ NO_VEG_FOUND == FALSE , .("IQR_depth_vegetated" = IQR(DEPTH_FT, na.rm = T)) , SURVEY_ID], by = "SURVEY_ID" , all.x =TRUE )


  # species matrix into survey data
  #species matrix for surveys
  surveys <- merge(surveys, survey_species_matrix, by = "SURVEY_ID", all.x = T)
  f_dowle3natozeros(surveys, names(survey_species_matrix)) #the merge incorrectly assigns NAs for non obs... here we replace those with 0s
  
  # check work:
  # summary(surveys[,1:17])
  
  #append survey data (basic data from plants db) to these
  # names(plants)
  surveys <- merge(plants[ , .("nobs" = .N) , .(SURVEY_ID, DATASOURCE, LAKE_NAME, DOW, DATESURVEYSTART, SUBBASIN, MULTIPARTSURVEY, order_ID) ],surveys,  by = "SURVEY_ID")
  # summary(surveys)
  names(surveys) <- gsub(" ", "_", gsub( "\\(", "_", gsub( "\\)", "_", names(surveys))))
  
  #get dates squared away in surveys
  surveys[ ,sort(unique(DATESURVEYSTART)),]
  surveys[ , DATESURVEYSTART := as.Date(DATESURVEYSTART, format = "%m/%d/%Y" ),]
  surveys[ , hist(month(DATESURVEYSTART)) ,]
  surveys[ , year := year(DATESURVEYSTART) , ]


  # secchi data metrics 
  # rescue the secchi data from the plants db for these surveys

  surveys[plants, Secchi_m := Secchi_m, on = "SURVEY_ID"]
  surveys[plants, Secchi_m_date := SECCHI_DATE, on = "SURVEY_ID"]

#merge in the geodata + lake data
  surveys <- pwi_l[surveys, on = .(order_ID), mult = "first" ]  
  
  
  # n samples within historical max depth
  
  surveys <- merge(surveys,plants[!is.na(TAXON), .(alltime_maxvegdep = max(DEPTH_FT)) , .(DOW, SUBBASIN) ], by = c("DOW", "SUBBASIN"), all.x = TRUE) [is.na(alltime_maxvegdep), alltime_maxvegdep := 0  ]
  summary(surveys$alltime_maxvegdep)
  surveys[ , hist(alltime_maxvegdep) , ]
  
  plants[!is.na(TAXON), "alltime_maxvegdep" := max(DEPTH_FT) , .(DOW, SUBBASIN) ][is.na(alltime_maxvegdep), alltime_maxvegdep := 0  ]
  
  
  
  # n_points within all time max vegetated depth
  surveys <- merge(surveys,plants[DEPTH_FT <= alltime_maxvegdep, .(alltime_maxvegdep_n_samp = length(unique(POINT_ID))) , SURVEY_ID ], by = "SURVEY_ID", all.x = TRUE)[is.na(alltime_maxvegdep_n_samp), alltime_maxvegdep_n_samp := 0 ]
  
# species pools -----------------------------------------------------------

#' We have super awesome species pool data because we've got species abunds
#' across multiple scales:
#' From the smallest (point-- plants_rakeabund_wide or plants_occurrence_wide)
#' scale we have a species abundance matrix that can be treated as a product of
#' the species pool above it (whole survey/lake), which we also have an
#' abundance matrix for! We can also move up to the landscape scale, building 
#' species abundance matricies by aggregating these next-lower-scale data.
#' 
#' For example, we can do as described above (compressing matricies to richness
#' for viz) and aggregate to the HUC-8 watershed level. 
#' 
#' 

  #first add the lake level richness from each survey to the point rake abund data
  plants_rakeabund_wide[ , surveyrichness := surveys[match(plants_rakeabund_wide[ ,SURVEY_ID ,], surveys[, SURVEY_ID, ]), taxa_richness] ,]
  
    ggplot( data = plants_rakeabund_wide,
            aes(jitter(surveyrichness), jitter(nat_richness)))+
      geom_point(alpha = .05)
  
  
  # now we need to create a watershed level species matrix:
  #check keys
  # pwi_l$order_ID
  # plants$order_ID
   
  plants[ , watershed := pwi_l[match(plants[ , order_ID ,],pwi_l[ , order_ID ,]), major , ],]
  
  # plants[ , length(unique(POINT_ID)) , watershed]
  
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
  
  plants_rakeabund_wide[ , watershed := plants[match(plants_rakeabund_wide[ ,SURVEY_ID ,], plants[, SURVEY_ID, ]), watershed] ,]
  
  plants_rakeabund_wide[ , watershedrichness := watershed_occurrence_wide[match(plants_rakeabund_wide[ ,watershed ,],    watershed_occurrence_wide[, watershed, ]), n_species] ,]
  
  # watershed richness as the predictor: 
  
  ggplot( data = plants_rakeabund_wide,
          aes(jitter(watershedrichness), jitter(nat_richness)))+
    geom_point()
  
  surveys[ , watershed := plants[match(surveys[ , SURVEY_ID ,], plants[, SURVEY_ID, ]), watershed] ,]
  surveys[ , watershedrichness := watershed_occurrence_wide[match(surveys[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), n_species] ,]
  
  surveys[is.na(watershedrichness), watershedrichness := 0]
  
  ggplot( data = surveys,
          aes(watershedrichness, nat_richness))+
    geom_point()+
    geom_smooth(method = "lm")+
    ylab("Survey Richness")+
    xlab("HUC-8 Watershed Richness")+
    theme_bw()

  # Get watershed Diversity

  names(watershed_occurrence_wide) %in% natcols
  
  watershed_occurrence_wide[ , simpson_div := diversity(.SD,index = "invsimpson" ) , .SDcols = c(names(watershed_occurrence_wide) %in% natcols)]
  
  hist(watershed_occurrence_wide$simpson_div)
  
  plants_rakeabund_wide[ , watershedsimpson := watershed_occurrence_wide[match(plants_rakeabund_wide[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), simpson_div] ,]
  
  plants_rakeabund_wide[ , surveysimpson := surveys[match(plants_rakeabund_wide[ ,SURVEY_ID ,], surveys[, SURVEY_ID, ]), simpsons_div_nat] ,]
  
  surveys[ , watershedsimpson := watershed_occurrence_wide[match(surveys[ ,watershed ,], watershed_occurrence_wide[, watershed, ]), simpson_div] ,]
  
  
  #redo species pool plots:
  point_pools <- ggplot( data = plants_rakeabund_wide[simpsons_div_nat != Inf],
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
  
   ggarrange(
     point_pools,
     lake_pools
   )
   
   
#clean up intermediates:
   
   rm(lake_pools, point_pools, survey_species_matrix)
   #have to strip off the geometry to prevent failure in write to csv
   surveys[ ,geometry := NULL ,]


# data products -----------------------------------------------------------


#' ## Export Data
#' 
#' We have 4 datasets to export: 
#' 
#'   
#' 
#' 1. plants --> plants_env_data.csv -- As long format: each row is a species observation within a point (multiple
#' rows per point) including all fields retained through cleaning processes
#' 
#' 2. plants_occurrence_wide --> plants_env_data_wide.csv -- As a wide format of occurrences: each row is a point record, and the columns include a 
#' species observation (presence/absence) matrix. Here we keep only fields we 
#' 
#' 3. plants_rakeabund_wide --> plants_abund_env_data_wide.csv -- As a wide format with species abundances (a subset of "2.") where each row
#' is a point record , and the columns include a species abundance matrix
#' 
#' 4. surveys --> surveys_aqplants.csv -- aggregated plants data at the survey level. Each row
#' is a set of survey-level summary stats and abundances (number of obs) for all
#' species in the dataset.   

  # fwrite(plants, file = "data&scripts/data/output/plants_env_data.csv")   
  # fwrite(plants_occurrence_wide, file = "data&scripts/data/output/plants_env_data_wide.csv")   
  # fwrite(plants_rakeabund_wide, file = "data&scripts/data/output/plants_abund_env_data_wide.csv")   
  # fwrite(surveys, file = "data&scripts/data/output/surveys_aqplants.csv")   
  # missing data surveys?
   
   
   save(pwi_l, rte, watersheds_huc8, watershed_occurrence_wide,  file = "synthesis_script_datasets.Rdata")  

   

# Figs for data pub -------------------------------------------------------


# map ---------------------------------------------------------------------

#plot with lakes shapes and point locs!! 
   
   # pwi_l[order_ID %in% unique(plants[ , order_ID]), ,]
   # 
   # ggplot(pwi_l[order_ID %in% plants[ , unique(order_ID) , ] , , ], aes(geometry=geometry)) +
   #   geom_sf() +
   #   labs(caption = "Map of lakes with surveys in our database")
   
   #Conversion of data frame to sf object to add points
   plants_pts <- st_as_sf(x = plants[!is.na(Longitude)],                         
                          coords = c("Longitude", "Latitude"),
                          crs = "+proj=lonlat +datum=WGS84")
   
   # #map points
   # ggplot(plants_pts, aes(geometry=geometry)) +
   #   geom_sf() +
   #   labs(caption = "Map of survey points in our database")
   
   #plot all together!
   
   #other data for fig
   # usa <- map_data("usa")
   # canada <- map_data("world", region = "canada")
   # states <- map_data("state")
   states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
   mn_df <- subset(states, ID == "minnesota")
   
   #Projection transformation
   plants_pts = st_transform(plants_pts, crs = "+proj=utm +zone=15")
   pwi_l <- st_sf(pwi_l)
   pwi_l <- st_transform(pwi_l, crs = st_crs(mn_df))
   setDT(pwi_l)
   
   watersheds_huc8 <- st_transform(watersheds_huc8, crs = st_crs(mn_df))
   
   
   
   #map
   study_map <- ggplot(data = pwi_l, aes(geometry=geometry))+
     geom_sf(data = watersheds_huc8,aes(geometry = geometry), alpha = .05, color = "gray")+
     geom_sf(alpha = .5,  color = "blue")+
     geom_sf(data = pwi_l[order_ID %in% plants[ , unique(order_ID) , ] , , ], aes(geometry=geometry), color = "red", alpha = 0.5)+
     geom_sf(data = mn_df,aes(geometry = geom), color = "black", alpha = .05)+
     scale_shape_discrete(solid = FALSE)+
     theme(text = element_text(size=20), legend.position = )+
     theme_bw()+
     ylab("Longitude")+
     xlab("Latitude")
   
   # study_map
   

  #  #add scalebar & North arrow 
  #  
  #  study_map+
  #    north(pwi_l, location = "topright", anchor = c("x"=-90, "y"= 49), scale = 0.2)
  #  
  #  
  #  #(https://stackoverflow.com/questions/39067838/parsimonious-way-to-add-north-arrow-and-scale-bar-to-ggmap):
  #  
  #  scalebar = function(x,y,w,n,d, units="Degrees"){
  #    # x,y = lower left coordinate of bar
  #    # w = width of bar
  #    # n = number of divisions on bar
  #    # d = distance along each division
  #    
  #    bar = data.frame( 
  #      xmin = seq(0.0, n*d, by=d) + x,
  #      xmax = seq(0.0, n*d, by=d) + x + d,
  #      ymin = y,
  #      ymax = y+w,
  #      z = rep(c(1,0),n)[1:(n+1)],
  #      fill.col = rep(c("black","white"),n)[1:(n+1)])
  #    
  #    labs = data.frame(
  #      xlab = c(seq(0.0, (n+1)*d, by=d) + x, x), 
  #      ylab = c(rep(y-w*1.5, n+2), y-3*w),
  #      text = c(as.character(seq(0.0, ((n+1)*d), by=d)), units)
  #    )
  #    list(bar, labs)
  #  }
  #  
  #  sb = scalebar( - 92.5, 49.1, .1, 2, .5 )
  #  
  #  # Plot map
  #  
  #  
  #  study_map+
  #    north(pwi_l, location = "topright", anchor = c("x"=-90, "y"= 49), scale = 0.2)+
  #    geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
  #              show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
  #    geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 
  #  
  #  
  # study_map
  #  
  #  #inset
  #  foo <- map_data("state")
  #  
  #  g2 <- ggplotGrob(
  #    ggplot() +
  #      geom_polygon(data = foo,
  #                   aes(x = long, y = lat, group = group),
  #                   fill = NA, color = "black", alpha = 0.5) +
  #      geom_polygon(data = mn_df,aes(x = long, y = lat), color = "black", alpha = .95)+
  #      theme(panel.background = element_rect(fill = NULL))+
  #      coord_map("polyconic")+
  #      theme_bw()+
  #      theme_inset()
  #    
  #  )     
  #  
  #  g3 <- study_map +
  #    annotation_custom(grob = g2, xmin = -92.5, xmax = -89.1,
  #                      ymin = 44.4, ymax = 46.1)
  #  
  #  
  #  sb = scalebar( - 91.7, 46.5, .1, 2, .5 )
  #  
  #  
  #  
  #  study_map <- ggplot(study_lakes)+
  #    geom_sf(data = clp_lakes, size = 3 ,shape = 1, alpha = 0.5)+
  #    geom_sf(size = 8, shape = "+", color= "black")+
  #    geom_polygon(data = mn_df,aes(x = long, y = lat), color = "black", alpha = .0)+
  #    scale_shape_discrete(solid = FALSE)+
  #    
  #    annotation_custom(grob = g2, xmin = -92.5, xmax = -89.1,
  #                      ymin = 44.4, ymax = 46.1)+
  #    north(study_lakes, location = "topright", anchor = c("x"=-89.3, "y"= 46.7), scale = 0.2)+
  #    geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
  #              show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
  #    geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F)+
  #    theme_bw()+
  #    theme(text = element_text(size=20), legend.position = )+
  #    ylab("Longitude")+
  #    xlab("Latitude")
  #  
  #  
   
   
   # figure construction -----------------------------------------------------
   #Figure 1
   # tiff("Fig1.tiff", res = 600, width = 9, height = 12, units = "in", compression = "lzw")
   # plot(study_map) # Make plot
   # dev.off()
   # 
   
   

# temporal accumulation ----------------------------------------------------

   # of surveys
   plants[ , length(unique(SURVEY_ID)), YEAR]
   
   #of obs
   plants[ , length(unique(OBS_ID)), YEAR]
   
   #taxa
   plants[!is.na(TAXON) , length(unique(TAXON)), YEAR]
   
   
   
   plotdat <- plants[ , first(SURVEY_DATE) , SURVEY_ID]
   
   setorder(plotdat, V1)
      plotdat[ , cumval := .I , ]
   
  temporal_accumulation <- ggplot(plotdat, aes(V1, cumval)) +
     geom_line()+
     theme_bw()+
     xlab("Year")+
     ylab("Cumulative Number of Surveys")
   
 #   plotdat[ , metric := "surveys" , ]
 #   
 #   plotdat_pts <- plants[ , first(SURVEY_DATE) , POINT_ID]
 #      setorder(plotdat_pts, V1)
 #      plotdat_pts[ , cumval := .I , ]
 #      plotdat_pts[ , metric := "points" , ]
 #      
 #   
 #   plotdat_taxa <- plants[ , first(SURVEY_DATE) , TAXON]
 #     setorder(plotdat_taxa, V1)
 #     plotdat_taxa[ , cumval := .I , ]
 #     plotdat_taxa[ , metric := "taxa" , ]
 #     
 #     
 # plotdat_all <- rbind(rbind(plotdat[ ,2:4 ], plotdat_pts[ ,2:4 ] ) , plotdat_taxa[ ,2:4 ])
 # 
 # ggplot(plotdat_all, aes(V1, cumval)) +
 #   geom_line()+
 #   facet_wrap(~ metric, scales = "free")
 # 
   
   #fig 1 
   
   # ggarrange(study_map, temporal_accumulation, nrow = 1)
   

# species abundance distributions -----------------------------------------

   plotdat <- plants[!is.na(TAXON) , .N , TAXON]
   setorder(plotdat, -N)
   plants[ , TAXON := factor(TAXON, levels = plotdat$TAXON)]
   
   plotdat[ , perc_abund := N/sum(plotdat$N) , ]
   
  ggplot(plants[!is.na(TAXON)], aes(TAXON))+
       geom_bar()+
    scale_y_log10()+
    theme(axis.text.x = element_blank())
     
  ggplot(plotdat[], aes(TAXON, N))+
    geom_point()+
    scale_y_log10()+
    theme(axis.text.x = element_text(angle = 45, face = "italic", hjust = 1))
  
  plotdat[ , TAXON := factor(TAXON, levels = plotdat$TAXON)]
  
  ggplot(plotdat[], aes(TAXON, perc_abund))+
    geom_point()+
    scale_y_log10()+
    xlab("Taxa")+
    ylab("log10(percent of all observations)")+
    theme_bw()+
    theme(axis.text.x = element_blank())





  # write.csv(plotdat, "data&scripts/data/output/species_abund_list.csv")

# lake traits distribution panel -------------------------------------------------

setDT(watersheds_huc8)
watersheds_huc8 <- watershed_occurrence_wide[watersheds_huc8, on = .(watershed = major) , ]

#Depth
nrow(plants_rakeabund_wide[!is.na(simpsons_div_nat)& !is.na(DEPTH_FT), ])
point_depth <- ggplot(plants_rakeabund_wide, aes(DEPTH_FT*0.348, simpsons_div_nat)) +
  geom_point()+
  scale_x_log10()+
  theme_bw()+
    ylab("ENSpie")+
  xlab("Water Depth (m)")

#Secchi
setDT(surveys)
nrow(surveys[!is.na(Secchi_m)])
lake_secchi <- ggplot(surveys[!is.na(Secchi_m)], aes(Secchi_m, simpsons_div_nat)) +
  geom_point()+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  xlab("Secchi Clarity (m)")


#area
wtrshd_area <- ggplot(watersheds_huc8, aes(acres, simpson_div )) +
  geom_point()+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  xlab("Area (ac)")
  

ggarrange(point_depth, lake_secchi, wtrshd_area, nrow = 1, labels = c("Point-scale N = 81906", "Lake-scale N = 2955", "Watershed-scale N = 68"))




# lake report data --------------------------------------------------------

  #who surveyed what sections
  plants[LAKE_NAME == "minnetonka", length(unique(SURVEY_ID)) , .(SURVEY_DATASOURCE, SUBBASIN, SURVEY_DATE)]
  
  #species list
  lakespp <- plants[!is.na(TAXON) & LAKE_NAME == "minnetonka", .N, TAXON]
    setorder(lakespp, -N)
    lakespp[ , TAXON := factor(TAXON,levels = lakespp$TAXON) , ]
    lakespp[ , logtaxaN := log(N)]
  ggplot(lakespp, aes(TAXON, N))+
    geom_bar(stat = "identity")+
    scale_y_log10()+
  
    theme(axis.text.x = element_text(angle = 90, face = "italic", vjust = .5))
  
  # richness over time -- UUUGLY
  
  ggplot(
    plants[LAKE_NAME == "minnetonka" & !is.na(TAXON), length(unique(TAXON)) , .(SURVEY_ID, SURVEY_DATE)],
    aes(SURVEY_DATE, V1))+
    geom_point()
  
  #Depth occurrences
  ggplot(
    plants[LAKE_NAME == "minnetonka" & !is.na(TAXON), ],
    aes(round(DEPTH_FT,0)))+
    geom_bar()
  
  #species list for deep locs
  
  plants[LAKE_NAME == "minnetonka" & !is.na(TAXON) & DEPTH_FT >20, max(DEPTH_FT), DATASOURCE ]
  
  lakespp <- plants[!is.na(TAXON) & LAKE_NAME == "minnetonka", .N, TAXON]
  setorder(lakespp, -N)
  lakespp[ , TAXON := factor(TAXON,levels = lakespp$TAXON) , ]
  lakespp[ , logtaxaN := log(N)]
  
  #Minnetonka SAD
  ggplot(lakespp, aes(TAXON, N))+
    geom_point()+
    scale_y_log10()+
    
    theme(axis.text.x = element_text(angle = 90, face = "italic", vjust = .5))

# lake shapefile for ATLAS, etc -------------------------------------------

plants[ ,  length(unique(TAXON)), .(DOW, SUBBASIN)]
plants[ ,  length(unique(SURVEY_DATE)), .(DOW, SUBBASIN)]
  
lakes <- plants[ ,  .("nyears" = length(unique(YEAR)),
                      "nsurveys" = length(unique(SURVEY_DATE)),
                      "surveylist" = toString(unique(SURVEY_DATE, na.rm = T)),
                      "ntaxa" = length(unique(TAXON, na.rm = T)),
                      "taxalist" = toString(unique(TAXON, na.rm = T)),
                      "surveyorlist" = toString(unique(SURVEYOR, na.rm = T))), .(DOW, LAKE_NAME ,SUBBASIN, order_ID)]


lakes[pwi_l, on = .(order_ID), geometry := geometry]
lakes[ ,surveylist := as.character(surveylist)]

lakes <- st_as_sf(lakes)
lakes <- st_cast(lakes, "MULTIPOLYGON")

ggplot(data = lakes, aes(geometry=geometry))+
  geom_sf(alpha = .5,  color = "blue")

summary(lakes)
str(pwi_l)

st_write(nc, "nc.shp")


st_crs(lakes)


# sf::st_write(lakes, "data/output/lakes_summ.csv")
# 
# saveRDS(lakes, "data&scripts/data/output/lakes_summ.rds")


head(plants[, lapply(.SD , function(x) toString(unique(TAXON))), by = .(DOW, LAKE_NAME ,SUBBASIN, order_ID)])

  a <- plants[, .(taxalist = toString(unique(TAXON, na.rm = T))), by = .(DOW, LAKE_NAME ,SUBBASIN)]

# workspace cleanup -------------------------------------------------------

  rm(lake_pools, lakespp, map, plants_pts, plotdat, point_pools)
  
  
  
  
# cursor catcher
