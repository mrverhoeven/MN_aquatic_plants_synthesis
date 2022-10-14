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
# 
# library(vegan)


# load in functions -------------------------------------------------------


# load in data -------------------------------------------------

#observation level data (uncomment if run directly)
# plants <- fread(input = "data&scripts/data/input/plant_surveys_mn.csv", drop = 1:2) #import, dropping the exported row numbers
# 
# #Secchi data
# secchi <- fread(input = "data&scripts/data/input/AllSecchi_plus_ShallowLakesSecchi.csv") #import, dropping the exported row numbers
# 
# #Public waters
# pwi_l <- st_read(dsn = "data&scripts/data/input/shp_water_dnr_hydrography", layer = "dnr_hydro_features_all")
# # pwi_r <- readOGR(dsn = "data&scripts/data/input/shp_water_mn_public_waters", layer = "public_waters_watercourses_delineations")
# 
# watersheds_huc4 <- st_read(dsn = "data&scripts/data/input/shp_geos_dnr_watersheds", layer = "dnr_watersheds_dnr_level_02_huc_04")
# 
# watersheds_huc8 <- st_read(dsn = "data&scripts/data/input/shp_geos_dnr_watersheds", layer = "dnr_watersheds_dnr_level_04_huc_08_majors")


# 
# #species statuses
# rte <- fread(input = "data&scripts/data/input/2013_dnr_plant_checklist_web.csv")




# merge geospatial files -------------------------------------------------------------

#change sf data.frame to a data.table
setDT(pwi_l)

# linking plants db to spatial reference:
plants[ , .N , DOW]
plants[ , summary(DOW), ]


#shapefile dows
pwi_l[ , dowlknum , ]
pwi_l[ , dow_main := round(as.numeric(dowlknum)/100,0)*100 , ]

pwi_l <- pwi_l[!is.na(dowlknum)]

pwi_l[  , order_ID:= .I , ]

pwi_l[duplicated(dowlknum)]

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


plants[is.na(order_ID) , .N ,  .(LAKE_NAME, DOW, DATASOURCE)]

plants[, summary(order_ID)]

# plants <- merge(plants, pwi_l[ ,  , ], by = "order_ID", all.x = T )


# fix up local geospatial info -------------------------------------------

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

#whatever the heck is leftover here is weeeeeird. (Blaming Andrea Prichard for that...)
plants[!is.na("X"), summary(X) ,]
plants[!is.na("Y"), summary(Y) ,]

# we need to delete these non-UTM vals form X & Y
plants[Y<4800000, .N, DATASOURCE ]
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
plants[!is.na(UTMX) & is.na(UTMY)]
plants[]

plants[!is.na(LATITUDE) & is.na(LONGITUDE)]
plants[is.na(LATITUDE) & !is.na(LONGITUDE)]

#Conversion of data frame to sf object
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




# plot with lakes shapes and point locs!! ---------------------------------------------------------


pwi_l[order_ID %in% unique(plants[ , order_ID]), ,]

# ggplot(pwi_l[order_ID %in% plants[ , unique(order_ID) , ] , , ], aes(geometry=geometry)) +
#   geom_sf() +
#   labs(caption = "Map of lakes with surveys in our database")

#Conversion of data frame to sf object to add points
plants_pts <- st_as_sf(x = plants[!is.na(Longitude)],                         
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=lonlat +datum=WGS84")

#Projection transformation
plants_pts = st_transform(plants_pts, crs = "+proj=utm +zone=15")

#map points
# ggplot(plants_pts, aes(geometry=geometry)) +
#   geom_sf() +
#   labs(caption = "Map of survey points in our database")

#plot all together!


      # map <- ggplot(data = pwi_l[order_ID %in% plants[ , unique(order_ID) , ] , , ], aes(geometry=geometry)) +
      #   geom_sf(alpha = .5) +
      #   labs(caption = "Map of survey points in our database")+
      #   geom_sf(data = plants_pts, aes(geometry=geometry))
      # 
      # # Customizing the output
      # tiff(filename = "Map.tiff",         # File name
      #      width = 8.5, height = 11, units = "in", # Width and height in inches
      #      # bg = "white",          # Background color
      #      # colormodel = "cmyk",   # Color model (cmyk is required for most publications)
      #      # paper = "A4"
      #      # pointsize = 12,
      #      # compression = c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"),
      #      res = 300, family = "", restoreConsole = TRUE,
      #      type =  "cairo"
      # )
      # 
      # # Creating a plot
      # map
      # 
      # # Closing the graphical device
      # dev.off()


# label all pwi_l with watershed names ------------------------------------

# st_join(pwi_l, watersheds_huc8)
# 
# st_crs(plants_UTMS) <- st_crs(watersheds_huc8)
# 
# plantsUTMS <- st_join(plants_UTMS, left = TRUE, watersheds_huc8)

pwi_l <- st_sf(pwi_l)
st_crs(pwi_l) <- st_crs(watersheds_huc8)
pwi_l <- st_join(pwi_l, left = TRUE, watersheds_huc8)

setDT(pwi_l)



rm(plants_pts, plants_UTMS, watersheds_huc8)
