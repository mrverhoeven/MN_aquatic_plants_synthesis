#'---
#' title: "Cleaning management data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

# !!!!!!!!!!!!!!!!!!!!!!!!CHECK DOWs in SUBBASIN MERGE to Survey DATA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# On 14 June, 2022 this script resulted in 1785 records for management actions (this line copied at footer)

# header ------------------------------------------------------------------

#' # Preamble
#' Load libraries
#+warning=FALSE, message=FALSE 
# library(data.table)
# library("googledrive")
# library(readxl)
# library(stringr)
# library(tidyr)


# load in data ------------------------------------------------------------

# read in files 

permits_IAPM <- fread("data&scripts/data/input/management_data/Kylie Cattoor IAPM data 20200402 (1).csv")
grants_EWM <- fread("data&scripts/data/input/management_data/DNR grant treatment dataEWM (1).csv")
grants_CLP <- fread("data&scripts/data/input/management_data/DNR grant treatment dataCLP (1).csv")
surveys2 <- fread("data&scripts/data/input/management_data/APM Survey Data to 2015 (4).csv")
surveys1 <- fread("data&scripts/data/input/management_data/APMsurveysMPARS.csv")


# review whats here: ------------------------------------------------------

# IAPM permits ------------------------------------------------------------


# permits_IAPM
# summary(permits_IAPM)
# str(permits_IAPM)

# #dates cleanup
# permits_IAPM[ , summary(effective_year)] #2016-2019
# permits_IAPM[issued_year == effective_year] # we dont need the issued date/data
permits_IAPM[ , c("issued_year", "issued_date") := NULL]
permits_IAPM[ , effective_date := as.Date(effective_date, "%m/%d/%Y") ]
# permits_IAPM[effective_year != as.integer(year(effective_date))]
permits_IAPM[ ,effective_year := NULL , ]

# #location info
# permits_IAPM[ , unique(watershed) ,]
# permits_IAPM[ , unique(resource), ]
# permits_IAPM[ resource == "(N/A)" , , ] # 38 permits contain NO loc information
permits_IAPM <- permits_IAPM[ !resource == "(N/A)" , , ]
# permits_IAPM[watershed == "(N/A)" , unique(resource) , watershed] #watershed data are incomplete. drop these
permits_IAPM[  , watershed := NULL  ,  ]
# permits_IAPM[ , resource] #we need to split name from DOWs
permits_IAPM[ , downum := gsub("[()]", "" , word(resource, -1) ), ] 
# permits_IAPM[ , downum , ]
permits_IAPM[ , lakename := gsub("\\s*\\([^\\)]+\\)", "", resource)]
# permits_IAPM[ , unique(lakename) ,]
permits_IAPM[ , resource := NULL  , ]

# #target species info
# permits_IAPM[ ,unique(species_groups), ]
# permits_IAPM[ ,unique(species) ]
# permits_IAPM[species %in% c("", "NULL")]
permits_IAPM[ , ewm_targeted := str_detect(species, "urasia") , ]
# permits_IAPM[ , summary(ewm_targeted)]
permits_IAPM[ , clp_targeted := str_detect(species, "urly") , ]

setcolorder(permits_IAPM, c("lakename", "downum", "effective_date", "ewm_targeted", "clp_targeted"))

#permit info
# permits_IAPM[ , unique(permit_used) ,]
permits_IAPM <- permits_IAPM[!permits_IAPM[ , str_detect(permit_used, "no") ,] ,  ,]
# permits_IAPM[ , .(unique(permit_type_id), unique(permit_type_name)) ,]
permits_IAPM[ , c("permit_type_id", "permit_type_name") := NULL , ]

# permits_IAPM[ , .N , product]
# permits_IAPM[ , .N , .(downum, year(effective_date))]

#' These are data from the IAPM program. They are permits targeting invasive
#' species. They have permit numbers and have DOW and Year to match up to other
#' datasets.
#' 
#' 


# grant program data ------------------------------------------------------

#' I went through this spreadsheet and split up any multi-DOW rows, splitting the treatment acreage equally among DOWs and copying all other trt info

# grants_EWM
# str(grants_EWM)
names(grants_EWM) <- gsub(" ", "", names(grants_EWM))

# names(grants_EWM)[str_detect(names(grants_EWM), "V")]
grants_EWM[ , names(grants_EWM)[str_detect(names(grants_EWM), "V")] := NULL , ]

#drop untreated
# grants_EWM[ , unique(Comments) , ][c(1,6,8,10,11,21,25,48,54,60,67,71,73,77,86,97,120,124,130)]
grants_EWM <- grants_EWM[!Comments %in% grants_EWM[ , unique(Comments) , ][c(1,6,8,10,11,21,25,48,54,60,67,71,73,77,86,97,120,124,130)] ]


#date
# grants_EWM[ , unique(`Treatmentdate`) , ]
grants_EWM[ , `Treatmentdate`:= as.Date(`Treatmentdate`, "%d-%b-%y" ) , ]


grants_EWM <- grants_EWM[!is.na(Treatmentdate), ] # this drops lots of blank rows and about 5 non-trts

#does "Year" contain something unique?
# grants_EWM[year(Treatmentdate) != Year]
grants_EWM[ ,Year := NULL ]

#method
# grants_EWM[ , unique(ProposedMethod), ]

# names(grants_EWM)

#species targeted:
# grants_EWM[ , unique(Species) , ]
grants_EWM[ , ewm_targeted := str_detect(Species, "urasia") , ]
# grants_EWM[ , summary(ewm_targeted)]
grants_EWM[ , clp_targeted := str_detect(Species, "urly") , ]
# grants_EWM[ , summary(clp_targeted)]

#downumbers
# grants_EWM[ , str(DOW) , ]

grants_EWM[ , DOW := as.integer(DOW*1000000)]
grants_EWM[ , DOWC := NULL]


setcolorder(grants_EWM, c("LakeName", "DOW", "Treatmentdate", "ewm_targeted", "clp_targeted", "Acrestreated", "ProposedMethod", "Treatmentmethod"))

# grants_EWM[ ewm_targeted ==T & clp_targeted ==T, ]

grants_EWM[ , Year := year(Treatmentdate)]

#now on to the CLP grant data

# grants_CLP

# str(grants_CLP)
names(grants_CLP) <- gsub(" ", "", names(grants_CLP))

#date
# grants_CLP[ , unique(`Treatmentdate`) , ]
grants_CLP[ , `Treatmentdate`:= as.Date(`Treatmentdate`, "%d-%b-%y" ) , ]

# grants_CLP[is.na(Treatmentdate) , ] # only have Year for these cats
# grants_CLP[ Year != year(Treatmentdate)] #order of use pref is Trt date, then Year where blank


#target species
#species targeted:
# grants_CLP[ , unique(Species) , ]
grants_CLP[ , ewm_targeted := str_detect(Species, "urasia") , ]
# grants_CLP[ , summary(ewm_targeted)]
grants_CLP[ , clp_targeted := str_detect(Species, "urly") , ]
# grants_CLP[ , summary(clp_targeted)]

#downumbers
# grants_CLP[ , str(DOW) , ]

# grants_CLP[, unique(DOW) , ]
grants_CLP[, DOW:= as.numeric(gsub("-",".", DOW))]

grants_CLP[ , DOW := as.integer(DOW*1000000)]

# names(grants_CLP)


setcolorder(grants_CLP, c("LakeName", "DOW", "Treatmentdate", "ewm_targeted", "clp_targeted", "Acrestreated", "ProposedMethod", "Treatmentmethod", "Year"))

# can we merge these two?
# cbind(names(grants_EWM),names(grants_CLP))

setcolorder(grants_CLP, names(grants_EWM))

grantsdata <- rbindlist(list(grants_EWM, grants_CLP))

rm(grants_CLP, grants_EWM)

#check for duplicates

# duplicated(grantsdata[ ,  ,])

grantsdata <- grantsdata[!duplicated(grantsdata[ ,  ,]), , ]


# survey data -------------------------------------------------------------

# surveys1
#' These are data from the "annual survey" reports. Has Permit numbers to join on.
#' These should tell us when folks did not use permit in a more reliable way.

# surveys2
#' These data cover everything pre-2016 and are from the end of year survey 
#' data. In 2012 the DNR started coding permits as "YY"F for fisheries permits,
#' which are near-shore, small scale permits, and "YY"W for Ecological and Water
#' resources permits, which should be our invasive species permits. 

#' overall these babes are a hot mess

# surveys1[ , unique(permit_type)]
#only want IAPM permits:
# surveys1[permit_type == "Invasive Aquatic Plant Management" , unique(permit_number) ,]
# looks like these survey data only have '16 & '17 in them. 
IAPMsurveys <- surveys1[permit_type == "Invasive Aquatic Plant Management" , ,]
# IAPMsurveys[, unique(permit_number) , .(calendar_year ,resource_number)]

# match(permits_IAPM$permit_number, IAPMsurveys$permit_number)

#' No matches for 2018/2019 permits... Okay. Why? Because we've only got survey data for
#' 2016 & 2017. We have some data for 2018/2019, but no acreages... We could
#' potentially retrieve those data from the MNDNR APM 
#' 
#' In the surveys data we have poopy, super wide data, wherin each chem got it's
#' own column. So let's start by greasing the survey data into the right format.


# clean and org the IAPM records from survey data --------------------

rm(surveys1)
# names(IAPMsurveys)

IAPMsurveys <- IAPMsurveys[ , lapply(.SD, as.character),]


IAPMsurveys <- melt(IAPMsurveys, id.vars = c(1:39), variable.name = "herbicide", value.name = "qty")

# names(IAPMsurveys)
# delete the aapcd lines (i think these are weed rollers?) because they're all blanks
IAPMsurveys[ ,c(34:39):=NULL ,]

# now move work dates to "work done" columns
# names(IAPMsurveys)

# test melt and verify that month is spelled out in the date cols
# summary(melt(IAPMsurveys, measure.vars = c(26:33), value.name = "workdate")[ , as.factor(workdate),])
#rather than melt, lets collapse these to allow multi-month work done 
IAPMsurveys <- unite(IAPMsurveys, col = "workdate", c(26:33), sep = "," )
#dump extra commas:
IAPMsurveys[ , workdate := gsub("^,*|(?<=,),|,*$", "", workdate, perl=T), ]

#we can see 2+ records for some entries.
# IAPMsurveys[ ,.N, permit_number]
# IAPMsurveys[permit_number == "2016-1930", ]
# IAPMsurveys[permit_number == "16W-3A066", ]
#we need to drop cases like ^^ where the herbicide collation process left us with 80 rows
IAPMsurveys[qty == "0", qty := NA]
herbicedusetbl <- IAPMsurveys[is.na(qty)==F , .(permit_id, permit_number,resource_name,resource_number,calendar_year,apm_annual_survey_type_name, landowner_name, commercial_applicator_name, total_treated_area_acres, herbicide, qty), ]

mgmttbl <- IAPMsurveys[ , !c("herbicide", "qty")]
mgmttbl <- mgmttbl[duplicated(mgmttbl) == F, , ] 

mgmtdat <- merge(mgmttbl,herbicedusetbl, all = T)

# mgmtdat[ , .N , .(resource_name, resource_number, calendar_year)]

#lake sarah and rush lake are missing name/dow:
# mgmtdat[ resource_name == "",  ]
# mgmtdat[ permit_number == "16W-3A080", .(resource_name, resource_number) ]
mgmtdat[ permit_number == "16W-3A080", c("resource_name" , "resource_number") := .("Lake Sarah", "27019100") ]
mgmtdat[ permit_number == "16W-3B037", c("resource_name" , "resource_number") := .("Rush", "13006900") ]

# mgmtdat[ , .N , .(resource_name, resource_number, calendar_year)]
# sum(complete.cases(mgmtdat[ , .N , .(resource_name, resource_number, calendar_year)])==T)

#drop a few unneeded cols (blank, mimally complied with, etc.:
mgmtdat[ , c("data_date_time", "satisified_with_swim_itch_control", "when_permit_expires", "control_methods_used", "satisfied_with_work_completed", "issued_date", "effective_date", "expiration_date") := NULL]

# mgmtdat

rm(IAPMsurveys,herbicedusetbl, mgmttbl)

#old surveys

surveys2 <- surveys2[ str_detect(PERMIT, "W")]
# surveys2[ , unique(PERMIT) ,]
surveys2[ , year := paste("20", gsub("W","", word(PERMIT, 1L, sep = fixed("-"))), sep = fixed(""))]

# names(surveys2)


setcolorder(surveys2, c("PERMIT", "year", "USED", "april", "may", "june", "july", "august", "september", "COM_DATE", "COMACREC", "COMACRM", "ACREAGE_AH", "COMPANY1","COMPANY2", "COUNTY", "LAKENAME", "Littora", "METHOD" ))

#reformat all herbicide cols to same data type (character)
herbcols <- names(surveys2)[c(20:76) ]
surveys2[ , (herbcols) := lapply(.SD, as.character) , .SDcols = herbcols ]
rm(herbcols)

surveys2 <- melt(surveys2, id.vars = c(1:19), variable.name = "herbicide", value.name = "qty", na.rm = T)

surveys2[ , workdate := paste(april, may, june, july, august, september, sep = fixed(","))]
surveys2[ , workdate := gsub("NA", "", workdate, perl=T), ]
surveys2[ , workdate := gsub("^,*|(?<=,),|,*$", "", workdate, perl=T), ]
# surveys2[, unique(workdate)]
# surveys2[ , length(unique(workdate)), .(PERMIT)][V1>1]
surveys2 <- surveys2[!(PERMIT == "15W-3B098" & USED == "")]
# surveys2[ , unique(PERMIT), ]

workdates <- surveys2[ , .("workdate" = unique(workdate)), .(PERMIT)]

# surveys2[ , .N , PERMIT][, N , ]
# surveys2[PERMIT == "15W-4032"][!is.na(qty)]
surveys2[qty == "" | qty == 0, qty := NA ]
surveys2 <- surveys2[!is.na(qty), ,]

surveys2 <- merge(surveys2,workdates, all = T)
rm(workdates)

#USED column
# surveys2[ , unique(USED)]
surveys2 <- surveys2[ !USED %in% c(NA, ""), ]

surveys2[ , c("april", "may", "june", "july", "august", "september") := NULL]

#has no DOW -- that was a huge waste of time...
# surveys2[ , unique(paste(COUNTY,LAKENAME)),]

names(surveys2) <- tolower(names(surveys2))

surveys2[ county == ""]
surveys2 <- surveys2[!county == ""]

# use county and lake to get DOWs for surveys 2 ---------------------------------------------

# need to pull in DOWs for these dirt-devils
# surveys2[ , .(year, county, lakename) ] 


county_name_key <- unique(pwi_l[ ,.(cty_name,pw_basin_n, dowlknum), ])
#check spelling:
# surveys2[ , sort(unique(tolower(county)))]
# county_name_key[ , sort(unique(tolower(cty_name))) , ]

# surveys2[ , sort(unique(tolower(county)))][!surveys2[ , sort(unique(tolower(county)))] %in% county_name_key[ , sort(unique(tolower(cty_name))) , ]]

county_name_key[ , cty_name := tolower(cty_name) ,]
surveys2[ , county:= tolower(county) ]

surveys2[county == "lesueur", county := "le sueur"]
surveys2[county == "ottertail", county := "otter tail"]
surveys2[county == "todd & stearns", county := "todd" ]

# consider merging to other data on lake & county (this is a smaller pool of candidates)
surveys2[ , unique(lakename) ]
surveys2[str_detect(lakename, "\x92"), lakename := c("Paul's(Florence)", "North Brown's") ]

surveys2[ , lakename := tolower(lakename)]
multilake <- surveys2[str_detect(lakename, pattern = fixed(","))]
surveys2 <- surveys2[!str_detect(lakename, pattern = fixed(","))]

multilake[lakename == "forest 1,2,3", lakename := "forest"]

# test fn()
# tstrsplit(multilake$lakename, ",", fixed = T, fill =  "<NA>", keep = 1 )
#execute
multilake[ , lake_1 := tstrsplit(lakename, ",", fixed = T, fill =  "<NA>", keep = 1 ), ]
multilake[ , lake_2 := tstrsplit(lakename, ",", fixed = T, fill =  "<NA>", keep = 2 ), ]
multilake[ , lake_3 := tstrsplit(lakename, ",", fixed = T, fill =  "<NA>", keep = 3 ), ]
multilake[ , lake_4 := tstrsplit(lakename, ",", fixed = T, fill =  "<NA>", keep = 4 ), ]

multilake <- melt(multilake, measure.vars = patterns("^lake_"))
multilake[ , lakename := value ,]
multilake[ ,  value := NULL, ] 
multilake[ ,  variable := NULL, ]

multilake <- multilake[!lakename == "<NA>", , ]

surveys2 <- rbind(surveys2,multilake)
rm(multilake)


#set up a lakename-county key for trying to add DOWs to these:
county_name_key[ , cty_code := round(as.numeric(dowlknum)/1000000,0) , ]

# county_name_key[ , length(unique(cty_name)) , cty_code ][V1>1]
# county_name_key[ cty_code %in% county_name_key[ , length(unique(cty_name)) , cty_code ][V1>1, cty_code], unique(cty_name), cty_code ]

county_name_key <- county_name_key[!cty_name == "not in mn"]
county_name_key[cty_name == "olmstead", cty_name := "olmsted"]
county_name_key[cty_code == 79, cty_name := "wabasha"]
county_name_key[cty_code == 6, cty_name := "big stone"]
county_name_key[cty_code == 49, cty_name := "morrison"]
county_name_key[cty_code == 21, cty_name := "douglas"]
county_name_key[cty_code == 1, cty_name := "aitkin"]

county_code_key <- county_name_key[ , unique(cty_name) , cty_code] #87 codes, 87 counties


permits_IAPM[ , county := county_code_key[match(permits_IAPM[ , round(as.numeric(downum)/1000000,0) , ], county_code_key[,cty_code]), V1], ]

permits_IAPM[ , year := year(effective_date) ,]

# permits_IAPM[ , .(year, lakename, county) ,]

# grantsdata[ , .(Year, LakeName, County, DOW) , ]

# county_name_key[ ,paste(pw_basin_n, cty_name)][county_name_key[ ,paste(pw_basin_n, cty_name)] %in% surveys2[ , paste(lakename, county) , ]]

#need to review each of these to see which DOW to assign

# permits_IAPM[ , .(lakename, county, downum) , ]
# surveys2[ , .(lakename, county) , ]
# permits_IAPM[ , .(lakename, county) , ]

permits_IAPM[ , lakename := tolower(lakename) , ]

# surveys2[ ,lakename, ]

surveys2[ , lakename := trimws(lakename, which = "both") , ]
surveys2[ , lakename := gsub(" ","_",
                             gsub("-","_",
                                  gsub("\\(", "_",gsub("\\)", "",
                                                       gsub("\\/", "_",
                                                            gsub("\\'","",
                                                                 gsub("’", "", lakename))))))) , ]
# surveys2[str_detect(lakename, "__")]

surveys2[lakename == "silver__nsp", downum := 62000100]
surveys2[lakename == "silver__nsp", lakename := "silver"]

surveys2[lakename == "sylvia__west_twin", downum := 86027900]
surveys2[lakename == "sylvia__west_twin", lakename := "west_lake_sylvia"]

surveys2[lakename == "paul’s__florence", downum := 30003500]
surveys2[lakename == "paul’s__florence", lakename := "florence"]

grantsdata[str_detect(LakeName, "aul"), LakeName := "Florence"]

# surveys2[ , sort(unique(lakename)) , ]

county_name_key[ , pw_basin_n := gsub(" ", "_", tolower(pw_basin_n)) ,  ]

# county_name_key[ , sort(unique(pw_basin_n)) ,]

# surveys2[ , sort(unique(lakename)) , ][ !surveys2[ , sort(unique(lakename)) , ]%in%county_name_key[ , sort(unique(pw_basin_n)) ,]]

#buck_little pelican
# surveys2[lakename == "buck_little_pelican", ]
surveys2 <- rbind(surveys2,surveys2[lakename == "buck_little_pelican", ])
surveys2[lakename == "buck_little_pelican", lakename := c("bucks", "little_pelican") ]


surveys2[lakename == "11th_crow_wing", lakename := "eleventh_crow_wing"]

surveys2[lakename == "center_north", lakename := "north_center" ]
surveys2[lakename == "center_south", lakename := "south_center" ]

surveys2[lakename == "comfort_big" , lakename := "comfort" ,]

surveys2[lakename %in% c("elin","pauls" , "florence"), downum := 30003500 ,]
surveys2[lakename %in% c("elin","pauls" , "florence"), lakename := "florence" ]

surveys2[str_detect(lakename, "okega"), lakename:= "pokegama" ,]

surveys2[lakename == "green_big", downum := 13004100 ,]
surveys2[lakename == "green_big", lakename := "green" ,]

surveys2[lakename == "koetter", downum := 73013303 ,]
surveys2[lakename == "koetter", lakename := "cedar_island_koetter" ,]
surveys2[lakename == "little_cedar_island", downum := 73013305 ,]
surveys2[lakename == "little_cedar_island",lakename := "cedar_island_little" ,]

surveys2[lakename == "lindstrom_south", lakename := "south_lindstrom" ,]

surveys2[lakename == "lower_south_long", downum := 18013600 ,]
surveys2[lakename == "lower_south_long", lakename := "south_long" ,]

# surveys2[lakename == "mink_somers",  ,]
surveys2 <- rbind(surveys2,surveys2[lakename == "mink_somers", ])
surveys2[lakename == "mink_somers", downum := c(86023000, 86022900) ]
surveys2[lakename == "mink_somers", lakename := c("somers", "mink") ]

surveys2[lakename == "minnie_belle", downum := 47011900 ,]

#fix the dashes in county name key
county_name_key[ , pw_basin_n := gsub("-", "_", pw_basin_n)]
county_name_key[ , pw_basin_n := gsub("'", "", pw_basin_n)]

surveys2[lakename == "n_anderson", downum := 27006201 ,]
surveys2[lakename == "n_anderson", lakename := "north_anderson",]
surveys2[ , lakename := gsub("__", "_", lakename) ]

# surveys2[ , sort(unique(lakename)) , ][ !surveys2[ , sort(unique(lakename)) , ]%in%county_name_key[ , sort(unique(pw_basin_n)) ,]
# ]

surveys2[lakename == "oehrlines", downum := 62001400 ,]

surveys2[lakename == "pauls_florence", downum := 30003500 ,]
surveys2[lakename == "pauls_florence", lakename := "florence" ]

surveys2 <- rbind(surveys2[!lakename == "sakatah", ],surveys2[lakename == "sakatah", ][1,])

surveys2[lakename == "sakatah", lakename:= "upper_sakatah" ]

surveys2[lakename == "se_anderson", downum := 27006202 ,]
surveys2[lakename == "se_anderson", lakename := "southeast_anderson",]

surveys2[lakename == "upper_cormorant_lake", lakename:= "upper_cormorant" ]

surveys2[lakename == "washington_meeker", lakename:= "washington" ]

surveys2 <- surveys2[!lakename %in% c("various", "pelican_river_watershed") ]

surveys2[ lakename == "zumbra" , lakename := "zumbra_sunny"  , ]

surveys2[ str_detect(lakename, "minnetonka"), subbasin := word(lakename, start = -1, sep = fixed("_")  ) , ]

surveys2[ str_detect(lakename, "minnetonka"), lakename := "minnetonka" , ]

#here we have to edit the code to only join 1:1
#get a unique list of county - name combos from surveys 2 this is the set we need matches for:
unique(surveys2[ , .(county, lakename) ,])

county_name_key[ , .N ,.(cty_name, pw_basin_n, dowlknum) ][N>1] #this says when we have DOW in the mix, we've got a clean key

county_name_key[ , .N ,.(cty_name, pw_basin_n) ][N>1]# these are the cases where mult dows for a lakename in a county

county_name_key[ , n_occ_cty := .N ,.(cty_name, pw_basin_n) ]

county_name_key[n_occ_cty < 2 ,.N ,.(cty_name, pw_basin_n) ][N>1] # now the county name key is only for clean county-name keys for DOW


county_name_key[n_occ_cty < 2][surveys2,,  on = .(pw_basin_n = lakename, cty_name = county), nomatch = 0  ]

# execute assign DOWs based on the clean county-name key set:
surveys2[county_name_key[n_occ_cty < 2] , dowlknum := dowlknum , on = .(lakename = pw_basin_n, county = cty_name), ]

#deprecated
# surveys2 <- merge(surveys2, county_name_key, by.x = c("lakename", "county"), by.y = c("pw_basin_n","cty_name"), all.x = T)

names(surveys2)

surveys2[!is.na(downum) & !is.na(dowlknum)]

surveys2[ , dowlknum := as.integer(dowlknum) , ]
surveys2[is.na(downum), downum := dowlknum]

surveys2[is.na(downum), ]

surveys2[lakename == "big_swan",]
surveys2[lakename == "big_swan", county := "todd"]
surveys2[lakename == "big_swan", downum := 77002300]

surveys2[lakename == "bucks", lakename := "buck"]
surveys2[lakename == "buck", downum := 3047300]

surveys2[lakename == "east", lakename := "cedar_island_east" ]

surveys2[str_detect(lakename, "cedar_island") & used == "Yes, used", downum := 73013300]
surveys2[str_detect(lakename, "cedar_island") & used == "Yes, used", subbasin := word(lakename, -1, sep = "_")]
surveys2[str_detect(lakename, "cedar_island") & used == "Yes, used", lakename := word(lakename, 1,2, sep = "_")]

surveys2[str_detect(lakename, "marie"), county := "stearns"]
surveys2[str_detect(lakename, "marie"), downum := 73001400]

surveys2[str_detect(lakename, "mill"), downum := 3037700 ]
surveys2[str_detect(lakename, "mill"), lakename := "mill" ]

surveys2[str_detect(lakename, "platte"), county := "crow wing"  ]
surveys2[str_detect(lakename, "platte"), downum := 18008800  ]

surveys2[str_detect(lakename, "riley"), county := "carver"  ]
surveys2[str_detect(lakename, "riley"), downum := 10000200  ]


#' The rest of this list was cleaned up (dows assigned) based on a lookup of the
#' dow number in the lakefinder webpage + newer records in other datsets passed
#' to us. We assumed that if a more recent record included control for the same
#' lake x county and had only one dow for that, that that was the dow these old
#' records reflected. 

surveys2[is.na(downum) & lakename == "margaret", downum := 11022200 ]

surveys2[is.na(downum) & lakename == "round", downum := 18037300 ]

surveys2[is.na(downum) & lakename == "bass", downum := 27009800 ]

surveys2[is.na(downum) & lakename == "cedar", downum := 86022700 ]

surveys2[is.na(downum) & lakename == "clear" & county == "waseca", downum := 81001401 ]

surveys2[is.na(downum) & lakename == "clear" & county == "washington", downum := 82016300	 ]

surveys2[is.na(downum) & lakename == "diamond" , downum := 27012500	 ]

surveys2[is.na(downum) & lakename == "eagle" , downum := 71006700]

surveys2[is.na(downum) & lakename == "elk" , downum := 71005500]

surveys2[is.na(downum) & lakename == "little_elk" , downum := 71005500]

surveys2[is.na(downum) & lakename == "little_pelican" , downum := 56076100]

surveys2[is.na(downum) & lakename == "long" & county == "isanti" , downum := 30007200]

surveys2[is.na(downum) & lakename == "long" & county == "meeker" , downum := 47017700]

surveys2[is.na(downum) & lakename == "long" & county == "morrison" , downum := 49001500]

surveys2[is.na(downum) & lakename == "long" & county == "stearns" , downum := 73013900] # found this record in grants data

surveys2[is.na(downum) & lakename == "long" & county == "washington" , downum := 82011800]# found this record in grants data

surveys2[is.na(downum) & lakename == "madison" , downum := 07004400]

surveys2[is.na(downum) & lakename == "maple" , downum := 86013400]

surveys2[is.na(downum) & lakename == "mink" , downum := 86022900]

surveys2[is.na(downum) & lakename == "pickerel" , downum := 03028700]

surveys2[is.na(downum) & lakename == "sylvia" , downum := 86027900]

surveys2[is.na(downum), , ][order(lakename, county)]

surveys2[ , c("dowlknum") := NULL , ]

surveys2[ , qty := as.numeric(qty) ,]

surveys2 <- surveys2[qty > 0, ]

# surveys2[ , sort(unique(workdate))  ,]

# surveys2[ workdate == ""]

# surveys2[ workdate != "" & com_date != "" ,. (workdate, com_date) , ]

# surveys2[ workdate != "" & com_date != "" & workdate != com_date  , .(workdate, com_date) , ]

surveys2[ workdate == "", workdate := com_date ]

surveys2[ ,com_date := NULL ]

# #ballantyne throw out (why? it's an October treatment? we gonna keep it )
# surveys2 <- surveys2[ !permit == "12W-4031"]

surveys2[ ,unique(herbicide), ]
surveys2[ ,unique(workdate), ]
# 
# surveys2[str_detect(herbicide, "AQUATHOL") & !workdate == "7" , clp_targeted := T ]
# 
# surveys2[str_detect(herbicide, "riclop") & str_detect(workdate, "6")  , clp_targeted := T]
# 
# surveys2[str_detect(herbicide, "HYDRO") & !workdate == 10 , clp_targeted := T]
# 
# surveys2[ herbicide %in% c("HYDRO191", "G_HYDRO191")& !workdate == 10, clp_targeted := T]
# 
# surveys2[herbicide %in% c("Clearcast 2.7GLB"), clp_targeted := T]
# 
# unique(surveys2[is.na(clp_targeted), .(herbicide, workdate)])
# 
# surveys2 <- surveys2[!herbicide %in% c("Potassium Chloride lbs", "Earthtech QZ" )] #zebra mussel control
# 
# # surveys2[permit %in% c("15W-3B107", "12W-2B024", "15W-2B32", "15W-3A123") , , ]
# 
# surveys2 <- surveys2[!permit %in% c("15W-3B107", "12W-2B024", "15W-2B32", "15W-3A123") , , ] #we think these are flowring rush and starry stonewort
# 
# # surveys2[is.na(clp_targeted) & is.na(ewm_targeted), .N , .(herbicide, workdate) ]
# 
# surveys2 <- surveys2[!herbicide %in% c("Knockout")] # non-aquatic herb--unlikely to be EWM control 
# 
# surveys2 <- surveys2[!(lakename %in% c("detroit") & is.na(clp_targeted))]#these are probable flowering rush treatments
# 
# surveys2[is.na(clp_targeted) , .N , .(herbicide,workdate) ]
# 
# surveys2[is.na(clp_targeted) , ewm_targeted := T ]
# 
# surveys2[ , year := as.numeric(year) ,]
# 
# # surveys2[!is.na(acreage_ah) , .(comacrec, comacrm, acreage_ah) , ][is.na(comacrec)]
# 
# # names(surveys2)
# 
# surveys2 <- surveys2[ , .(lakename, subbasin, downum, year, permit, workdate,  comacrec, comacrm, method, herbicide, qty, clp_targeted, ewm_targeted) ,  ]
# 
# # surveys2[ , .N , .(year, lakename, subbasin, downum, clp_targeted, ewm_targeted)][N>1]
# 
# surveys2[, qty := round(qty, 2)]

# merge some stuff ---------------------------------------------------------

#' # Consolidate: 
#' We're after a table showing for each dow-subbasin X year how many acres of
#' control were implemented for each species. We can increase complexity later
#' to include dates, methods, etc. For now lets just grab 
#' 
#' dow-subbasin as row, yeari-species1ac, and yearispecies2ac for 2 * i cols 
#' 
#' To do this, we need DOW, subbasin, target species and acres managed. 

#prepnames & classes
names(grantsdata) <- tolower(names(grantsdata))


# we want a single row for each sp x trt (we need to duplicate the EWM = T and CLP = T rows)
grantsdata[ , .N , .(ewm_targeted, clp_targeted)]
grantsdata[ewm_targeted == T & clp_targeted ==T  , comments] #there's a good bit of data in the comments here

l_grantsdata_bothsp <- melt(grantsdata[ewm_targeted == T & clp_targeted ==T], measure.vars = c("ewm_targeted", "clp_targeted"))

l_grantsdata_onesp <- melt(grantsdata[!(ewm_targeted == T & clp_targeted ==T)], measure.vars = c("ewm_targeted", "clp_targeted"))

fwrite(l_grantsdata_bothsp, "data&scripts/data/output/l_grantsdata_bothsp.csv")

grantsdata_a <- fread("data&scripts/data/input/l_grantsdata_bothsp_cleaned.csv")

grantsdata_a[ , treatmentdate := as.Date(treatmentdate)]

grantsdata_l <-  rbind(l_grantsdata_onesp, grantsdata_a )

rm(grantsdata, l_grantsdata_bothsp, l_grantsdata_onesp, grantsdata_a)

sum(duplicated(grantsdata_l))

grantsdata_l <- grantsdata_l[!duplicated(grantsdata_l)]

grantsdata_l <- grantsdata_l[!value == F]

grantsdata_l[ ,  lakename:= gsub("Lake", "",
                                         gsub("lake", "", 
                                              gsub("lakes", "", 
                                                   gsub("Lakes", "" , lakename )
                                                   
                                              )
                                         )
)  , ]

grantsdata_l[ , lakename := gsub( " ", "", lakename) , ]

grantsdata_l <- grantsdata_l[!duplicated(grantsdata_l)]

grantsdata_l[ , .(N = .N) , .(dow,lakename,year,variable)][N>1]


# make treatment matrix from grants data
dcast(grantsdata_l, dow + lakename ~ year + variable,
      value.var = "acrestreated", fun.aggregate = mean, fill = 0)



# names(mgmtdat)
names(mgmtdat)[names(mgmtdat)  == "calendar_year"] <- "year"
names(mgmtdat)[names(mgmtdat)  == "resource_number"] <- "dow"

mgmtdat[ , unique(total_treated_area_acres,total_cut_area_acres)]

mgmtdat[ , total_treated_area_acres := as.numeric(gsub("ac", "",
                                                                gsub("Acres", "", 
                                                                     gsub("Ac", "",
                                                                          gsub("acres", "", 
                                                                               trimws(total_treated_area_acres, which = "both"))))))]

mgmtdat[ , total_cut_area_acres := as.numeric(gsub("ac", "",
                                                            gsub("Acres", "", 
                                                                 gsub("Ac", "",
                                                                      gsub("acres", "", 
                                                                           trimws(total_cut_area_acres, which = "both"))))))]

mgmtdat[ , controlacres := sum(total_treated_area_acres,total_cut_area_acres)]

#make the table from the mgmtdata
dcast(mgmtdat, dow + resource_name ~ year,
      value.var = "controlacres", fun.aggregate = mean, fill = 0)


permits_IAPM[, .N , .(ewm_targeted, clp_targeted) ]

permits_IAPM <- melt(permits_IAPM, measure.vars = c("ewm_targeted", "clp_targeted"))

permits_IAPM <- permits_IAPM[!value == F]

permits_IAPM[ , unique(product) ,]

#make the table from the IAPM data
dcast(permits_IAPM, downum + lakename ~ year + variable,
      value.var = "value", fill = 0)


surveys2[ , controlacres := sum(comacrec,comacrm)]


#make the table from the IAPM data
dcast(surveys2, downum + lakename + subbasin ~ year,
      value.var = "method", fun.aggregate = first, fill = 0)

# merge all for one big control matrix -------------------------------

simp_IAPM <- permits_IAPM[ , .(dow= as.integer(downum),
                               lakename,
                               year,
                               treatmentdate = as.character(effective_date),
                               species = variable) ,
                           ][!duplicated(permits_IAPM[ , .(downum, lakename, year, effective_date, variable) ,])]

simp_grants <- grantsdata_l[ , .(dow,
                                 lakename = tolower(lakename),
                                 year,
                                 treatmentdate = as.character(treatmentdate),
                                 species = variable,
                                 controlacres = acrestreated,
                                 method = paste(proposedmethod, treatmentmethod) ) ,
                             ][!duplicated(grantsdata_l[ , .(dow, lakename, year, variable, treatmentdate, acrestreated, method = paste(proposedmethod, treatmentmethod) )  ])]

simp_mgmtdat <- mgmtdat[ , .(dow= as.numeric(dow),
                             lakename = resource_name,
                             year,
                             treatmentdate = as.character(workdate),
                             controlacres) ,
                         ][!duplicated(mgmtdat[ , .(dow, resource_name, year, workdate, controlacres) , ])]

simp_survey <-  surveys2[ , .(dow = as.numeric(downum),
                              lakename,
                              subbasin,
                              year,
                              treatmentdate = as.character(workdate),
                              controlacres,
                              method = paste(method,herbicide)) , ][!duplicated(surveys2[ , .(downum, lakename, subbasin, year, workdate, controlacres, method = paste(method,herbicide)) , ])]






# lowest resolution: 0/1 trt ----------------------------------------------

s1_grants  <- simp_grants[   ,.(dow, lakename, year, treatmentdate)]
s1_IAPM    <- simp_IAPM[     ,.(dow, lakename, year, treatmentdate)]
s1_mgmtdat <- simp_mgmtdat[  ,.(dow, lakename, year, treatmentdate)] 
s1_survey  <- simp_survey[   ,.(dow, lakename = paste(lakename, subbasin, sep = "_"), year, treatmentdate) ] 

s1_mgmt <- rbind(s1_grants, s1_IAPM, s1_mgmtdat, s1_survey)

s1_mgmt[ ,sort(unique(lakename))]

s1_mgmt[ , lakename := tolower(gsub("_NA", "",
                                    gsub("-", "_",
                                         gsub(" ", "_",
                                              gsub("\\.", "", lakename)))))] 

s1_mgmt[lakename == "baldeagle", lakename := "bald_eagle"]
s1_mgmt[lakename == "cedarisland", lakename := "cedar_island"]
s1_mgmt[lakename == "crowwing", lakename := "crow_wing"]
s1_mgmt[lakename == "11thcrowwing", lakename := "eleventh_crow_wing"]
s1_mgmt[lakename == "fishtrap", lakename := "fish_trap"]
s1_mgmt[lakename %in%c("l'hommedieu(perry_langebay)", "l'hommedieu_perrylange"), lakename := "l'hommedieu_perrylangebay"]
s1_mgmt[lakename == "lake_sarah", lakename := "sarah"]
s1_mgmt[lakename == "le_homme_dieu", lakename := "l'hommedieu"]
s1_mgmt[lakename == "littlebirch", lakename := "little_birch"]
s1_mgmt[lakename == "littlecedar", lakename := "little_cedar"]
s1_mgmt[lakename == "littleelk", lakename := "little_elk"]
s1_mgmt[lakename == "littlepelican", lakename := "little_pelican"]
s1_mgmt[lakename == "littlepulaski", lakename := "little_pulaski"]
s1_mgmt[lakename == "lowercullen", lakename := "lower_cullen"]
s1_mgmt[lakename == "lowermission", lakename := "lower_mission"]
s1_mgmt[lakename == "lowerprior", lakename := "lower_prior"]
s1_mgmt[lakename == "lowertwin", lakename := "lower_twin"]
s1_mgmt[lakename == "middlecormorant", lakename := "middle_cormorant"]
s1_mgmt[lakename == "middlecullen", lakename := "middle_cullen"]
s1_mgmt[lakename == "middlespunk", lakename := "middle_spunk"]

surveys[str_detect(LAKE_NAME, "innetonk"), .(LAKE_NAME, SUBBASIN)]

s1_mgmt[lakename %in%c("minnetonka_alb", "minnetonka_stalbansbay"), lakename := "minnetonka_st_albans_bay"]
s1_mgmt[lakename %in%c("minnetonka_carmen'sbay", "minnetonka_carmens"), lakename := "minnetonka_carmens_bay"]
s1_mgmt[lakename %in%c("minnetonka_carson"), lakename := "minnetonka_carsons_bay"]
s1_mgmt[lakename %in%c("minnetonka_gideo", "minnetonka_gideons", "minnetonka_gideonsbay"), lakename := "minnetonka_gideons_bay"]
s1_mgmt[lakename %in%c("minnetonka_grays", "minnetonka_graysbay"), lakename := "minnetonka_grays_bay"]
s1_mgmt[lakename %in%c("minnetonka_maxw", "minnetonka_graysbay"), lakename := "minnetonka_maxwell_bay"]
s1_mgmt[lakename %in%c("minnetonka_north", "minnetonka_north_arm", "minnetonka_northarmbay"), lakename := "minnetonka_northarm_bay"]
s1_mgmt[lakename %in%c("minnetonka_phelps", "minnetonka_phelpsbay"), lakename := "minnetonka_phelps_bay"]
s1_mgmt[lakename == "minniebelle", lakename := "minnie_belle"]
s1_mgmt[lakename == "northbrown's", lakename := "north_browns"]
s1_mgmt[lakename == "northcenter", lakename := "north_center"]
s1_mgmt[lakename == "northlong", lakename := "north_long"]
s1_mgmt[lakename == "o'dowd", lakename := "odowd"]
s1_mgmt[lakename == "oftheisles", lakename := "lake_of_the_isles"]
s1_mgmt[lakename == "redrock", lakename := "red_rock"]
s1_mgmt[lakename == "sleepyeye", lakename := "sleepy_eye"]
s1_mgmt[lakename == "southcenter", lakename := "south_center"]
s1_mgmt[lakename == "southlindstrom", lakename := "south_lindstrom"]
s1_mgmt[lakename == "townline", lakename := "town_line"]
s1_mgmt[lakename == "uppercormorant", lakename := "upper_cormorant"]
s1_mgmt[lakename == "uppercullen", lakename := "upper_cullen"]
s1_mgmt[lakename == "uppermission", lakename := "upper_mission"]
s1_mgmt[lakename == "upperprior", lakename := "upper_prior"]
s1_mgmt[lakename == "uppersouthlong", lakename := "upper_south_long"]
s1_mgmt[lakename == "uppertwin", lakename := "upper_twin"]

s1_mgmt <- s1_mgmt[!duplicated(s1_mgmt),]


# without trt date: -------------------------------------------------------

s1_trtmatrix <- dcast(s1_mgmt, dow + lakename ~ year, value.var = "treatmentdate", fun.aggregate = length)


# work to keep trt date ---------------------------------------------------
#' this is a challenge and an effing mess
s1_mgmt[ , td := order(treatmentdate) , .(dow, lakename, year)]

s1_mgmt[ , unique(treatmentdate) ]

s1_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := paste(year, treatmentdate, sep = "-") , ]
s1_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := gsub("-20", "",
                                                gsub("000", "", treatmentdate)) , ]

s1_mgmt[is.na(treatmentdate), ]

s1_mgmt[treatmentdate == "", treatmentdate := NA ]


s1_mgmt_wdates <- dcast(s1_mgmt, dow + lakename + year ~ paste("date",td,sep = ""), value.var = "treatmentdate")

s1_mgmt_wdates[is.na(date1), date1 := date2 , ]
s1_mgmt_wdates[is.na(date1), date1 := date3]
s1_mgmt_wdates[is.na(date1), date1 := date4]
s1_mgmt_wdates[is.na(date1), date1 := date5]
s1_mgmt_wdates[is.na(date1), date1 := date6]
s1_mgmt_wdates[is.na(date1), date1 := date7]
s1_mgmt_wdates[is.na(date1), date1 := date8]
s1_mgmt_wdates[is.na(date1), date1 := date9]

s1_mgmt_wdates[ , .N , is.na(date1) ]

s1_trtmatrix_dates <- dcast(s1_mgmt_wdates, dow + lakename ~ year, value.var = "date1",)

# with target species -------------------------------------------------------------------------

s2_grants  <- simp_grants[   ,.(dow, lakename, year, treatmentdate, species)]
s2_IAPM    <- simp_IAPM[     ,.(dow, lakename, year, treatmentdate, species)]

s2_mgmt <- rbind(s2_grants, s2_IAPM)

s2_mgmt[ ,sort(unique(lakename))]

s2_mgmt[ , lakename := tolower(gsub("_NA", "",
                                    gsub("-", "_",
                                         gsub(" ", "_",
                                              gsub("\\.", "", lakename)))))] 

s2_mgmt[lakename == "baldeagle", lakename := "bald_eagle"]
s2_mgmt[lakename == "cedarisland", lakename := "cedar_island"]
s2_mgmt[lakename == "crowwing", lakename := "crow_wing"]
s2_mgmt[lakename == "11thcrowwing", lakename := "eleventh_crow_wing"]
s2_mgmt[lakename == "fishtrap", lakename := "fish_trap"]
s2_mgmt[lakename %in%c("l'hommedieu(perry_langebay)", "l'hommedieu_perrylange"), lakename := "l'hommedieu_perrylangebay"]
s2_mgmt[lakename == "lake_sarah", lakename := "sarah"]
s2_mgmt[lakename == "le_homme_dieu", lakename := "l'hommedieu"]
s2_mgmt[lakename == "littlebirch", lakename := "little_birch"]
s2_mgmt[lakename == "littlecedar", lakename := "little_cedar"]
s2_mgmt[lakename == "littleelk", lakename := "little_elk"]
s2_mgmt[lakename == "littlepelican", lakename := "little_pelican"]
s2_mgmt[lakename == "littlepulaski", lakename := "little_pulaski"]
s2_mgmt[lakename == "lowercullen", lakename := "lower_cullen"]
s2_mgmt[lakename == "lowermission", lakename := "lower_mission"]
s2_mgmt[lakename == "lowerprior", lakename := "lower_prior"]
s2_mgmt[lakename == "lowertwin", lakename := "lower_twin"]
s2_mgmt[lakename == "middlecormorant", lakename := "middle_cormorant"]
s2_mgmt[lakename == "middlecullen", lakename := "middle_cullen"]
s2_mgmt[lakename == "middlespunk", lakename := "middle_spunk"]

surveys[str_detect(LAKE_NAME, "innetonk"), .(LAKE_NAME, SUBBASIN)]

s2_mgmt[lakename %in%c("minnetonka_alb", "minnetonka_stalbansbay"), lakename := "minnetonka_st_albans_bay"]
s2_mgmt[lakename %in%c("minnetonka_carmen'sbay", "minnetonka_carmens"), lakename := "minnetonka_carmens_bay"]
s2_mgmt[lakename %in%c("minnetonka_carson"), lakename := "minnetonka_carsons_bay"]
s2_mgmt[lakename %in%c("minnetonka_gideo", "minnetonka_gideons", "minnetonka_gideonsbay"), lakename := "minnetonka_gideons_bay"]
s2_mgmt[lakename %in%c("minnetonka_grays", "minnetonka_graysbay"), lakename := "minnetonka_grays_bay"]
s2_mgmt[lakename %in%c("minnetonka_maxw", "minnetonka_graysbay"), lakename := "minnetonka_maxwell_bay"]
s2_mgmt[lakename %in%c("minnetonka_north", "minnetonka_north_arm", "minnetonka_northarmbay"), lakename := "minnetonka_northarm_bay"]
s2_mgmt[lakename %in%c("minnetonka_phelps", "minnetonka_phelpsbay"), lakename := "minnetonka_phelps_bay"]
s2_mgmt[lakename == "minniebelle", lakename := "minnie_belle"]
s2_mgmt[lakename == "northbrown's", lakename := "north_browns"]
s2_mgmt[lakename == "northcenter", lakename := "north_center"]
s2_mgmt[lakename == "northlong", lakename := "north_long"]
s2_mgmt[lakename == "o'dowd", lakename := "odowd"]
s2_mgmt[lakename == "oftheisles", lakename := "lake_of_the_isles"]
s2_mgmt[lakename == "redrock", lakename := "red_rock"]
s2_mgmt[lakename == "sleepyeye", lakename := "sleepy_eye"]
s2_mgmt[lakename == "southcenter", lakename := "south_center"]
s2_mgmt[lakename == "southlindstrom", lakename := "south_lindstrom"]
s2_mgmt[lakename == "townline", lakename := "town_line"]
s2_mgmt[lakename == "uppercormorant", lakename := "upper_cormorant"]
s2_mgmt[lakename == "uppercullen", lakename := "upper_cullen"]
s2_mgmt[lakename == "uppermission", lakename := "upper_mission"]
s2_mgmt[lakename == "upperprior", lakename := "upper_prior"]
s2_mgmt[lakename == "uppersouthlong", lakename := "upper_south_long"]
s2_mgmt[lakename == "uppertwin", lakename := "upper_twin"]

s2_mgmt <- s2_mgmt[!duplicated(s2_mgmt),]

# without trt date: -------------------------------------------------------

s2_trtmatrix <- dcast(s2_mgmt, dow + lakename ~ year + species, value.var = "treatmentdate")

#and now with dates
#' this is a challenge and an effing mess
s2_mgmt[ , td := order(treatmentdate) , .(dow, lakename, year, species)]

s2_mgmt[ , unique(treatmentdate) ]

s2_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := paste(year, treatmentdate, sep = "-") , ]
s2_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := gsub("-20", "",
                                                                 gsub("000", "", treatmentdate)) , ]

s2_mgmt[is.na(treatmentdate), ]

s2_mgmt[treatmentdate == "", treatmentdate := NA ]

s2_mgmt_wdates <- dcast(s2_mgmt, dow + lakename + year + species ~ paste("date",td,sep = ""), value.var = "treatmentdate")

s2_mgmt_wdates[is.na(date1), date1 := date2 , ]
s2_mgmt_wdates[is.na(date1), date1 := date3]
s2_mgmt_wdates[is.na(date1), date1 := date4]


s2_mgmt_wdates[ , .N , is.na(date1) ]

s2_trtmatrix_dates <- dcast(s2_mgmt_wdates, dow + lakename ~ year + species, value.var = "date1",)



# with acreages ------------------------------------------

s3_grants  <- simp_grants[   ,.(dow, lakename, year, treatmentdate, controlacres)]
s3_mgmtdat <- simp_mgmtdat[  ,.(dow, lakename, year, treatmentdate, controlacres)] 
s3_survey  <- simp_survey[   ,.(dow, lakename = paste(lakename, subbasin, sep = "_"), year, treatmentdate, controlacres) ] 

s3_mgmt <- rbind(s3_grants, s3_mgmtdat, s3_survey)

s3_mgmt[ ,sort(unique(lakename))]

s3_mgmt[ , lakename := tolower(gsub("_NA", "",
                                    gsub("-", "_",
                                         gsub(" ", "_",
                                              gsub("\\.", "", lakename)))))] 

s3_mgmt[lakename == "baldeagle", lakename := "bald_eagle"]
s3_mgmt[lakename == "cedarisland", lakename := "cedar_island"]
s3_mgmt[lakename == "crowwing", lakename := "crow_wing"]
s3_mgmt[lakename == "11thcrowwing", lakename := "eleventh_crow_wing"]
s3_mgmt[lakename == "fishtrap", lakename := "fish_trap"]
s3_mgmt[lakename %in%c("l'hommedieu(perry_langebay)", "l'hommedieu_perrylange"), lakename := "l'hommedieu_perrylangebay"]
s3_mgmt[lakename == "lake_sarah", lakename := "sarah"]
s3_mgmt[lakename == "le_homme_dieu", lakename := "l'hommedieu"]
s3_mgmt[lakename == "littlebirch", lakename := "little_birch"]
s3_mgmt[lakename == "littlecedar", lakename := "little_cedar"]
s3_mgmt[lakename == "littleelk", lakename := "little_elk"]
s3_mgmt[lakename == "littlepelican", lakename := "little_pelican"]
s3_mgmt[lakename == "littlepulaski", lakename := "little_pulaski"]
s3_mgmt[lakename == "lowercullen", lakename := "lower_cullen"]
s3_mgmt[lakename == "lowermission", lakename := "lower_mission"]
s3_mgmt[lakename == "lowerprior", lakename := "lower_prior"]
s3_mgmt[lakename == "lowertwin", lakename := "lower_twin"]
s3_mgmt[lakename == "middlecormorant", lakename := "middle_cormorant"]
s3_mgmt[lakename == "middlecullen", lakename := "middle_cullen"]
s3_mgmt[lakename == "middlespunk", lakename := "middle_spunk"]

surveys[str_detect(LAKE_NAME, "innetonk"), .(LAKE_NAME, SUBBASIN)]

s3_mgmt[lakename %in%c("minnetonka_alb", "minnetonka_stalbansbay"), lakename := "minnetonka_st_albans_bay"]
s3_mgmt[lakename %in%c("minnetonka_carmen'sbay", "minnetonka_carmens"), lakename := "minnetonka_carmens_bay"]
s3_mgmt[lakename %in%c("minnetonka_carson"), lakename := "minnetonka_carsons_bay"]
s3_mgmt[lakename %in%c("minnetonka_gideo", "minnetonka_gideons", "minnetonka_gideonsbay"), lakename := "minnetonka_gideons_bay"]
s3_mgmt[lakename %in%c("minnetonka_grays", "minnetonka_graysbay"), lakename := "minnetonka_grays_bay"]
s3_mgmt[lakename %in%c("minnetonka_maxw", "minnetonka_graysbay"), lakename := "minnetonka_maxwell_bay"]
s3_mgmt[lakename %in%c("minnetonka_north", "minnetonka_north_arm", "minnetonka_northarmbay"), lakename := "minnetonka_northarm_bay"]
s3_mgmt[lakename %in%c("minnetonka_phelps", "minnetonka_phelpsbay"), lakename := "minnetonka_phelps_bay"]
s3_mgmt[lakename == "minniebelle", lakename := "minnie_belle"]
s3_mgmt[lakename == "northbrown's", lakename := "north_browns"]
s3_mgmt[lakename == "northcenter", lakename := "north_center"]
s3_mgmt[lakename == "northlong", lakename := "north_long"]
s3_mgmt[lakename == "o'dowd", lakename := "odowd"]
s3_mgmt[lakename == "oftheisles", lakename := "lake_of_the_isles"]
s3_mgmt[lakename == "redrock", lakename := "red_rock"]
s3_mgmt[lakename == "sleepyeye", lakename := "sleepy_eye"]
s3_mgmt[lakename == "southcenter", lakename := "south_center"]
s3_mgmt[lakename == "southlindstrom", lakename := "south_lindstrom"]
s3_mgmt[lakename == "townline", lakename := "town_line"]
s3_mgmt[lakename == "uppercormorant", lakename := "upper_cormorant"]
s3_mgmt[lakename == "uppercullen", lakename := "upper_cullen"]
s3_mgmt[lakename == "uppermission", lakename := "upper_mission"]
s3_mgmt[lakename == "upperprior", lakename := "upper_prior"]
s3_mgmt[lakename == "uppersouthlong", lakename := "upper_south_long"]
s3_mgmt[lakename == "uppertwin", lakename := "upper_twin"]

s3_mgmt <- s3_mgmt[!duplicated(s3_mgmt),]


# without trt date: -------------------------------------------------------

s3_trtmatrix <- dcast(s3_mgmt, dow + lakename ~ year, value.var = "controlacres", fun.aggregate = function(x) mean(x, na.rm = T))


# work to keep trt date ---------------------------------------------------
#' this is a challenge and an effing mess
s3_mgmt[ , td := order(treatmentdate) , .(dow, lakename, year)]

s3_mgmt[ , unique(treatmentdate) ]

s3_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := paste(year, treatmentdate, sep = "-") , ]
s3_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := gsub("-20", "",
                                                                 gsub("000", "", treatmentdate)) , ]

s3_mgmt[is.na(treatmentdate), ]

s3_mgmt[treatmentdate == "", treatmentdate := NA ]


s3_mgmt_wdates <- dcast(s3_mgmt, dow + lakename + year ~ paste("date",td,sep = ""), value.var = "controlacres")

s3_mgmt_wdates[is.na(date1), date1 := date2 , ]
s3_mgmt_wdates[is.na(date1), date1 := date3]
s3_mgmt_wdates[is.na(date1), date1 := date4]
s3_mgmt_wdates[is.na(date1), date1 := date5]
s3_mgmt_wdates[is.na(date1), date1 := date6]


s3_mgmt_wdates[ , .N , is.na(date1) ]



# keep species psecific acreages ------------------------------------------

s4_grants  <- simp_grants[   ,.(dow, lakename, year, treatmentdate, species, controlacres)]

s4_mgmt <- rbind(s4_grants)

s4_mgmt[ ,sort(unique(lakename))]

s4_mgmt[ , lakename := tolower(gsub("_NA", "",
                                    gsub("-", "_",
                                         gsub(" ", "_",
                                              gsub("\\.", "", lakename)))))] 

s4_mgmt[lakename == "baldeagle", lakename := "bald_eagle"]
s4_mgmt[lakename == "cedarisland", lakename := "cedar_island"]
s4_mgmt[lakename == "crowwing", lakename := "crow_wing"]
s4_mgmt[lakename == "11thcrowwing", lakename := "eleventh_crow_wing"]
s4_mgmt[lakename == "fishtrap", lakename := "fish_trap"]
s4_mgmt[lakename %in%c("l'hommedieu(perry_langebay)", "l'hommedieu_perrylange"), lakename := "l'hommedieu_perrylangebay"]
s4_mgmt[lakename == "lake_sarah", lakename := "sarah"]
s4_mgmt[lakename == "le_homme_dieu", lakename := "l'hommedieu"]
s4_mgmt[lakename == "littlebirch", lakename := "little_birch"]
s4_mgmt[lakename == "littlecedar", lakename := "little_cedar"]
s4_mgmt[lakename == "littleelk", lakename := "little_elk"]
s4_mgmt[lakename == "littlepelican", lakename := "little_pelican"]
s4_mgmt[lakename == "littlepulaski", lakename := "little_pulaski"]
s4_mgmt[lakename == "lowercullen", lakename := "lower_cullen"]
s4_mgmt[lakename == "lowermission", lakename := "lower_mission"]
s4_mgmt[lakename == "lowerprior", lakename := "lower_prior"]
s4_mgmt[lakename == "lowertwin", lakename := "lower_twin"]
s4_mgmt[lakename == "middlecormorant", lakename := "middle_cormorant"]
s4_mgmt[lakename == "middlecullen", lakename := "middle_cullen"]
s4_mgmt[lakename == "middlespunk", lakename := "middle_spunk"]

surveys[str_detect(LAKE_NAME, "innetonk"), .(LAKE_NAME, SUBBASIN)]

s4_mgmt[lakename %in%c("minnetonka_alb", "minnetonka_stalbansbay"), lakename := "minnetonka_st_albans_bay"]
s4_mgmt[lakename %in%c("minnetonka_carmen'sbay", "minnetonka_carmens"), lakename := "minnetonka_carmens_bay"]
s4_mgmt[lakename %in%c("minnetonka_carson"), lakename := "minnetonka_carsons_bay"]
s4_mgmt[lakename %in%c("minnetonka_gideo", "minnetonka_gideons", "minnetonka_gideonsbay"), lakename := "minnetonka_gideons_bay"]
s4_mgmt[lakename %in%c("minnetonka_grays", "minnetonka_graysbay"), lakename := "minnetonka_grays_bay"]
s4_mgmt[lakename %in%c("minnetonka_maxw", "minnetonka_graysbay"), lakename := "minnetonka_maxwell_bay"]
s4_mgmt[lakename %in%c("minnetonka_north", "minnetonka_north_arm", "minnetonka_northarmbay"), lakename := "minnetonka_northarm_bay"]
s4_mgmt[lakename %in%c("minnetonka_phelps", "minnetonka_phelpsbay"), lakename := "minnetonka_phelps_bay"]
s4_mgmt[lakename == "minniebelle", lakename := "minnie_belle"]
s4_mgmt[lakename == "northbrown's", lakename := "north_browns"]
s4_mgmt[lakename == "northcenter", lakename := "north_center"]
s4_mgmt[lakename == "northlong", lakename := "north_long"]
s4_mgmt[lakename == "o'dowd", lakename := "odowd"]
s4_mgmt[lakename == "oftheisles", lakename := "lake_of_the_isles"]
s4_mgmt[lakename == "redrock", lakename := "red_rock"]
s4_mgmt[lakename == "sleepyeye", lakename := "sleepy_eye"]
s4_mgmt[lakename == "southcenter", lakename := "south_center"]
s4_mgmt[lakename == "southlindstrom", lakename := "south_lindstrom"]
s4_mgmt[lakename == "townline", lakename := "town_line"]
s4_mgmt[lakename == "uppercormorant", lakename := "upper_cormorant"]
s4_mgmt[lakename == "uppercullen", lakename := "upper_cullen"]
s4_mgmt[lakename == "uppermission", lakename := "upper_mission"]
s4_mgmt[lakename == "upperprior", lakename := "upper_prior"]
s4_mgmt[lakename == "uppersouthlong", lakename := "upper_south_long"]
s4_mgmt[lakename == "uppertwin", lakename := "upper_twin"]

s4_mgmt <- s4_mgmt[!duplicated(s4_mgmt),]

# without trt date: -------------------------------------------------------

s4_trtmatrix <- dcast(s4_mgmt, dow + lakename ~ year + species, value.var = "controlacres", fun.aggregate = function(x) mean(x, na.rm = T), fill = 0 )

#and now with dates
#' this is a challenge and an effing mess
s4_mgmt[ , td := order(treatmentdate) , .(dow, lakename, year, species)]

s4_mgmt[ , unique(treatmentdate) ]

s4_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := paste(year, treatmentdate, sep = "-") , ]
s4_mgmt[ str_detect(treatmentdate, "000"), treatmentdate := gsub("-20", "",
                                                                 gsub("000", "", treatmentdate)) , ]

s4_mgmt[is.na(treatmentdate), ]

s4_mgmt[treatmentdate == "", treatmentdate := NA ]

s4_mgmt_wdates <- dcast(s4_mgmt, dow + lakename + year + species ~ paste("date",td,sep = ""), value.var = "controlacres")

s4_mgmt_wdates[is.na(date1), date1 := date2 , ]
s4_mgmt_wdates[is.na(date1), date1 := date3]
s4_mgmt_wdates[is.na(date1), date1 := date4]


s4_mgmt_wdates[ , .N , is.na(date1) ]

# 

# clean up workspace ------------------------------------------------------

#remove intermediates for simple versions of data

rm(s1_grants, s1_IAPM, s1_mgmtdat, s1_survey, 
   s2_grants, s2_IAPM,
   s3_grants, s3_mgmtdat, s3_survey,
   s4_grants, 
   simp_grants, simp_IAPM, simp_mgmtdat, simp_survey,
   county_code_key, county_name_key,
   surveys2, mgmtdat, grantsdata_l, permits_IAPM)



# bring in mgmt data ------------------------------------------------------


# simplest ----------------------------------------------------------------

#' we've got 4 versions of the management data:
#' 
#' We'll use these to build a management index for each survey with a rearward
#' look, to say that in a given year, a lake has some value *m* that indicates 
#' that it was managed some qty over the previous n years. 
#' 
#' 

s1_trtmatrix[ , m_2012 :=  as.numeric(`2012`!=0) , ]
s1_trtmatrix[ , m_2013 :=  as.numeric(`2013`!=0) + as.numeric(`2012`!=0)/2 , ]
s1_trtmatrix[ , m_2014 :=  as.numeric(`2014`!=0) + as.numeric(`2013`!=0)/2 + as.numeric(`2012`!=0)/3 , ]
s1_trtmatrix[ , m_2015 :=  as.numeric(`2015`!=0) + as.numeric(`2014`!=0)/2 + as.numeric(`2013`!=0)/3 + as.numeric(`2012`!=0)/4 , ]
s1_trtmatrix[ , m_2016 :=  as.numeric(`2016`!=0) + as.numeric(`2015`!=0)/2 + as.numeric(`2014`!=0)/3 + as.numeric(`2013`!=0)/4 + as.numeric(`2012`!=0)/5, ]
s1_trtmatrix[ , m_2017 :=  as.numeric(`2017`!=0) + as.numeric(`2016`!=0)/2 + as.numeric(`2015`!=0)/3 + as.numeric(`2014`!=0)/4 + as.numeric(`2013`!=0)/5 + as.numeric(`2012`!=0)/6, ]
s1_trtmatrix[ , m_2018 :=  as.numeric(`2018`!=0) + as.numeric(`2017`!=0)/2 + as.numeric(`2016`!=0)/3 + as.numeric(`2015`!=0)/4 + as.numeric(`2014`!=0)/5 + as.numeric(`2013`!=0)/6 + as.numeric(`2012`!=0)/7, ]
s1_trtmatrix[ , m_2019 :=  as.numeric(`2019`!=0) + as.numeric(`2018`!=0)/2 + as.numeric(`2017`!=0)/3 + as.numeric(`2016`!=0)/4 + as.numeric(`2015`!=0)/5 + as.numeric(`2014`!=0)/6 + as.numeric(`2013`!=0)/7 + as.numeric(`2012`!=0)/8, ]

s1_trtmatrix[ , hist(m_2019) , ]


#setup naming for melt from patterns
names(s1_trtmatrix)[str_detect(names(s1_trtmatrix), "^2")] <- paste("y",names(s1_trtmatrix)[str_detect(names(s1_trtmatrix), "^2")], sep = "_")

#recast to long:
s1_trtmatrix_l <- melt(s1_trtmatrix, id.vars = c("dow", "lakename"),
                       value.name = c("ntrtdates", "m"),
                       measure.vars = patterns("^y_", "^m_"))[, variable := as.integer(as.character(factor(variable, labels = c(2012:2019))))]



#merge mgmt data on dows & years
s1_trtmatrix_l[ , p_dow := round(dow/100, 0) ,]
surveys[ , p_dow := round(DOW/100, 0) , ]

#gotta deal with subbasin exents
surveys[ , sort(unique(SUBBASIN)) ,]

#minnetonka
surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
s1_trtmatrix_l[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s1_trtmatrix_l[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

s1_trtmatrix_l[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s1_trtmatrix_l[str_detect(lakename, "innet") &
                 subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s1_trtmatrix_l[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s1_trtmatrix_l[, sort(unique(lakename)),]


s1_trtmatrix_l[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s1_trtmatrix_l[lakename == "koronis_(main_lake)", lakename := "koronis"]

s1_trtmatrix_l[lakename == "island(southbay)", subbasin := "south basin"]
s1_trtmatrix_l[lakename == "island(southbay)", lakename := "island"]

s1_trtmatrix_l[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s1_trtmatrix_l[lakename == "southeast_anderson", lakename := "anderson"]


s1_trtmatrix_l[lakename == "zumbra", subbasin := "Zumbra"]
s1_trtmatrix_l[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s1_trtmatrix_l[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]


#do merge, use dow numbers, subbasins, and years?:
names(s1_trtmatrix_l)[names(s1_trtmatrix_l)== "variable"] <- "year"
names(s1_trtmatrix_l)[names(s1_trtmatrix_l)== "subbasin"] <- "SUBBASIN"
names(s1_trtmatrix_l)[names(s1_trtmatrix_l)== "dow"] <- "DOW"
surveys[SUBBASIN == "", SUBBASIN := NA]

setkey(s1_trtmatrix_l, DOW, SUBBASIN, year )
setkey(surveys, DOW, SUBBASIN, year)

surveys_s1mgmt <- s1_trtmatrix_l[surveys[!is.na(DOW)] , , ] #drop where DOW == NA

surveys_s1mgmt[!is.na(m) , .N , year]
surveys_s1mgmt[!is.na(ntrtdates) , .N , year]

names(surveys_s1mgmt)

# treatment visualizations -------------------------------------------------------

ggplot(surveys_s1mgmt, aes(m, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()
ggplot(surveys_s1mgmt, aes(m, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()

#we can assume if no trt records, that the survey is on a 0 history lake - we'll do this cleanup at the end
# surveys_s1mgmt[is.na(m), m:=0]

ggplot(surveys_s1mgmt, aes(m, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()
ggplot(surveys_s1mgmt, aes(m, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()


# and we can ask if there's a within year effect visible:
ggplot(surveys_s1mgmt, aes(ntrtdates>0, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_boxplot()
ggplot(surveys_s1mgmt, aes(ntrtdates>0, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_boxplot()
# but recall that these surveys and trts happened in the same year, which means we don't know if they were pre or post trt. 

# review numbers:
surveys_s1mgmt[ m > 0 , .N, ]

surveys_s1mgmt[ m > 0 , length(unique(DOW)), ]

surveys_s1mgmt[ m > 0 , length(unique(year)), .(DOW) ][ ,hist(V1) ,]


# species specific mgmt data ----------------------------------------------------


s2_trtmatrix_ewm <- s2_trtmatrix[ , .SD , .SDcols = c("dow", "lakename", names(s2_trtmatrix)[str_detect(names(s2_trtmatrix), "ewm")])]

s2_trtmatrix_clp <- s2_trtmatrix[ , .SD , .SDcols = c("dow", "lakename", names(s2_trtmatrix)[str_detect(names(s2_trtmatrix), "clp")])]

names(s2_trtmatrix_ewm)[str_detect(names(s2_trtmatrix_ewm), "ewm")] <- gsub( "_ewm_targeted", "" , names(s2_trtmatrix_ewm)[str_detect(names(s2_trtmatrix_ewm), "ewm")])
names(s2_trtmatrix_clp)[str_detect(names(s2_trtmatrix_clp), "clp")] <- gsub( "_clp_targeted", "" , names(s2_trtmatrix_clp)[str_detect(names(s2_trtmatrix_clp), "clp")])


s2_trtmatrix_ewm[ , m_2012 :=  as.numeric(`2012`!=0) , ]
s2_trtmatrix_ewm[ , m_2013 :=  as.numeric(`2013`!=0) + as.numeric(`2012`!=0)/2 , ]
s2_trtmatrix_ewm[ , m_2014 :=  as.numeric(`2014`!=0) + as.numeric(`2013`!=0)/2 + as.numeric(`2012`!=0)/3 , ]
s2_trtmatrix_ewm[ , m_2015 :=  as.numeric(`2015`!=0) + as.numeric(`2014`!=0)/2 + as.numeric(`2013`!=0)/3 + as.numeric(`2012`!=0)/4 , ]
s2_trtmatrix_ewm[ , m_2016 :=  as.numeric(`2016`!=0) + as.numeric(`2015`!=0)/2 + as.numeric(`2014`!=0)/3 + as.numeric(`2013`!=0)/4 + as.numeric(`2012`!=0)/5, ]
s2_trtmatrix_ewm[ , m_2017 :=  as.numeric(`2017`!=0) + as.numeric(`2016`!=0)/2 + as.numeric(`2015`!=0)/3 + as.numeric(`2014`!=0)/4 + as.numeric(`2013`!=0)/5 + as.numeric(`2012`!=0)/6, ]
s2_trtmatrix_ewm[ , m_2018 :=  as.numeric(`2018`!=0) + as.numeric(`2017`!=0)/2 + as.numeric(`2016`!=0)/3 + as.numeric(`2015`!=0)/4 + as.numeric(`2014`!=0)/5 + as.numeric(`2013`!=0)/6 + as.numeric(`2012`!=0)/7, ]
s2_trtmatrix_ewm[ , m_2019 :=  as.numeric(`2019`!=0) + as.numeric(`2018`!=0)/2 + as.numeric(`2017`!=0)/3 + as.numeric(`2016`!=0)/4 + as.numeric(`2015`!=0)/5 + as.numeric(`2014`!=0)/6 + as.numeric(`2013`!=0)/7 + as.numeric(`2012`!=0)/8, ]

s2_trtmatrix_ewm[ , hist(m_2019) , ]

s2_trtmatrix_clp[ , m_2012 :=  as.numeric(`2012`!=0) , ]
s2_trtmatrix_clp[ , m_2013 :=  as.numeric(`2013`!=0) + as.numeric(`2012`!=0)/2 , ]
s2_trtmatrix_clp[ , m_2014 :=  as.numeric(`2014`!=0) + as.numeric(`2013`!=0)/2 + as.numeric(`2012`!=0)/3 , ]
s2_trtmatrix_clp[ , m_2015 :=  as.numeric(`2015`!=0) + as.numeric(`2014`!=0)/2 + as.numeric(`2013`!=0)/3 + as.numeric(`2012`!=0)/4 , ]
s2_trtmatrix_clp[ , m_2016 :=  as.numeric(`2016`!=0) + as.numeric(`2015`!=0)/2 + as.numeric(`2014`!=0)/3 + as.numeric(`2013`!=0)/4 + as.numeric(`2012`!=0)/5, ]
s2_trtmatrix_clp[ , m_2017 :=  as.numeric(`2017`!=0) + as.numeric(`2016`!=0)/2 + as.numeric(`2015`!=0)/3 + as.numeric(`2014`!=0)/4 + as.numeric(`2013`!=0)/5 + as.numeric(`2012`!=0)/6, ]
s2_trtmatrix_clp[ , m_2018 :=  as.numeric(`2018`!=0) + as.numeric(`2017`!=0)/2 + as.numeric(`2016`!=0)/3 + as.numeric(`2015`!=0)/4 + as.numeric(`2014`!=0)/5 + as.numeric(`2013`!=0)/6 + as.numeric(`2012`!=0)/7, ]
s2_trtmatrix_clp[ , m_2019 :=  as.numeric(`2019`!=0) + as.numeric(`2018`!=0)/2 + as.numeric(`2017`!=0)/3 + as.numeric(`2016`!=0)/4 + as.numeric(`2015`!=0)/5 + as.numeric(`2014`!=0)/6 + as.numeric(`2013`!=0)/7 + as.numeric(`2012`!=0)/8, ]

s2_trtmatrix_clp[ , hist(m_2019) , ]


#setup naming for melt from patterns
names(s2_trtmatrix_ewm)[str_detect(names(s2_trtmatrix_ewm), "^2")] <- paste("y",names(s2_trtmatrix_ewm)[str_detect(names(s2_trtmatrix_ewm), "^2")], sep = "_")
names(s2_trtmatrix_clp)[str_detect(names(s2_trtmatrix_clp), "^2")] <- paste("y",names(s2_trtmatrix_clp)[str_detect(names(s2_trtmatrix_clp), "^2")], sep = "_")


#recast to long:
s2_trtmatrix_ewm_l <- melt(s2_trtmatrix_ewm, id.vars = c("dow", "lakename"),
                           value.name = c("ntrtdates", "m"),
                           measure.vars = patterns("^y_", "^m_"))[, variable := as.integer(as.character(factor(variable, labels = c(2012:2019))))]

s2_trtmatrix_clp_l <- melt(s2_trtmatrix_clp, id.vars = c("dow", "lakename"),
                           value.name = c("ntrtdates", "m"),
                           measure.vars = patterns("^y_", "^m_"))[, variable := as.integer(as.character(factor(variable, labels = c(2012:2019))))]



#merge mgmt data on dows & years
s2_trtmatrix_ewm_l[ , p_dow := round(dow/100, 0) ,]

#gotta deal with subbasin exents
surveys[ , sort(unique(SUBBASIN)) ,]

#minnetonka
surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

s2_trtmatrix_ewm_l[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s2_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s2_trtmatrix_ewm_l[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s2_trtmatrix_ewm_l[, sort(unique(lakename)),]


s2_trtmatrix_ewm_l[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s2_trtmatrix_ewm_l[lakename == "koronis_(main_lake)", lakename := "koronis"]

s2_trtmatrix_ewm_l[lakename == "island(southbay)", subbasin := "south basin"]
s2_trtmatrix_ewm_l[lakename == "island(southbay)", lakename := "island"]

s2_trtmatrix_ewm_l[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s2_trtmatrix_ewm_l[lakename == "southeast_anderson", lakename := "anderson"]


s2_trtmatrix_ewm_l[lakename == "zumbra", subbasin := "Zumbra"]
s2_trtmatrix_ewm_l[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s2_trtmatrix_ewm_l[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]

#do merge, use dow numbers, subbasins, and years?:
names(s2_trtmatrix_ewm_l)[names(s2_trtmatrix_ewm_l)== "variable"] <- "year"
names(s2_trtmatrix_ewm_l)[names(s2_trtmatrix_ewm_l)== "subbasin"] <- "SUBBASIN"
names(s2_trtmatrix_ewm_l)[names(s2_trtmatrix_ewm_l)== "dow"] <- "DOW"

names(s2_trtmatrix_ewm_l)[names(s2_trtmatrix_ewm_l) %in% c("ntrtdates", "m")] <- 
  paste("ewm", names(s2_trtmatrix_ewm_l)[names(s2_trtmatrix_ewm_l) %in% c("ntrtdates", "m")], sep = "_")

setkey(s2_trtmatrix_ewm_l, DOW, SUBBASIN, year )
setkey(surveys_s1mgmt, DOW, SUBBASIN, year)

surveys_s2mgmt <- s2_trtmatrix_ewm_l[surveys_s1mgmt , , ]


#' Lets have a look at how our merge worked. For each year, how many cases of
#' ewm managment index > 0? And how about for the number of surveys in treatment years?
surveys_s2mgmt[ewm_m>0 , .N , year]
surveys_s2mgmt[ewm_ntrtdates>0 , .N , year]


#merge mgmt data on dows & years
s2_trtmatrix_clp_l[ , p_dow := round(dow/100, 0) ,]

#gotta deal with subbasin exents
# surveys[ , sort(unique(SUBBASIN)) ,]

#minnetonka
# surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
# s2_trtmatrix_clp_l[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s2_trtmatrix_clp_l[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

# s2_trtmatrix_clp_l[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s2_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s2_trtmatrix_clp_l[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s2_trtmatrix_clp_l[, sort(unique(lakename)),]


s2_trtmatrix_clp_l[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s2_trtmatrix_clp_l[lakename == "koronis_(main_lake)", lakename := "koronis"]

s2_trtmatrix_clp_l[lakename == "island(southbay)", subbasin := "south basin"]
s2_trtmatrix_clp_l[lakename == "island(southbay)", lakename := "island"]

s2_trtmatrix_clp_l[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s2_trtmatrix_clp_l[lakename == "southeast_anderson", lakename := "anderson"]


s2_trtmatrix_clp_l[lakename == "zumbra", subbasin := "Zumbra"]
s2_trtmatrix_clp_l[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s2_trtmatrix_clp_l[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]

#do merge, use dow numbers, subbasins, and years?:
names(s2_trtmatrix_clp_l)[names(s2_trtmatrix_clp_l)== "variable"] <- "year"
names(s2_trtmatrix_clp_l)[names(s2_trtmatrix_clp_l)== "subbasin"] <- "SUBBASIN"
names(s2_trtmatrix_clp_l)[names(s2_trtmatrix_clp_l)== "dow"] <- "DOW"

names(s2_trtmatrix_clp_l)[names(s2_trtmatrix_clp_l) %in% c("ntrtdates", "m")] <- 
  paste("clp", names(s2_trtmatrix_clp_l)[names(s2_trtmatrix_clp_l) %in% c("ntrtdates", "m")], sep = "_")

setkey(s2_trtmatrix_clp_l, DOW, SUBBASIN, year )
setkey(surveys_s2mgmt, DOW, SUBBASIN, year)

surveys_s2mgmt <- s2_trtmatrix_clp_l[surveys_s2mgmt , , ]

surveys_s2mgmt[clp_m>0 , .N , year]
surveys_s2mgmt[clp_ntrtdates>0 , .N , year]



# treatment visualizations 2.0 -------------------------------------------------------

ggplot(surveys_s2mgmt, aes(ewm_m, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()
ggplot(surveys_s2mgmt, aes(clp_m, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+ geom_smooth()


# and we can ask if there's a within year effect visible:
ggplot(surveys_s2mgmt, aes(ewm_ntrtdates>0, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_boxplot()
ggplot(surveys_s2mgmt, aes(clp_ntrtdates>0, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_boxplot()
# but recall that these surveys and trts happened in the same year, which means we don't know if they were pre or post tr -- we'll address this later 

# review numbers:
surveys_s2mgmt[ ewm_m > 0 , .N, ]
surveys_s2mgmt[ clp_m > 0 , .N, ]

surveys_s2mgmt[ ewm_m > 0 , length(unique(DOW)), ]
surveys_s2mgmt[ clp_m > 0 , length(unique(DOW)), ]

surveys_s2mgmt[ ewm_m > 0 , length(unique(year)), .(DOW) ][ ,hist(V1) ,]
surveys_s2mgmt[ clp_m > 0 , length(unique(year)), .(DOW) ][ ,hist(V1) ,]


# acreages of treatments --------------------------------------------------


s4_trtmatrix_ewm <- s4_trtmatrix[ , .SD , .SDcols = c("dow", "lakename", names(s4_trtmatrix)[str_detect(names(s4_trtmatrix), "ewm")])]

s4_trtmatrix_clp <- s4_trtmatrix[ , .SD , .SDcols = c("dow", "lakename", names(s4_trtmatrix)[str_detect(names(s4_trtmatrix), "clp")])]

names(s4_trtmatrix_ewm)[str_detect(names(s4_trtmatrix_ewm), "ewm")] <- gsub( "_ewm_targeted", "" , names(s4_trtmatrix_ewm)[str_detect(names(s4_trtmatrix_ewm), "ewm")])
names(s4_trtmatrix_clp)[str_detect(names(s4_trtmatrix_clp), "clp")] <- gsub( "_clp_targeted", "" , names(s4_trtmatrix_clp)[str_detect(names(s4_trtmatrix_clp), "clp")])


s4_trtmatrix_ewm[ , m_2012 :=  `2012` , ]
s4_trtmatrix_ewm[ , m_2013 :=  `2013` + `2012`/2 , ]
s4_trtmatrix_ewm[ , m_2014 :=  `2014` + `2013`/2 + `2012`/3 , ]
s4_trtmatrix_ewm[ , m_2015 :=  `2015` + `2014`/2 + `2013`/3 + `2012`/4 , ]
s4_trtmatrix_ewm[ , m_2016 :=  `2016` + `2015`/2 + `2014`/3 + `2013`/4 + `2012`/5, ]
s4_trtmatrix_ewm[ , m_2017 :=  `2017` + `2016`/2 + `2015`/3 + `2014`/4 + `2013`/5 + `2012`/6, ]
s4_trtmatrix_ewm[ , m_2018 :=  `2018` + `2017`/2 + `2016`/3 + `2015`/4 + `2014`/5 + `2013`/6 + `2012`/7, ]

s4_trtmatrix_ewm[ , hist(m_2018, breaks = 50) , ]

s4_trtmatrix_clp[ , m_2012 :=  `2012` , ]
s4_trtmatrix_clp[ , m_2013 :=  `2013` + `2012`/2 , ]
s4_trtmatrix_clp[ , m_2014 :=  `2014` + `2013`/2 + `2012`/3 , ]
s4_trtmatrix_clp[ , m_2015 :=  `2015` + `2014`/2 + `2013`/3 + `2012`/4 , ]
s4_trtmatrix_clp[ , m_2016 :=  `2016` + `2015`/2 + `2014`/3 + `2013`/4 + `2012`/5, ]
s4_trtmatrix_clp[ , m_2017 :=  `2017` + `2016`/2 + `2015`/3 + `2014`/4 + `2013`/5 + `2012`/6, ]
s4_trtmatrix_clp[ , m_2018 :=  `2017`/2 + `2016`/3 + `2015`/4 + `2014`/5 + `2013`/6 + `2012`/7, ]


s4_trtmatrix_clp[ , hist(m_2018, breaks = 50) , ]


#setup naming for melt from patterns
names(s4_trtmatrix_ewm)[str_detect(names(s4_trtmatrix_ewm), "^2")] <- paste("y",names(s4_trtmatrix_ewm)[str_detect(names(s4_trtmatrix_ewm), "^2")], sep = "_")
names(s4_trtmatrix_clp)[str_detect(names(s4_trtmatrix_clp), "^2")] <- paste("y",names(s4_trtmatrix_clp)[str_detect(names(s4_trtmatrix_clp), "^2")], sep = "_")


#recast to long:
s4_trtmatrix_ewm_l <- melt(s4_trtmatrix_ewm, id.vars = c("dow", "lakename"),
                           value.name = c("controlacres", "acres_m"),
                           measure.vars = patterns("^y_", "^m_"))[, variable := as.integer(as.character(factor(variable, labels = c(2012:2018))))]

s4_trtmatrix_clp_l <- melt(s4_trtmatrix_clp, id.vars = c("dow", "lakename"),
                           value.name = c("controlacres", "acres_m"),
                           measure.vars = patterns("^y_", "^m_"))[, variable := as.integer(as.character(factor(variable, labels = c(2012:2018))))]



#merge mgmt data on dows & years
s4_trtmatrix_ewm_l[ , p_dow := round(dow/100, 0) ,]

#gotta deal with subbasin exents
surveys[ , sort(unique(SUBBASIN)) ,]


#minnetonka
surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

s4_trtmatrix_ewm_l[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s4_trtmatrix_ewm_l[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s4_trtmatrix_ewm_l[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s4_trtmatrix_ewm_l[, sort(unique(lakename)),]


s4_trtmatrix_ewm_l[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s4_trtmatrix_ewm_l[lakename == "koronis_(main_lake)", lakename := "koronis"]

s4_trtmatrix_ewm_l[lakename == "island(southbay)", subbasin := "south basin"]
s4_trtmatrix_ewm_l[lakename == "island(southbay)", lakename := "island"]

s4_trtmatrix_ewm_l[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s4_trtmatrix_ewm_l[lakename == "southeast_anderson", lakename := "anderson"]


s4_trtmatrix_ewm_l[lakename == "zumbra", subbasin := "Zumbra"]
s4_trtmatrix_ewm_l[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s4_trtmatrix_ewm_l[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]

#do merge, use dow numbers, subbasins, and years?:
names(s4_trtmatrix_ewm_l)[names(s4_trtmatrix_ewm_l)== "variable"] <- "year"
names(s4_trtmatrix_ewm_l)[names(s4_trtmatrix_ewm_l)== "subbasin"] <- "SUBBASIN"
names(s4_trtmatrix_ewm_l)[names(s4_trtmatrix_ewm_l)== "dow"] <- "DOW"

names(s4_trtmatrix_ewm_l)[names(s4_trtmatrix_ewm_l) %in% c("controlacres", "acres_m")] <- 
  paste("ewm", names(s4_trtmatrix_ewm_l)[names(s4_trtmatrix_ewm_l) %in% c("controlacres", "acres_m")], sep = "_")

setkey(s4_trtmatrix_ewm_l, DOW, SUBBASIN, year )
setkey(surveys_s2mgmt, DOW, SUBBASIN, year)

surveys_s2mgmt[s4_trtmatrix_ewm_l , ':=' (ewm_controlacres = ewm_controlacres, ewm_acres_m = ewm_acres_m) , ]

surveys_s2mgmt[ewm_controlacres>0 , .N , year]
surveys_s2mgmt[ewm_acres_m>0 , .N , year]


#merge mgmt data on dows & years
s4_trtmatrix_clp_l[ , p_dow := round(dow/100, 0) ,]

#gotta deal with subbasin exents
surveys[ , sort(unique(SUBBASIN)) ,]

#minnetonka
surveys[str_detect(LAKE_NAME, "inneto"), sort(unique(SUBBASIN))]
s4_trtmatrix_clp_l[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s4_trtmatrix_clp_l[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

s4_trtmatrix_clp_l[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s4_trtmatrix_clp_l[str_detect(lakename, "innet") &
                     subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s4_trtmatrix_clp_l[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s4_trtmatrix_clp_l[, sort(unique(lakename)),]


s4_trtmatrix_clp_l[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s4_trtmatrix_clp_l[lakename == "koronis_(main_lake)", lakename := "koronis"]

s4_trtmatrix_clp_l[lakename == "island(southbay)", subbasin := "south basin"]
s4_trtmatrix_clp_l[lakename == "island(southbay)", lakename := "island"]

s4_trtmatrix_clp_l[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s4_trtmatrix_clp_l[lakename == "southeast_anderson", lakename := "anderson"]


s4_trtmatrix_clp_l[lakename == "zumbra", subbasin := "Zumbra"]
s4_trtmatrix_clp_l[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s4_trtmatrix_clp_l[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]

#do merge, use dow numbers, subbasins, and years?:
names(s4_trtmatrix_clp_l)[names(s4_trtmatrix_clp_l)== "variable"] <- "year"
names(s4_trtmatrix_clp_l)[names(s4_trtmatrix_clp_l)== "subbasin"] <- "SUBBASIN"
names(s4_trtmatrix_clp_l)[names(s4_trtmatrix_clp_l)== "dow"] <- "DOW"

names(s4_trtmatrix_clp_l)[names(s4_trtmatrix_clp_l) %in% c("controlacres", "acres_m")] <- 
  paste("clp", names(s4_trtmatrix_clp_l)[names(s4_trtmatrix_clp_l) %in% c("controlacres", "acres_m")], sep = "_")

setkey(s4_trtmatrix_clp_l, DOW, SUBBASIN, year )
setkey(surveys_s2mgmt, DOW, SUBBASIN, year)

surveys_s2mgmt[s4_trtmatrix_clp_l , ':=' (clp_controlacres = clp_controlacres, clp_acres_m = clp_acres_m) , ]

surveys_s2mgmt[clp_controlacres>0 , .N , year]
surveys_s2mgmt[clp_acres_m>0 , .N , year]


# acreage visualizations 2.0 -------------------------------------------------------

# and we can ask if there's a within year effect visible:
ggplot(surveys_s2mgmt, aes(ewm_controlacres, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+scale_x_log10()+ geom_smooth()
ggplot(surveys_s2mgmt, aes(clp_controlacres, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+scale_x_log10()+ geom_smooth()

# versus an accumulation of trt acres effect
ggplot(surveys_s2mgmt, aes(ewm_acres_m, Myriophyllum_spicatum/n_points_vegetated ) ,)+
  geom_point()+scale_x_log10()
ggplot(surveys_s2mgmt, aes(clp_acres_m, Potamogeton_crispus/n_points_vegetated ) ,)+
  geom_point()+scale_x_log10()
# but recall that these surveys and trts happened in the same year, which means we don't know if they were pre or post trt. 

# review numbers:
surveys_s2mgmt[ ewm_acres_m > 0 , .N, ]
surveys_s2mgmt[ clp_acres_m > 0 , .N, ]

surveys_s2mgmt[ ewm_acres_m > 0 , length(unique(DOW)), ]
surveys_s2mgmt[ clp_acres_m > 0 , length(unique(DOW)), ]

surveys_s2mgmt[ ewm_acres_m > 0 , length(unique(year)), .(DOW) ][ ,hist(V1) ,]
surveys_s2mgmt[ clp_acres_m > 0 , length(unique(year)), .(DOW) ][ ,hist(V1) ,]


# pre- or post- trt? ---------------------

s1_mgmt_wdates

#merge mgmt data on dows & years
s1_mgmt_wdates[ , p_dow := round(dow/100, 0) ,]


#minnetonka

s1_mgmt_wdates[str_detect(lakename, "innet") , sort(unique(lakename)) ,]
s1_mgmt_wdates[str_detect(lakename, "innet") , subbasin := word(lakename, start = 2, sep = "_") , ]

s1_mgmt_wdates[str_detect(lakename, "innet") , sort(unique(subbasin)) ,]
#st.albans
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "st" , subbasin := "St Albans Bay"]
#carsons & st louis
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "carsons/stlouisbay" , subbasin := "Carsons & St. Louis Bays"]
#grays bay
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "grays" , subbasin := "Grays Bay"]
#lower lake
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "lower" , subbasin := "Lower Lake"]
#carsons bay
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "carsons" , subbasin := "Carsons Bay"]
#crystal bay
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "crystal" , subbasin := "Crystal Bay"]
#northarm
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "northarm" , subbasin := "North Arm Bay" ]
#halsteads
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "halsteds" , subbasin := "Halsteads Bay" ]
#upper
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "upper" , subbasin := "Upper Lake" ]
#gideons
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "gideons" , subbasin := "Gideons Bay"  ]
#maxwell
s1_mgmt_wdates[str_detect(lakename, "innet") &
                 subbasin == "maxwell" , subbasin := "Maxwell Bay"  ]

s1_mgmt_wdates[str_detect(lakename, "innet") , lakename := "minnetonka"]

#more subbasin exents
surveys[!str_detect(LAKE_NAME, "inneto") & !SUBBASIN=="", sort(unique(paste(SUBBASIN, LAKE_NAME, sep = "_"))) ,]

#can ignore mille lacs, leech, 
s1_mgmt_wdates[, sort(unique(lakename)),]


s1_mgmt_wdates[lakename == "koronis_(main_lake)", subbasin := "main lake"]
s1_mgmt_wdates[lakename == "koronis_(main_lake)", lakename := "koronis"]

s1_mgmt_wdates[lakename == "island(southbay)", subbasin := "south basin"]
s1_mgmt_wdates[lakename == "island(southbay)", lakename := "island"]

s1_mgmt_wdates[lakename == "southeast_anderson", subbasin := "Southeast Basin"]
s1_mgmt_wdates[lakename == "southeast_anderson", lakename := "anderson"]


s1_mgmt_wdates[lakename == "zumbra", subbasin := "Zumbra"]
s1_mgmt_wdates[lakename == "zumbra", lakename := "Zumbra-Sunny"]
s1_mgmt_wdates[lakename == "zumbra_sunny", lakename := "Zumbra-Sunny"]


#do merge, use dow numbers, subbasins, and years?:
names(s1_mgmt_wdates)[names(s1_mgmt_wdates)== "variable"] <- "year"
names(s1_mgmt_wdates)[names(s1_mgmt_wdates)== "subbasin"] <- "SUBBASIN"
names(s1_mgmt_wdates)[names(s1_mgmt_wdates)== "dow"] <- "DOW"

s1_mgmt_wdates[ ,year := as.integer(year) , ]
setkey(s1_mgmt_wdates, DOW, SUBBASIN, year )

surveys_s2mgmt <- s1_mgmt_wdates[surveys_s2mgmt , , ]

sum(duplicated(surveys_s2mgmt[ ,SURVEY_ID , ]))

# now we will export, drop duplicated surveys, and use eyes & brain to assign each survey as pre- or post-
# fwrite(surveys_s2mgmt, file = "data&scripts/data/output/survs_mgmt_datemess.csv")

#clean out our workspace:

# rm(s1_mgmt, s1_mgmt_wdates, s1_trtmatrix, s1_trtmatrix_dates, s1_trtmatrix_l,
#    s2_mgmt, s2_mgmt_wdates, s2_trtmatrix, s2_trtmatrix_clp, s2_trtmatrix_clp_l, s2_trtmatrix_dates, s2_trtmatrix_ewm, s2_trtmatrix_ewm_l, 
#    s3_mgmt, s3_mgmt_wdates, s3_trtmatrix, 
#    s4_mgmt, s4_mgmt_wdates, s4_trtmatrix, s4_trtmatrix_clp, s4_trtmatrix_clp_l, s4_trtmatrix_ewm, s4_trtmatrix_ewm_l,
#    surveys_s1mgmt, surveys_s2mgmt)






# footer ------------------------------------------------------------------



# On 14 June, 2022 (WHEN WRITTEN) this script resulted in 1785 records for management actions
