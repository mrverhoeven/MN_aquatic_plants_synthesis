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

#I went through this spreadsheet and split up any multi-DOW rows, splitting the treatment acreage equally among DOWs and copying all other trt info

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
setDT(IAPMsurveys)
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

surveys2[str_detect(herbicide, "AQUATHOL") & !workdate == "7" , clp_targeted := T ]

surveys2[str_detect(herbicide, "riclop") & str_detect(workdate, "6")  , clp_targeted := T]

surveys2[str_detect(herbicide, "HYDRO") & !workdate == 10 , clp_targeted := T]

surveys2[ herbicide %in% c("HYDRO191", "G_HYDRO191")& !workdate == 10, clp_targeted := T]

surveys2[herbicide %in% c("Clearcast 2.7GLB"), clp_targeted := T]

unique(surveys2[is.na(clp_targeted), .(herbicide, workdate)])

surveys2 <- surveys2[!herbicide %in% c("Potassium Chloride lbs", "Earthtech QZ" )] #zebra mussel control

# surveys2[permit %in% c("15W-3B107", "12W-2B024", "15W-2B32", "15W-3A123") , , ]

surveys2 <- surveys2[!permit %in% c("15W-3B107", "12W-2B024", "15W-2B32", "15W-3A123") , , ] #we think these are flowring rush and starry stonewort

# surveys2[is.na(clp_targeted) & is.na(ewm_targeted), .N , .(herbicide, workdate) ]

surveys2 <- surveys2[!herbicide %in% c("Knockout")] # non-aquatic herb--unlikely to be EWM control 

surveys2 <- surveys2[!(lakename %in% c("detroit") & is.na(clp_targeted))]#these are probable flowering rush treatments

surveys2[is.na(clp_targeted) , .N , .(herbicide,workdate) ]

surveys2[is.na(clp_targeted) , ewm_targeted := T ]

surveys2[ , year := as.numeric(year) ,]

# surveys2[!is.na(acreage_ah) , .(comacrec, comacrm, acreage_ah) , ][is.na(comacrec)]

# names(surveys2)

surveys2 <- surveys2[ , .(lakename, subbasin, downum, year, permit, workdate,  comacrec, comacrm, method, herbicide, qty, clp_targeted, ewm_targeted) ,  ]

# surveys2[ , .N , .(year, lakename, subbasin, downum, clp_targeted, ewm_targeted)][N>1]

surveys2[, qty := round(qty, 2)]

# merge some stuff ---------------------------------------------------------

#prepnames & classes
names(grantsdata) <- tolower(names(grantsdata))
# names(grantsdata)

grantsdata[ , isparea := NULL ,] 
grantsdata[ , county := NULL ,]

setcolorder(grantsdata, c("year", "dow"))

# names(mgmtdat)
names(mgmtdat)[names(mgmtdat)  == "calendar_year"] <- "year"
names(mgmtdat)[names(mgmtdat)  == "resource_number"] <- "dow"
mgmtdat <- mgmtdat[ , .(year, dow, resource_name, workdate, total_cut_area_acres, total_treated_area_acres, herbicide, qty  ) , ]

# str(mgmtdat)
# str(grantsdata)

# mgmtdat[ , sort(unique(dow))]
mgmtdat <- mgmtdat[!dow == "M-050"]

grantsdata[ , dow := as.integer(dow) , ]
mgmtdat[ , year := as.integer(year) , ]
mgmtdat[ , dow := as.integer(dow) , ]
mgmtdat[ , county := county_code_key[match(mgmtdat[ , round(dow/1000000,0) ],county_code_key$cty_code), V1]]
names(mgmtdat)[!names(mgmtdat) %in%  c("dow","year")] <- paste(names(mgmtdat)[!names(mgmtdat) %in%  c("dow","year")], "_surveys", sep = "" )

#date column will cause rbindlist to have heartburn
grantsdata[ , treatmentdate := as.character(treatmentdate) , ]

mgmtdata <- merge(grantsdata, mgmtdat, by = c("dow","year"), all = T)
rm(grantsdata, mgmtdat)

# names(mgmtdata)

mgmtdata[is.na(lakename), lakename := resource_name_surveys , ]

# mgmtdata[str_detect(lakename, regex("lake", ignore_case = T)) ,  lakename  , ]

mgmtdata[ ,  lakename:= gsub("Lake", "",
                              gsub("lake", "", 
                                   gsub("lakes", "", 
                                        gsub("Lakes", "" , lakename )
      
    )
  )
)  , ]

mgmtdata[ , lakename := gsub( " ", "", lakename) , ]

mgmtdata[ ,  resource_name_surveys:= gsub("Lake", "",
                             gsub("lake", "", 
                                  gsub("lakes", "", 
                                       gsub("Lakes", "" , resource_name_surveys )
                                       
                                  )
                             )
)  , ]

mgmtdata[ , resource_name_surveys := gsub( " ", "", resource_name_surveys) , ]


# mgmtdata[!is.na(resource_name_surveys) & !is.na(lakename), .(lakename, resource_name_surveys)]
# mgmtdata[ , .(lakename, resource_name_surveys) , ]

mgmtdata[!is.na(species) , grantsprogram := "grantsprogram" ,]
mgmtdata[!is.na(resource_name_surveys), lateeraAPMdata := "lateeraAPMdata"]


#merge to IAPM data

names(permits_IAPM)[names(permits_IAPM)%in%c("downum")] <- "dow"

permits_IAPM[ , dow:= as.integer(dow) ,]

mgmtdata <- merge(mgmtdata, permits_IAPM, by = c("dow","year"), all = T)

# names(mgmtdata) 

mgmtdata[!is.na(lakename.y) , IAPM_Permits := "IAPM_Permits" , ]
rm(permits_IAPM)


names(mgmtdata)[names(mgmtdata) %in% "lakename.x"] <- "lakename_grantsprogram"

names(mgmtdata)[names(mgmtdata) %in% "lakename.y"] <- "lakename_IAPMpermits"

names(mgmtdata)[names(mgmtdata) %in% "resource_name_surveys"] <- "lakename_lateeraAPM"

mgmtdata[ , lakename := lakename_grantsprogram , ] #wendy enters lakename
mgmtdata[is.na(lakename) , lakename := lakename_IAPMpermits , ] #staff enter lakename
mgmtdata[is.na(lakename) , lakename := lakename_lateeraAPM , ] #staff enter lakename
mgmtdata[, lakename := tolower(lakename)]

# sort(unique(mgmtdata$lakename))

# sort(unique(surveys2$lakename))[!sort(unique(surveys2$lakename))%in%sort(unique(mgmtdata$lakename))]

surveys2[ , year := as.integer(year)]

# summary(mgmtdata$year)

# summary(surveys2$year)

mgmtdata <- merge(surveys2, mgmtdata, by.x = c("downum", "year"), by.y = c("dow", "year"), all = T)

rm(surveys2)

#clear out some stuff:
rm( county_code_key, county_name_key)

names(mgmtdata)[names(mgmtdata)%in% c("lakename.x")] <- "lakename_earlyeraAPM"
names(mgmtdata)[names(mgmtdata)%in% c("lakename.y")] <- "lakename"

mgmtdata[ , lakename_earlyeraAPM := gsub("_", "", lakename_earlyeraAPM)]

# mgmtdata[ lakename_earlyeraAPM != lakename, .(lakename_earlyeraAPM,lakename)]

mgmtdata[is.na(lakename), lakename := lakename_earlyeraAPM]

# cleaning on the merged dataset: -----------------------------------------

setcolorder(mgmtdata, c("year", "downum", "lakename", "subbasin"))
names(mgmtdata)

#target species:
#ewm
# summary(mgmtdata[ ,.(ewm_targeted, ewm_targeted.x,ewm_targeted.y),] )
mgmtdata[ewm_targeted.x == T | ewm_targeted.y == T, ewm_targeted := T ]

mgmtdata[ , c("ewm_targeted.x", "ewm_targeted.y") := NULL , ]

mgmtdata[ , .N , ewm_targeted]

# summary(mgmtdata$ewm_targeted.x)
#clp
# summary(mgmtdata[ ,.(clp_targeted, clp_targeted.x, clp_targeted.y),] )

mgmtdata[clp_targeted.x == T | clp_targeted.y == T, clp_targeted := T ]

mgmtdata[ , c("clp_targeted.x", "clp_targeted.y") := NULL , ]

mgmtdata[ , .N , clp_targeted]


#which records do we not know the target organism?
# mgmtdata[is.na(clp_targeted) & is.na(ewm_targeted), .N ,]
#drop those
mgmtdata <- mgmtdata[!(is.na(clp_targeted) & is.na(ewm_targeted))]

mgmtdata[ , c("species.x", "species_groups", "species.y") := NULL, ]

#drop county
mgmtdata[ , "county" := NULL]
mgmtdata[ , county_surveys := NULL]

#lakenames
# mgmtdata[ , .(lakename, lakename_grantsprogram, lakename_lateeraAPM, lakename_IAPMpermits) , ]

setcolorder(mgmtdata, c("year", "downum", "lakename", "subbasin", "lakename_grantsprogram", "lakename_lateeraAPM", "lakename_IAPMpermits", "lakename_earlyeraAPM" ))


#year/date
# mgmtdata[ , .(year, workdate_surveys, treatmentdate, effective_date, workdate) , ]

names(mgmtdata)[match(c("workdate_surveys", "treatmentdate", "effective_date", "workdate"),names(mgmtdata))] <- c("workdate_lateraAPM", "trtdate_grantsprogram", "effectivedate_IAPMpermits", "workdate_earlyeraAPM")

mgmtdata[  , c("workdate_lateraAPM", "trtdate_grantsprogram", "effectivedate_IAPMpermits", "workdate_earlyeraAPM") := lapply(.SD, as.character) , .SDcols = c("workdate_lateraAPM", "trtdate_grantsprogram", "effectivedate_IAPMpermits", "workdate_earlyeraAPM") ,]

# mgmtdata[  , .SD , .SDcols = c("workdate_lateraAPM", "trtdate_grantsprogram", "effectivedate_IAPMpermits", "workdate_earlyeraAPM") ,]

mgmtdata[ , trtdate := trtdate_grantsprogram]
# mgmtdata[is.na(trtdate) , sort(unique(workdate_lateraAPM))]

mgmtdata[ , workdate_lateraAPM := gsub("April", "4", 
                                       gsub("May", "5",
                                            gsub("June", "6", 
                                                 gsub("July", "7", 
                                                      gsub( "August", "8", 
                                                            gsub("September", "9",
                                                                 gsub( "October", "10", 
                                                                       gsub("November", "11", 
                                                                            workdate_lateraAPM)))))))) ,]

mgmtdata[is.na(trtdate), trtdate := workdate_earlyeraAPM]
mgmtdata[is.na(trtdate), trtdate := workdate_lateraAPM]
# mgmtdata[!is.na(trtdate), sort(unique(trtdate))]
mgmtdata[ , c("workdate_lateraAPM", "workdate_earlyeraAPM", "trtdate_grantsprogram") := NULL]

mgmtdata[ , organization := NULL]

mgmtdata[is.na(permit) , permit := permit_number ,]
mgmtdata[ , permit_number := NULL]


# mgmtdata[ , .(total_cut_area_acres_surveys, total_treated_area_acres_surveys, acrestreated, comacrec, comacrm)]

# check if other acreage data exist:
# summary(mgmtdata[is.na(acrestreated), .(as.numeric(total_cut_area_acres_surveys), as.numeric(total_treated_area_acres_surveys), acrestreated, comacrec, comacrm) ])

mgmtdata[is.na(acrestreated), acrestreated := comacrec]
# mgmtdata[ , sort(unique(total_treated_area_acres_surveys))]
mgmtdata[ , total_treated_area_acres_surveys := as.numeric(gsub("ac", "",
                             gsub("Acres", "", 
                                  gsub("Ac", "",
                                       gsub("acres", "", 
                                            trimws(total_treated_area_acres_surveys, which = "both"))))))]

mgmtdata[ , total_cut_area_acres_surveys := as.numeric(gsub("ac", "",
                                                                gsub("Acres", "", 
                                                                     gsub("Ac", "",
                                                                          gsub("acres", "", 
                                                                               trimws(total_cut_area_acres_surveys, which = "both"))))))]
mgmtdata[is.na(acrestreated), acrestreated := total_treated_area_acres_surveys]
mgmtdata[is.na(acrestreated), acrestreated := total_cut_area_acres_surveys]

mgmtdata[ , c("total_treated_area_acres_surveys", "total_cut_area_acres_surveys", "comacrec", "comacrm") := NULL , ]

#where no trt method is specified, fill in:
# mgmtdata[ , sort(unique(treatmentmethod))]
mgmtdata[treatmentmethod == "" ,treatmentmethod := NA  , ]

# mgmtdata[ , sort(unique(proposedmethod))]
mgmtdata[proposedmethod == "" ,proposedmethod := NA  , ]

mgmtdata[, controlmethod := treatmentmethod]
mgmtdata[is.na(treatmentmethod), controlmethod := proposedmethod]
mgmtdata[is.na(controlmethod), controlmethod := method]





# mgmtdata[ is.na(controlmethod) , sort(unique(product))]
mgmtdata[product %in% c("", "NULL") , product := NA]

mgmtdata[is.na(controlmethod), controlmethod := product]

# mgmtdata[ is.na(controlmethod) , sort(unique(herbicide_surveys))]

mgmtdata[is.na(controlmethod), controlmethod := as.character(herbicide_surveys)]

#merge herbicide data cols together
mgmtdata[!is.na(product), product_IAPMpermits := paste(product,product_amount,measurement_units, sep = "_")]
mgmtdata[ , c("product","product_amount","measurement_units") := NULL , ]

mgmtdata[!is.na(herbicide_surveys), product_lateeraAPM := paste(herbicide_surveys, qty_surveys, sep = "_")]
mgmtdata[ , c("herbicide_surveys", "qty_surveys") := NULL , ]

mgmtdata[!is.na(treatmentmethod), product_grantsdata := paste(treatmentmethod, herbicerate, totalproductused, sep = "_")]
mgmtdata[ , c("treatmentmethod", "herbicerate", "totalproductused", "windspeed", "watertemp") := NULL , ]

mgmtdata[!is.na(herbicide), product_earlyeraAPM := paste(herbicide, qty, sep = "_")]
mgmtdata[ , c("method", "herbicide", "qty") := NULL ]

mgmtdata[ , proposedmethod := NULL]

setcolorder(mgmtdata, c("year", "downum", "lakename", "clp_targeted", "ewm_targeted", "controlmethod", "trtdate" , "acrestreated", "permit"))

#clean up control methods:
# mgmtdata[ , sort(unique(controlmethod)) , ]
mgmtdata[str_detect(controlmethod, pattern = "arve") , physical_removal := T , ]
mgmtdata[str_detect(controlmethod, pattern = "ull") , physical_removal := T, ]
mgmtdata[str_detect(controlmethod, pattern = "echa") , physical_removal := T, ]
# mgmtdata[is.na(physical_removal),  sort(unique(controlmethod))]

mgmtdata[is.na(physical_removal),  chemical_control := T]

# mgmtdata[physical_removal == T, sort(unique(controlmethod))]

mgmtdata[str_detect(controlmethod, pattern = "Aquathol") , chemical_control := T, ]
mgmtdata[str_detect(controlmethod, pattern = "chemical") , chemical_control := T, ]
mgmtdata[str_detect(controlmethod, pattern = "DMA-4") , chemical_control := T, ]
mgmtdata[is.na(chemical_control), chemical_control := F]
mgmtdata[is.na(physical_removal), physical_removal := F]


# mgmtdata[ , sort(unique(permit_used)) , ]
mgmtdata <- mgmtdata[!str_detect(permit_used, pattern = "No") , , ]
mgmtdata[ , permit_used := NULL , ]


#drop complete duplicates
mgmtdata <- mgmtdata[!duplicated(mgmtdata), , ]

mgmtdata[ , records := .N, .(year, downum, lakename)]

mgmtdata[ , .N , .(clp_targeted, ewm_targeted)  ]

mgmtdata[ , .N, .(year, downum, lakename)][ , hist(N, breaks = 80)]

dcast(mgmtdata[ , clp_targeted , .(downum, year, subbasin)][!is.na(clp_targeted)], downum + subbasin ~ year, value.var = "clp_targeted")

dcast(mgmtdata[ , ewm_targeted , .(downum, year, subbasin)][!is.na(ewm_targeted)], downum + subbasin ~ year, value.var = "ewm_targeted")

#' currently, I'm losing all of the pre-2016 data... thats wrong and no bueno. Start there tomorrow!






# footer ------------------------------------------------------------------



# On 14 June, 2022 (WHEN WRITTEN) this script resulted in 1785 records for management actions
