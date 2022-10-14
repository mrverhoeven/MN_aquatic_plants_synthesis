#'---
#' title: "unite Secchi and plant survey data"
#' author: "Mike Verhoeven, Dan Larkin"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

# header ------------------------------------------------------------------

#' # Preamble
#' Load libraries
#+warning=FALSE, message=FALSE 



# load in data ------------------------------------------------------------

# read in files -- this is done in the main data prep script

#Secchi data
# secchi <- fread(input = "data&scripts/data/input/AllSecchi_plus_ShallowLakesSecchi.csv") #import, dropping the exported row numbers


# review whats here: ------------------------------------------------------

# number of observations
hist(secchi[,year(Date)])
hist(secchi[,month(Date)])
secchi[,.N,Source]

hist(plants[ ,.N , .(SURVEY_ID,YEAR) ][ , YEAR,])


secchi[, YEAR := year(Date)]

secchi[ , old_DOW := DOW]
secchi[, DOW := as.integer(DOW)]
secchi[ is.na(DOW) , old_DOW ]


#how many survey DOW's are missing a secchi for the lake (ever)?
summary(plants[ , unique(DOW) , ]%in%secchi[ ,DOW ,])

#how many surveys are missing a secchi for that year?
summary(plants[ , .N ,.(DOW,YEAR) ][,paste(DOW,YEAR, sep = "_"),] %in% secchi[ ,paste(DOW,YEAR, sep = "_") ,])

# evaluate fuzzy temporal joins (+/- 0 to 3yr) ---------------------------------------------------------


#' # Secchi measurements for each obs:
#' 
#' Assign a Secchi to each observation based on a few rules (EDIT as you see fit):
#' 
#'    1. Use only Secchi from July-Sep
#'    2. Create a mean for that time pd. for each lake-year of secchi data
#'    3. Use summer means in same year (or +/- 1 year if no w/in year available)
#'        

#add month to secchi
secchi[ , MONTH := month(Date)] 

# Survey data (survey information columns from plants)
surveys <- plants[ , .N, .(DOW, SURVEY_DATE, YEAR, MONTH, SURVEY_ID)]

# Joining secchi and surveys tables
surveys.secchi <- merge(surveys, secchi, by.x = "DOW", by.y = "DOW", allow.cartesian=TRUE)

# Subsetting to summer secchi readings
surveys.secchi.summer <- surveys.secchi[MONTH.y %in% 7:9]

# Same-year matches
surveys.secchi.summer.sameYr <- surveys.secchi.summer[YEAR.x == YEAR.y]

# +/- 1-year matches
surveys.secchi.summer.within1Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-1, YEAR.y+1)]

# +/- 2-year matches
surveys.secchi.summer.within2Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-2, YEAR.y+2)]

# +/- 3-year matches
surveys.secchi.summer.within3Yr <- surveys.secchi.summer[YEAR.x %between% list(YEAR.y-3, YEAR.y+3)]

matching.summ <- 
  data.frame(Year.diff = c(0, 1, 2, 3), 
             Matched.surveys = c(length(unique(surveys.secchi.summer.sameYr$SURVEY_ID)),
                                 length(unique(surveys.secchi.summer.within1Yr$SURVEY_ID)),
                                 length(unique(surveys.secchi.summer.within2Yr$SURVEY_ID)),
                                 length(unique(surveys.secchi.summer.within3Yr$SURVEY_ID))),
             Total.surveys = dim(surveys)[1]
  )

(matching.summ <- transform(matching.summ, Prop.matched = round(Matched.surveys/Total.surveys, digits = 3)))


# choose a timeframe for join ---------------------------------------------


# Secchi data to use: +/- 1-year matches w/ multiple secchi readings averaged
surveys.secchi.final <- surveys.secchi.summer.within1Yr
surveys.secchi.final[, Source := NULL]
setnames(surveys.secchi.final, old = c("YEAR.x", "YEAR.y", "MONTH.x", "Date", "MONTH.y"), 
         new = c("YEAR.SURVEY", "YEAR.SECCHI", "MONTH.SURVEY", "DATE.SECCHI", "MONTH.SECCHI"), skip_absent=TRUE)  


# calculate metrics for selected data -------------------------------------

# calc useable values and sum stats from each secchi set used by each survey linking to them
surveys.secchi.final <- surveys.secchi.final[, .(Secchi_m.mean=mean(Secchi_m), YEAR.SECCHI.mean=mean(YEAR.SECCHI), MONTH.SECCHI.mean=mean(MONTH.SECCHI),
                                                 Secchi_m.min=min(Secchi_m), YEAR.SECCHI.min=min(YEAR.SECCHI), MONTH.SECCHI.min=min(MONTH.SECCHI),
                                                 Secchi_m.max=max(Secchi_m), YEAR.SECCHI.max=max(YEAR.SECCHI), MONTH.SECCHI.max=max(MONTH.SECCHI),
                                                 Secchi_m.sd=sd(Secchi_m), Secchi_m.length=length(Secchi_m)), 
                                             .(DOW, SURVEY_DATE, YEAR.SURVEY, MONTH.SURVEY, SURVEY_ID)]  
surveys.secchi.final[, Secchi_m.se := Secchi_m.sd/sqrt(Secchi_m.sd)]
surveys.secchi.final[Secchi_m.se == "NaN", Secchi_m.se := 0]

surveys.secchi.final[, .(Secchi_m.length) ]
surveys.secchi.final[, .(Secchi_m.sd) ]



# link these to the plants dataset -----------------------------------------------

#' Now that we have secchi data for a prop of the dataset, let's link those data
#' to the plant surveys 

# merge by lake and date
plants <- merge(plants,surveys.secchi.final, by = c("SURVEY_ID","DOW", "SURVEY_DATE"), all.x = T)

#cleanup intermediate products:

rm(secchi,surveys,surveys.secchi, surveys.secchi.summer, surveys.secchi.summer.sameYr, surveys.secchi.final, surveys.secchi.summer.within1Yr, surveys.secchi.summer.within2Yr, surveys.secchi.summer.within3Yr, matching.summ)











