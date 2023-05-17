#Load Mike's current leaderboard file.
lakes.summ = read.csv("data&scripts/data/output/lakes_summ.csv")

#Let's replace all NAs in the surveyors column with "Anonymous"
lakes.summ$surveyorlist[is.na(lakes.summ$surveyorlist)] = "Unnamed hardworking surveyor(s)"



#Create from it a leaderboard based on surveyors

#Slam everyone into a single string.
people.list = paste(lakes.summ$surveyorlist,collapse = ",")

#Do substitutions--1st argument string becomes second argument string.
#First, replacing non-descript things or institution names with our unnamed string.
people.list = gsub("DNR Fisheries", "MN DNR", people.list, fixed=T)
people.list = gsub("DNR Staff", "MN DNR", people.list, fixed=T)
people.list = gsub("Unk,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("?,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("NA,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("MN DNR,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Blue Water Science,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Ramsey Co conservation district", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Endangered Resource Services,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("RWMWD Staff,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Ramsey Conservation District,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)
people.list = gsub("Three Rivers Park District,", "Unnamed hardworking surveyor(s),", people.list, fixed = T)

#Replace random gaps and random punctuation with clean single commas.
people.list = gsub("  ", " ", people.list, fixed=T)
people.list = gsub(" ,", ",", people.list, fixed=T)
people.list = gsub(";", ",", people.list, fixed=T)
people.list = gsub(",,,", ",", people.list, fixed=T)
people.list = gsub(",,", ",", people.list, fixed=T)
people.list = gsub(", ", ",", people.list, fixed=T)
people.list = gsub(",  ", ",", people.list, fixed=T)
people.list = gsub(" and ", ",", people.list, fixed=T)
people.list = gsub(".", "", people.list, fixed=T)
people.list = gsub(" with ", ",", people.list, fixed=T)

#Replace inconsistent names
people.list = gsub("A Doll", "Adam Doll", people.list, fixed=T)
people.list = gsub("B Hummel", "Brittany Hummel", people.list, fixed=T)
people.list = gsub("A Londo", "April Londo", people.list, fixed=T)
people.list = gsub("Eric F,", "Eric Fieldseth,", people.list, fixed=T)
people.list = gsub("Angie Les", "Angela Les", people.list, fixed=T)
people.list = gsub("Hallie M Jensen", "Hallie Jensen", people.list, fixed=T)
people.list = gsub("Josh Curtin", "Joshua Curtin", people.list, fixed=T)
people.list = gsub("J Bjorklund", "Jill Bjorklund", people.list, fixed=T)
people.list = gsub(",Jill B,", ",Jill Bjorklund,", people.list, fixed=T)
people.list = gsub("Brunell,", "Brunelle,", people.list, fixed=T)#***
people.list = gsub("James A Johnson", "James Johnson", people.list, fixed=T)
people.list = gsub("JA Johnson", "James Johnson", people.list, fixed=T)
people.list = gsub("Emmy Hauck", "Emelia Hauck Jacobs", people.list, fixed=T)
people.list = gsub("Jonathan D JaKa", "John Jaka", people.list, fixed=T)
people.list = gsub("Josh Knopik", "Joshua Knopik", people.list, fixed=T)
people.list = gsub("Joshua M Knopik", "Joshua Knopik", people.list, fixed=T)
people.list = gsub("Katie W,", "Katie Wigen,", people.list, fixed=T)
people.list = gsub("K Lund", "Keegan Lund", people.list, fixed=T)
people.list = gsub("Kelly D,", "Kelly Dooley,", people.list, fixed=T)
people.list = gsub("Kris Carlson", "Kristin Carlson", people.list, fixed=T)
people.list = gsub("K Bloodsworth", "Kylie Bloodsworth", people.list, fixed=T)
people.list = gsub("K Cattoor", "Kylie Cattoor", people.list, fixed=T)
people.list = gsub("Kailey K,", "Kailey Kreatz,", people.list, fixed=T)
people.list = gsub("M Verhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Mike Verhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Nik Myrha,", "Nik Myrha-Edwards,", people.list, fixed=T)#***
people.list = gsub("Yvette C,", "Yvette Christianson,", people.list, fixed=T)
people.list = gsub("Nick Whichello", "Nick Wichello", people.list, fixed=T)#***
people.list = gsub("Todd P,", "Todd Piepho,", people.list, fixed=T)#***
people.list = gsub("Marty Evans", "Martin Evans", people.list, fixed=T)
people.list = gsub("R Contreras,", "Rafa Contreras,", people.list, fixed=T)
people.list = gsub("R Roche", "Rochelle Roche", people.list, fixed=T)
people.list = gsub("Rafa Contrera,", "Rafa Contreras,", people.list, fixed=T)
people.list = gsub("Allly", "Ally", people.list, fixed=T)
people.list = gsub("Carstenen", "Carstensen", people.list, fixed=T)
people.list = gsub("Jill,", "Jill Bjorklund,", people.list, fixed=T)
people.list = gsub(",Crowell,", ",Wendy Crowell,", people.list, fixed=T)
people.list = gsub(",Kailey,", ",Kailey Kreatz,", people.list, fixed=T)
people.list = gsub("LW,", "Lucas Wandrie,", people.list, fixed=T)
people.list = gsub(",Drake", ",Christa Drake", people.list, fixed=T)
people.list = gsub(",Einck", ",Alan Einck", people.list, fixed=T)
people.list = gsub(",Hennen", ",Matt Hennen", people.list, fixed=T)
people.list = gsub(",JN,", ",Joe Norman,", people.list, fixed=T)
people.list = gsub(",Mottl", ",E Mottl", people.list, fixed=T)
people.list = gsub(",Proulx", ",Nick Proulx", people.list, fixed=T)
people.list = gsub(",Sean,", ",Sean Sissler,", people.list, fixed=T)
people.list = gsub(",Travis", ",Mitch Travis", people.list, fixed=T)
people.list = gsub(",Uhler", ",Kyle Uhler", people.list, fixed=T)
people.list = gsub("Yvetter,", "Yvette Christianson,", people.list, fixed=T)
people.list = gsub("Eric,", ",Eric Fieldseth,", people.list, fixed=T)
people.list = gsub(",Adam,", ",Adam Doll,", people.list, fixed=T)
people.list = gsub(",Eisterhold", ",Joe Eisterhold", people.list, fixed=T)
people.list = gsub(",Haworth,", ",Matt Horwath,", people.list, fixed=T)
people.list = gsub("Matt,", "Matt Hennen,", people.list, fixed=T)
people.list = gsub("Mike Veerhoven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("MVerhoeven", "Michael Verhoeven", people.list, fixed=T)
people.list = gsub("Scott M,", "Scott Mackenthun,", people.list, fixed=T)
people.list = gsub("AR,", "Adam Rollins,", people.list, fixed=T)
people.list = gsub("CP,", "Christine Powell,", people.list, fixed=T)
people.list = gsub("M Bockman,", "M Bokman,", people.list, fixed=T)

#Get rid of odd substrings here and there.
people.list = gsub("LLC", "", people.list, fixed = T)
people.list = gsub(" (first part of day)", "", people.list, fixed = T)
people.list = gsub(" (UMN)", "", people.list, fixed = T)
people.list = gsub("(7/16 only)", "", people.list, fixed = T)
people.list = gsub("/", ",", people.list, fixed=T)
people.list = gsub(" (Lincoln County)", "", people.list, fixed = T)
people.list = gsub("(Scott Co)", "", people.list, fixed = T)

#Create a vector now out of the string, chopping at every single comma.
people.list2 = unlist(str_split(people.list, ",", simplify = FALSE))

#Get rid of random blanks and an E and some random issues not already cleared up by the above.
people.list2 = people.list2[people.list2 != ""]
people.list2 = people.list2[people.list2 != "E"]
people.list2[people.list2 == "Jill"] = "Jill Bjorklund"
people.list2[people.list2 == "Kailey"] = "Kailey Kreatz"
people.list2[people.list2 == "Ramsey Co conservation district"] = "Unnamed hardworking surveyor(s)"

#Take all our first-name-only entries and replace with our unnamed string.
people.list2[people.list2 %in% c("Marcie", "Wenck", "Alex", "Andy", "Tom", "Chase", "Mulu", "Jake", "Bri", "Chris", "Marissa","Weston", "Johanna", "DA", "RP")] = "Unnamed hardworking surveyor(s)" 

#Count up everybody
leaderboard.df = data.frame(table(people.list2)) %>% 
  arrange(desc(Freq))

#Name the resulting columns.
names(leaderboard.df) = c("Surveyor", "Number of surveys")

