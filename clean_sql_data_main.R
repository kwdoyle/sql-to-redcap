source("./Rscripts/clean_sql_data_func.R")
library(tidyverse)


# read all files at once and then lapply prepareData to them
# then clean out the mrn columns if they exist
# and rename the date column if need be

# to find which event to use, search the dictionary for like, the "main column names" without the number
# e.g., "BI".
# then get the single, unique Form.Name
# and then use that form name in the events and get the unique_event_name
# NOTE: if the csv file has "12month" or "3month" in it, then there
# will likely be 2 event_names, one with 12 month and the other with 3 month--use the respective one.

# then run ReChooseRCEvents and if there's more than one except for the basicinfo/contactinfo,
# then save as each individual table
# only need to do this for the hc table.


# using the current database-to-enter-into's metadata
dict <- read.csv("./SHOPDatabase_DataDictionary_2022-09-13.csv")
events <- read.csv("./SHOPDatabase_InstrumentDesignations_2022-09-13.csv")

allcsvs <- list.files("./", "*from_sql.csv", recursive = T)
dfs_clean <- lapply(allcsvs, processFiles, d=dict, e=events)

baseflnms <- gsub("\\.csv", "", unlist(lapply(strsplit(allcsvs, '\\/'), tail, 1L)))
names(dfs_clean) <- baseflnms
savenms <- paste0('cleaned_data/', baseflnms, "_cleaned.csv")


# convert these variables into their new format for redcap.
# essentially turning each value into its own variable
aneur <- dfs_clean$hospitalcomp_from_sql %>%
  select(id, hc18) %>%
  mutate(newvar = paste0("hc18___", hc18),
         newval = 1) %>%
  pivot_wider(names_from="newvar", values_from="newval") %>%
  select(-hc18, -hc18___NA) %>%
  distinct()
aneur$redcap_event_name <- "aneurysm_repair_arm_1"

cool <- dfs_clean$hospitalcomp_from_sql %>%
  select(id, hc69o) %>%
  mutate(newvar = paste0("hc69o___", hc69o),
         newval = 1) %>%
  pivot_wider(names_from="newvar", values_from="newval") %>%
  select(-hc69o, -hc69o___NA) %>%
  distinct()
cool$redcap_event_name <- "icu_stay_arm_1"

# now remove these columns from those tables
dfs_clean$hospitalcomp_from_sql <- dfs_clean$hospitalcomp_from_sql %>% 
  select(-hc18, -hc69o) 

# have to re-save tables with the correct "event name"
# then can remove the hc18 and hc69o
hc_dfs <- ReChooseRCEvents(tab=dfs_clean$hospitalcomp_from_sql, dictt=dict, eventz=events)



# save stuff
sapply(names(hc_dfs), function(x) {
  deef <- hc_dfs[[x]]
  savenm <- paste0('cleaned_data/hc_tables/', x, "_cleaned.csv")
  write.csv(deef, savenm, row.names=F, na="")
})

write.csv(aneur, "./cleaned_data/hc_tables/aneurysm_from_sql_cleaned.csv", row.names=F, na="")
write.csv(cool, "./cleaned_data/hc_tables/cooling_from_sql_cleaned.csv", row.names=F, na="")

# now remove the hc table from here and then save
dfs_clean2 <- dfs_clean[-which(names(dfs_clean) == "hospitalcomp_from_sql")]

sapply(names(dfs_clean2), function(x) {
  deef <- dfs_clean2[[x]]
  savenm <- paste0('cleaned_data/', x, "_cleaned.csv")
  write.csv(deef, savenm, row.names=F, na="")
})


# check if anything still missing
alldatavars <- unique(c(unlist(lapply(dfs_clean2, names)), unlist(lapply(hc_dfs, names))))
missing_vars <- unique(dict$Variable...Field.Name)[which(!unique(dict$Variable...Field.Name) %in% alldatavars)]
# search raw data for specific variables
# chk <- checkRawDat(x=allcsvs, varchk="HC74") #"ST1")

# Ok, looks like everything's that's still "missing" are fields that aren't in the SQL database to begin with
# e.g., the date fields of some variables. there was a spot on the forms for them, but they were never in the SQL database
