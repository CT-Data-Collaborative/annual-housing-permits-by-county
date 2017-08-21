library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)

##################################################################
#
# Processing Script for Annual Housing Permits by County
# Created by Jenna Daly
# On 08/21/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
permit_data <- dir(path_to_raw, recursive=T, pattern = "xls")

#Read in data
permits <- read_excel(paste0(path_to_raw, "/", permit_data), sheet=1, skip=3)

#Set column names
names(permits)[names(permits) == "State/County"] <- "County"

#Remove blank line
permits <- permits[!is.na(permits$County),]

#Rename county names
permits$County <- gsub("$", " County", permits$County)
permits$County[permits$County == "Connecticut County"] <- "Connecticut"

#Convert to long format
last_col <- ncol(permits)
permits_long <- gather(permits, Year, Value, 2:last_col, factor_key=FALSE)

# Derive percent of total permits
state <- permits_long[permits_long$County == "Connecticut",]
percents <- permits_long[permits_long$County != "Connecticut",]

percents <- merge(percents, state, by = "Year")

percents <- percents %>% 
  mutate(Value = round((Value.x / Value.y)*100, 1)) %>% 
  select(Year, County.x, Value) 

# Add measure types, Variable = "Housing Permits"
permits_long$`Measure Type` <- "Number"
permits_long$Variable <- "Housing Permits"

percents$`Measure Type` <- "Percent"
percents$Variable <- "Housing Permits"

names(percents)[names(percents) == "County.x"] <- "County"

# bind data together
permits_long <- rbind(permits_long, percents)

#Merge in FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
fips <- (county_fips_dp$data[[1]])

permits_long_fips <- merge(permits_long, fips, by = "County", all=T)

# Reorder columns
permits_long_fips <- permits_long_fips %>% 
  select(County, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(County, Year, `Measure Type`)

# Write to File
write.table(
  permits_long_fips,
  file.path(getwd(), "data", "annual-housing-permits-by-county_2016.csv"),
  sep = ",",
  row.names = F,
  na = "-9999"
)

