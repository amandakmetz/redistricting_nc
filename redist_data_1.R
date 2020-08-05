# Redistricting + Election Modeling Script 1
#
# Author: Amanda Kmetz 
# Last update: 8/5/2020
#
# Downloads census demographic variables
# Imports, filters and pivots NC election results
# Joins demographic and election variables
# Exports shapefiles for regression modeling and visualization

# clear environment and console
rm(list=ls()) 
cat("\014")  

# load packages
library(sf)
library(stringr)
library(tidycensus)
library(tidyverse)
library(tmap)

## load Census API key if not already in environment

# list all acs_vars for 2016
acs_vars <- load_variables(2016, "acs5", cache = FALSE)

# ----- COUNTY DEMOGRAPHICS

# Define variables of interest: 
# Total population,
# White, Black, Asian, Hispanic, 
# Naturalized Citizens, Non-Citizens, 
# Medhhinc12mo, Median Age, Geographic Mobility,
# %BAs, %PHD, 25+ Population, 
# Agriculture/Fishing/Hunting/Mining/Forestry Sector, Information Sector, Total Employed, 
# Public Transit, Walk to work, Bike to work, Total workers

dem_vars = c("B01003_001",
             "B03002_003", "B03002_004", "B03002_006", "B03002_012", 
             "B05001_005", "B05001_006", 
             "B19013_001", "B01002_001", "B07001_001",
             "B15003_022", "B15003_025", "B15003_001",
             "C24070_002", "C24070_008", "C24050_001", 
             "B08301_010", "B08301_019", "B08301_018", "B08301_001")


# download NC county level data for the 2016 5-year ACS

nc_counties = get_acs(
  geography = "county",
  state = "NC",
  variables = dem_vars, 
  year = 2016, 
  output = "wide", 
  geometry = TRUE
)



# rename columns and drop margins of error
nc_counties = select(nc_counties, fips = GEOID, name = NAME, 
                     totpop = B01003_001E, 
                     white = B03002_003E, black = B03002_004E, asian = B03002_006E, hispanic = B03002_012E,
                     naturalized = B05001_005E, noncitizen = B05001_006E,
                     medhhinc = B19013_001E, medage = B01002_001E, mobility = B07001_001E, 
                     bachelors = B15003_022E, phd = B15003_025E, over25 = B15003_001E,
                     aghuntfishmine = C24070_002E, information = C24070_008E, totalemployed = C24050_001E, 
                     pubtrans = B08301_010E, walkwork = B08301_019E, bikework = B08301_018E, totalworkers = B08301_001E)

# calculate percentages for absolute variables
nc_counties <- nc_counties %>% mutate(
  name = str_to_upper(str_replace(name, " County, North Carolina", "")),
  pct_white = white / totpop,
  pct_black = black / totpop,
  pct_asian = asian / totpop,
  pct_hisp = hispanic / totpop,
  pct_naturalized = naturalized / totpop,
  pct_noncitizen = noncitizen / totpop,
  pct_bachelors = bachelors / over25,
  pct_phd = phd / over25,
  pct_aghuntfishmine = aghuntfishmine / totalemployed,
  pct_information = information / totalemployed,
  pct_mobility = mobility / totpop,
  pct_pubtrans = pubtrans / totalworkers,
  pct_walkwork = walkwork / totalworkers,
  pct_bikework = bikework / totalworkers
)


# ----- CENSUS TRACT DEMOGRAPHICS

# variables of interest are the same

# download NC tract level data for the 2016 5-year ACS
nc_tracts = get_acs(
  geography = "tract",
  state = "NC",
  variables = dem_vars, 
  year = 2016, 
  output = "wide", 
  geometry = TRUE
)

# rename columns and drop margins of error
nc_tracts = select(nc_tracts, fips = GEOID, name = NAME, 
                   totpop = B01003_001E, 
                   white = B03002_003E, black = B03002_004E, asian = B03002_006E, hispanic = B03002_012E,
                   naturalized = B05001_005E, noncitizen = B05001_006E,
                   medhhinc = B19013_001E, medage = B01002_001E, mobility = B07001_001E, 
                   bachelors = B15003_022E, phd = B15003_025E, over25 = B15003_001E,
                   aghuntfishmine = C24070_002E, information = C24070_008E, totalemployed = C24050_001E, 
                   pubtrans = B08301_010E, walkwork = B08301_019E, bikework = B08301_018E, totalworkers = B08301_001E)

# calculate percentages for absolute variables
nc_tracts <- nc_tracts %>% mutate(
  pct_white = white / totpop,
  pct_black = black / totpop,
  pct_asian = asian / totpop,
  pct_hisp = hispanic / totpop,
  pct_naturalized = naturalized / totpop,
  pct_noncitizen = noncitizen / totpop,
  pct_bachelors = bachelors / over25,
  pct_phd = phd / over25,
  pct_aghuntfishmine = aghuntfishmine / totalemployed,
  pct_information = information / totalemployed,
  pct_mobility = mobility / totpop,
  pct_pubtrans = pubtrans / totalworkers,
  pct_walkwork = walkwork / totalworkers,
  pct_bikework = bikework / totalworkers
)


# ----- NC ELECTION RESULTS

# read csv of 2016 NC Election results 
nc_results <- read.csv("NC_results.csv")
head(nc_results)


# make column a character
nc_results$party <- as.character(nc_results$party)

# filter to only presidential results from listed parties, drop choice name
results_pres <- nc_results %>%
  filter(str_detect(name, "US PRESIDENT",)) %>%
  select(county, date, precinct, groupid, type, name, party, totalvotes)

# replace blank party names with write-in
results_pres <- results_pres %>%
  mutate(party = replace(party, party == "", "WRITEIN"))


# total county votes for each party
results_county <- results_pres %>%
  select(county, party, totalvotes) %>%
  group_by(county, party) %>%
  summarize(county_votes = sum(totalvotes))

# pivot wider to have one row for each county
results_county <- pivot_wider(results_county, 
                                 names_from = party,
                                 values_from = county_votes)

# add vote totals for all parties
results_county <- results_county %>%
  mutate(county_total_votes = REP + DEM + LIB + WRITEIN)


# ----- JOIN ELECTION RESULTS + DEMOGRAPHIC VARIABLES

# make join criteria characters not factors
results_county$county <- as.character(results_county$county)

# inner join all election results to demographic variables
nc_vars <- inner_join(nc_counties, results_county, by = c("name" = "county"),copy = FALSE, suffix = c(".x", ".y"))

# calculate pcts for each party
nc_vars <- nc_vars %>% mutate(
  pct_voted = county_total_votes / totpop,
  pct_dem = DEM / county_total_votes,
  pct_rep = REP / county_total_votes
)

# narrow down columns
nc_vars <- nc_vars %>%
  select(fips, name, totpop, medhhinc, medage, 
         pct_white, pct_black, pct_asian, pct_hisp, 
         pct_naturalized, pct_noncitizen, pct_bachelors, pct_phd, pct_mobility,
         pct_aghuntfishmine, pct_information,
         pct_pubtrans, pct_walkwork, pct_bikework, 
         county_total_votes, DEM, REP,
         pct_voted, pct_dem, pct_rep)

# write to shp
st_write(nc_vars, "nc_county_vars.shp")

# same for tracts
nc_tracts <- nc_tracts %>%
  select(fips, name, totpop, medhhinc, medage, 
         pct_white, pct_black, pct_asian, pct_hisp, 
         pct_naturalized, pct_noncitizen, pct_bachelors, pct_phd, pct_mobility,
         pct_aghuntfishmine, pct_information,
         pct_pubtrans, pct_walkwork, pct_bikework)

# write to shp
st_write(nc_tracts, "nc_tract_vars.shp")




