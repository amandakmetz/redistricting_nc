# Redistricting + Election Modeling Script 4
#
# Author: Amanda Kmetz 
# Last update: 8/5/2020
#
# Evaluating results of multiple plans using our model
# Uses MCMC random districts to create distribution of results

# clear environment and console
rm(list=ls()) 
cat("\014")  

library(tidyverse)
library(tidyr)
library(sf)

# read tract predictions
tract_predictions <- read.csv("tract_map.csv")

# read randomly generated plans
tract_config <- read.csv("nc_rand_dist.csv")

# tracts shapefile
tract_map <-st_read("NC_tracts.shp")
tract_map <- st_transform(tract_map, 26918)

#initialize
d = NULL

c_names <- colnames(tract_config)[2:1000]


# prepare fields for join
tract_map$geoid <- as.character(tract_map$GEOID)
tract_predictions$fips <- as.character(tract_predictions$fips)
tract_config$geoid <- as.character(tract_config$geoid)

# pivot longer to group by district
tract_config <- pivot_longer(tract_config, -geoid, names_to = "plan", values_to = "district")

# initialize plan name
plan = ""

# join random plans to tract predictions
nc_dist_pred <- inner_join(tract_predictions, tract_config, by = c("fips" = "geoid"), copy = FALSE, suffix = c(".x", ".y"))


# loop walks through plans, filters them, and calculates winners
# totals winners in df
for (i in 1:999){

    # set name of current plan
    # ex: plan_num = plan998
    plan_num = paste("plan", as.character(i), sep = "")
    
    # group by congressional district + count votes per district
    # ex: plan == plan998
    nc_dist_pred_cur <- nc_dist_pred %>%
      filter(plan == plan_num) %>%
      select(district, adj_pred_turnout, adj_pred_votes_dem, adj_pred_votes_rep) %>%
      group_by(district) %>%
      summarize(dist_turnout = sum(adj_pred_turnout),
                dist_dem = sum(adj_pred_votes_dem),
                dist_rep = sum(adj_pred_votes_rep))
    
    # calculate percentages and determine winners
    nc_dist_pred_cur <- nc_dist_pred_cur %>%
      mutate(dist_pct_dem = dist_dem/dist_turnout,
             dist_pct_rep = dist_rep/dist_turnout) %>%
      mutate(winner = case_when(dist_dem > dist_rep ~ "DEM",
                                dist_rep > dist_dem ~ "REP",
                                TRUE ~ "TIE"))
    
    # find number of republican districts
    rep_wins <- nc_dist_pred_cur %>%
      filter(winner == "REP")
    
    # find number of democratic districts
    dem_wins <- nc_dist_pred_cur %>%
      filter(winner == "DEM")
    
    # add results to dataframe
    d = rbind(d, data.frame(i, nrow(dem_wins), nrow(rep_wins)))
  
}

# change column names
winners <- select(d, plan = i, dem_wins = nrow.dem_wins., rep_wins = nrow.rep_wins.)

# distribution of results with random plans
ggplot(winners, aes(x = dem_wins)) +
  geom_histogram(bins = 10)

ggplot(winners, aes(x = rep_wins)) + 
  geom_histogram(bins=10)






