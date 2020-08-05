# Redistricting + Election Modeling Script 3
#
# Author: Amanda Kmetz 
# Last update: 8/5/2020
#
# Applies predictions to plans
# Calculates percentages and winners in districts
# Maps winners and votes 

# clear environment and console
rm(list=ls()) 
cat("\014")  

library(tidyverse)
library(sf)
library(tmap)


# read tracts shapefile
tract_map <-st_read("NC_tracts.shp")
tract_map <- st_transform(tract_map, 26918)

# read tract predictions
tract_predictions <- read.csv("tract_map.csv")
tract_predictions$fips <- as.character(tract_predictions$fips)
tract_map$fips <- as.character(tract_map$GEOID)

tract_map <- inner_join(tract_map, tract_predictions, by = c("fips" = "fips"), copy = FALSE, suffix = c(".x", ".y"))

# Districts shapefile
nc_dist_2016 <-st_read("NC_leg_dist_2016/2016_Contingent_Congressional_Plan_Corrected.shp")
nc_dist_2016 <- st_transform(nc_dist_2016, 26918)
plot(nc_dist_2016)


# Map of tract predictions - polygons
tm_shape(tract_map) + 
  tm_polygons("adj_pred_votes_dem", style="quantile", n = 5, legend.hist = TRUE,palette = "Blues", border.col = "black", title="Dem Votes") + 
  tm_legend(outside = TRUE) 

# Map of tract predictions - points
tract_map_cent <- st_point_on_surface(tract_map)

tm_shape(tract_map_cent) + 
  tm_dots("adj_pred_votes_dem", style="quantile", n = 5, legend.hist = TRUE,palette = "Blues", border.col = "black", title="Dem Votes") + 
  tm_legend(outside = TRUE) 

# spatial join - join district numbers to the tracts whose centroids fall within
nc_dist_pred <- st_join(tract_map_cent, nc_dist_2016, join = st_within)

# summed turnout, dem and rep votes
nc_dist_pred <- nc_dist_pred %>%
  group_by(District) %>%
  summarize(dist_turnout = sum(adj_pred_turnout),
            dist_dem = sum(adj_pred_votes_dem),
            dist_rep = sum(adj_pred_votes_rep)) 
  
# add district vote percentages and winners
nc_dist_pred <- nc_dist_pred %>%
  mutate(dist_pct_dem = dist_dem/dist_turnout,
         dist_pct_rep = dist_rep/dist_turnout) %>%
  mutate(winner = case_when(dist_dem > dist_rep ~ "DEM",
                            dist_rep > dist_dem ~ "REP",
                            TRUE ~ "TIE"))

# join votes and winners back to district geometry
nc_map_dist <- st_join(nc_dist_2016, nc_dist_pred, by = NULL, copy = FALSE, suffix = c(".x", ".y"))
                      
# map outcome of districts
tm_shape(nc_map_dist) + 
  tm_polygons(col = "winner", palette = "-RdBu", border.col = "black", title="District Outcomes") + 
  tm_layout(main.title="Outcomes of Legislative Districts using Model Predictions", 
            scale=1, main.title.fontface = "bold", main.title.size=1, main.title.position = "center", 
            legend.title.size = 2, legend.text.size = 1) +
  tm_legend(outside = TRUE)

# write to shapefile
st_write(nc_map_dist, "nc_map_dist_outcomes.shp")




