# Redistricting + Election Modeling Script 2
#
# Author: Amanda Kmetz 
# Last update: 8/5/2020
#
# Imports demographic variables + election results in NC shapefiles
# Visualizes distribution + correlations
# Fits models using stepwise regression
# Makes predictions for votes at the census tract level
# Adjusts predictions to match accurate county vote totals
#
# Stepwise Regression following STHDA
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

# clear environment and console
rm(list=ls()) 
cat("\014")  

library(tidyverse) # for data manipulation, contains dplyr and ggplot2
library(sf)
library(tmap)
library(tmaptools)
library(corrplot) #for correlations uses source line directly below
source("http://www.sthda.com/upload/rquery_cormat.r")
library(ppcor) # for partial correlations
library(plotly) # for parallel coordinate plot and 3d scatter
library(caret) # for easy machine learning workflow
library(leaps) # for computing stepwise regression

# ----- READ IN DATA

# county shapefile + demographics + election results
county_map <-st_read("nc_county_vars.shp")

# tract shapefile + demographics
tract_map <-st_read("nc_tract_vars.shp")
tract_map <- na.omit(tract_map)

# project to utm zone 18n
county_map <- st_transform(county_map, 26918)
tract_map <- st_transform(tract_map, 26918)
  

# ----- DESCRIPTIVES + PLOTS

# descriptives
str(county_map)
head(county_map)

#plots of independent variables
plot(county_map$pct_dem)
plot(county_map$pct_rep)
plot(county_map$pct_vtd)


# histograms of independent variables
ggplot(county_map, aes(x = pct_dem)) +
  geom_histogram(bins = 10)

ggplot(county_map, aes(x = pct_rep)) +
  geom_histogram(bins = 10)

ggplot(county_map, aes(x = pct_vtd)) +
  geom_histogram(bins = 10)

# box plots of independent variables

# dem results
ggplot(county_map, aes(y = pct_dem)) +
  geom_boxplot()+
  scale_y_continuous(name = "% Democrat", 
                     breaks = seq(0, 1, .1),
                     limits=c(0, 1)) +
  scale_x_discrete() +
  ggtitle("Democratic Votes in NC Counties, 2016 Presidential Election")

# rep results
ggplot(county_map, aes(y = pct_rep)) +
  geom_boxplot()+
  scale_y_continuous(name = "% Republican", 
                     breaks = seq(0, 1, .1),
                     limits=c(0, 1)) +
  scale_x_discrete() +
  ggtitle("Republican Votes in NC Counties, 2016 Presidential Election")

# voter turnout
ggplot(county_map, aes(y = pct_vtd)) +
  geom_boxplot()+
  scale_y_continuous(name = "% of Population Voting", 
                     breaks = seq(0, 1, .1),
                     limits=c(0.2, 0.7)) +
  scale_x_discrete() +
  ggtitle("Voter Turnout in NC Counties, 2016 Presidential Election")


# ----- CORRELATIONS

# drop geometry to use data frame in correlations
county_df <- as.data.frame(st_drop_geometry(county_map))

# create subsets

# all variables
subset_all <-as.data.frame(county_df[, c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23, 24, 25)])

# dem vote correlations - use for model specification
subset_dem <-as.data.frame(county_df[, c(24, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)])

# rep vote correlations - use for model specification
subset_rep <-as.data.frame(county_df[, c(25, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)])

# voter turnout correlations - use for model specification
subset_turnout <-as.data.frame(county_df[, c(23, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)])


# correlation visualization 
rquery.cormat(subset_dem)
rquery.cormat(subset_rep)
rquery.cormat(subset_turnout)


# correlation coefficients
cor(subset_dem)
cor(subset_rep)
cor(subset_turnout)

# scatterplot of select x and y
p <- plot_ly (
  x = county_map$pct_phd, 
  y = county_map$pct_voted,
  mode='markers')

p


# scatterplot matrix
pairs(~pct_dem + pct_wht + pct_blc + medage + pct_phd + pct_pbt, data=county_df, main="Scatterplot Matrix: Democratic Support")
pairs(~pct_rep + pct_wht + pct_blc + medage + pct_phd + pct_pbt, data=county_df, main="Scatterplot Matrix: Republican Support")
pairs(~pct_vtd + pct_wht + pct_blc + medage + pct_phd + pct_pbt, data=county_df, main="Scatterplot Matrix: Voter Turnout")


# ----- MODEL SPECIFICATION

# stepwise regression 3 different methods: 
# 1. stepAIC (MASS package) 
# 2. regsubsets (leaps package) 
# 3. trainControl (caret package + MASS/leaps)
# models specified for dem support, republican support and voter turnout

# fit full models
full_model_dem <- lm(pct_dem ~., data = subset_dem)
full_model_rep <- lm(pct_rep ~., data = subset_rep)
full_model_turnout <- lm(pct_vtd ~., data = subset_turnout)

# full model output
summary(full_model_dem)
summary(full_model_rep)
summary(full_model_turnout)

# -- 1. stepwise regression model by AIC (MASS package)
# set direction to forward/backward/both
# trace = TRUE shows history of steps
step_model_dem1 <- stepAIC(full_model_dem, direction = "both", trace = FALSE)
summary(step_model_dem1)

step_model_rep1 <- stepAIC(full_model_rep, direction = "both", trace = FALSE)
summary(step_model_rep1)

step_model_turnout1 <- stepAIC(full_model_turnout, direction = "both", trace = FALSE)
summary(step_model_turnout1)

# -- 2. stepwise regression model by nvmax (leaps package)
# set nvmax to max number of variables - finds best model at each number
# need to compare model output of each to find best fit
models_dem <- regsubsets(pct_dem~., data = subset_dem, nvmax = 5, method = "seqrep")
summary(models_dem)

models_rep <- regsubsets(pct_rep~., data = subset_rep, nvmax = 5, method = "seqrep")
summary(models_rep)

models_turnout <- regsubsets(pct_vtd~., data = subset_turnout, nvmax = 5, method = "seqrep")
summary(models_turnout)

# -- 3. stepwise regression with trainControl (caret package - using leaps and MASS)
# set nvmax as above, and find the model with the best RMSE

# Set seed for reproducibility
set.seed(100)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the models
step_model_dem2 <- train(pct_dem ~., data = subset_dem,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)

step_model_rep2 <- train(pct_rep ~., data = subset_rep,
                        method = "leapSeq", 
                        tuneGrid = data.frame(nvmax = 1:5),
                        trControl = train.control
)

step_model_turnout2 <- train(pct_vtd ~., data = subset_turnout,
                        method = "leapSeq", 
                        tuneGrid = data.frame(nvmax = 1:5),
                        trControl = train.control
)

# get results - RMSE, R2 of all models
step_model_dem2$results
step_model_rep2$results
step_model_turnout2$results

# determine nvmax of best model
step_model_dem2$bestTune
step_model_rep2$bestTune
step_model_turnout2$bestTune

# preview the variables and coefficients in best model
summary(step_model_dem2$finalModel)
coef(step_model_dem2$finalModel, 3)

summary(step_model_rep2$finalModel)
coef(step_model_rep2$finalModel, 3)

summary(step_model_turnout2$finalModel)
coef(step_model_turnout2$finalModel, 5)


# ----- FITTING MODELS


# specify models (from stepAIC)
mod_dem1 <- lm(pct_dem ~ medhhnc + medage + pct_wht + pct_blc + pct_bch + pct_phd, data=county_map)
mod_rep1 <- lm(pct_rep ~ medhhnc + pct_wht + pct_blc + pct_asn + pct_bch + pct_phd, data=county_map)
mod_turnout1 <- lm(pct_vtd ~ medage + pct_wht + pct_blc + pct_ntr + pct_bch + pct_phd + pct_ghn, data=county_map)

# some alternative models (from stepwise using caret)
mod_dem2 <- lm(pct_dem ~ pct_blc + pct_bch + pct_phd, data=county_map)
mod_rep2 <- lm(pct_rep ~ pct_blc + pct_bch + pct_phd, data=county_map)
mod_turnout2 <- lm(pct_vtd ~ medage + pct_hsp + pct_ntr + pct_bch + pct_phd, data=county_map)


# Summarize models
summary(mod_dem1)
summary(mod_rep1)
summary(mod_turnout1)

summary(mod_dem2)
summary(mod_rep2)
summary(mod_turnout2)

# Other useful functions using models (stats package)
coefficients(mod_dem1) # model coefficients
confint(mod_dem1, level=0.95) # CIs for model parameters 
fitted(mod_dem1) # predicted values
residuals(mod_dem1) # residuals
anova(mod_dem1) # anova table 
vcov(mod_dem1) # covariance matrix for model parameters 
influence(mod_dem1) # regression diagnostics

# ----- MODEL PREDICTIONS

# predicting dem + rep support + turnout in census tracts
tract_map$pred_pct_dem <- predict(mod_dem1, select(tract_map, medhhnc, medage, pct_wht, pct_blc, pct_bch, pct_phd))
tract_map$pred_pct_rep <- predict(mod_rep1, select(tract_map, medhhnc, pct_wht, pct_blc, pct_asn, pct_bch, pct_phd))
tract_map$pred_pct_vtd <- predict(mod_turnout1, select(tract_map, medage, pct_wht, pct_blc, pct_ntr, pct_bch, pct_phd, pct_ghn))

# finding exact number of predicted votes
tract_map <- tract_map %>% mutate(
  pred_turnout = totpop * pred_pct_vtd,
  pred_votes_dem = pred_turnout * pred_pct_dem,
  pred_votes_rep = pred_turnout * pred_pct_rep
) 

# ----- ADJUSTING PREDICTIONS

# total predicted votes for each party do not match reported votes
# need to calculate coefficient for converting predictions to accurate county totals

# get each tract's county fips code
tract_map <- tract_map %>% mutate(
  county_fips = substring(fips, 1, 5)
)

tract_df <- st_drop_geometry(tract_map)

# summarize total predicted dem/rep votes by county

county_pred <- tract_df %>%
  select(county_fips, pred_votes_dem, pred_votes_rep, pred_turnout) %>%
  group_by(county_fips) %>%
  summarize(pred_votes_dem_county = sum(pred_votes_dem),
            pred_votes_rep_county = sum(pred_votes_rep),
            pred_turnout_county = sum(pred_turnout))

# join actual votes to predictions in the counties : votes are off (DEM vs. pred_votes_dem_county)

county_df$fips <- as.character(county_df$fips)
county_df <- county_df %>%
  select(fips,
         DEM, REP,
         totalvotes = cnty_t_,
         pct_dem, pct_rep, pct_vtd)

county_pred <- inner_join(county_pred, county_df,by = c("county_fips" = "fips"), copy = FALSE, suffix = c(".x", ".y"))

# find the adjustment coefficient for each county 
# to multiply each tract's predictions by for a more accurate result
county_pred <- county_pred %>% 
  mutate(adj_coef_dem = DEM / pred_votes_dem_county) %>%
  mutate(adj_coef_rep = REP / pred_votes_rep_county) %>%
  mutate(adj_coef_turnout = totalvotes / pred_turnout_county) %>%
  select(county_fips, adj_coef_dem, adj_coef_rep, adj_coef_turnout)

# join coefficients back to tract predictions + attributes
# apply adjustment coefficient to each tract's predictions
tract_map <- inner_join(tract_map, county_pred, by = c("county_fips" = "county_fips"), copy = FALSE, suffix = c(".x", ".y"))
tract_map <- tract_map %>%
  mutate(adj_pred_votes_dem = pred_votes_dem * adj_coef_dem) %>%
  mutate(adj_pred_votes_rep = pred_votes_rep * adj_coef_rep) %>%
  mutate(adj_pred_turnout = pred_turnout * adj_coef_turnout) 


# aggregate tracts to counties to see county total predictions vs the adjusted predictions
tract_predictions_agg <- st_drop_geometry(tract_map) %>%
  select(county_fips, pred_votes_dem, adj_pred_votes_dem, pred_votes_rep, adj_pred_votes_rep, pred_turnout, adj_pred_turnout) %>%
  group_by (county_fips) %>%
  summarize(predicted_dem = sum(pred_votes_dem),
            adjusted_dem = sum(adj_pred_votes_dem),
            predicted_rep = sum(pred_votes_rep),
            adjusted_rep = sum(adj_pred_votes_rep),
            predicted_turnout = sum(pred_turnout),
            adjusted_turnout = sum(adj_pred_turnout))

# join original county totals to double check
# adjusted votes and turnout should match actual votes and turnout
tract_predictions_agg <- inner_join(tract_predictions_agg, county_df, by = c("county_fips" = "fips"), copy = FALSE, suffix = c(".x", ".y"))

tract_predictions_agg <- tract_predictions_agg %>%
  select(county_fips,
         predicted_dem, adjusted_dem, actual_dem = DEM,
         predicted_rep, adjusted_rep, actual_rep = REP,
         predicted_turnout, adjusted_turnout, actual_turnout = totalvotes)

# calculate adjusted dem/rep support and turnout

tract_map <- tract_map %>%
  mutate(adj_pct_dem = adj_pred_votes_dem / adj_pred_turnout,
         adj_pct_rep = adj_pred_votes_rep / adj_pred_turnout,
         adj_pct_turnout = adj_pred_turnout / totpop)


# ----- PREDICTION MAPS

# map of turnout by county
tm_shape(county_map) + 
  tm_polygons("pct_vtd", style="quantile", n = 5, palette = "Purples", border.col = "black", title="% Turnout") + 
  tm_layout(main.title="NC Turnout", scale=1, main.title.fontface = "bold", main.title.size=1, main.title.position = "center", legend.title.size = 2, legend.text.size = 1) +
  tm_legend(outside = TRUE)


# map of intitial tract predictions - dem/rep support + turnout
tm_shape(tract_map) + 
  tm_polygons("pred_pct_dem", style="quantile", n = 5, legend.hist = TRUE,palette = "Blues", border.col = "black", title="Predicted % Democrat") + 
  tm_legend(outside = TRUE) 

tm_shape(tract_map) + 
  tm_polygons("pred_pct_rep", style="quantile", n = 5, legend.hist = TRUE,palette = "Reds", border.col = "black", title="Predicted % Republican") + 
  tm_legend(outside = TRUE) 

tm_shape(tract_map) + 
  tm_polygons("pred_pct_vtd", style="quantile", n = 5, legend.hist = TRUE,palette = "Purples", border.col = "black", title="Predicted % Turnout") + 
  tm_legend(outside = TRUE) 

# map of adjusted tract predictions  

tm_shape(tract_map) + 
  tm_polygons("adj_pct_dem", style="quantile", n = 5, palette = "Blues", border.col = "black", title="Predicted % Dem") + 
  tm_layout(main.title="Adjusted Predicted % Democratic Support in NC Census Tracts, 2016", 
            scale=1, main.title.fontface = "bold", main.title.size=1, main.title.position = "center", 
            legend.title.size = 2, legend.text.size = 1) +
  tm_legend(outside = TRUE)

tm_shape(tract_map) + 
  tm_polygons("adj_pct_rep", style="quantile", n = 5, palette = "Reds", border.col = "black", title="Predicted % Rep") + 
  tm_layout(main.title="Adjusted Predicted % Republican Support in NC Census Tracts, 2016", 
            scale=1, main.title.fontface = "bold", main.title.size=1, main.title.position = "center", 
            legend.title.size = 2, legend.text.size = 1) +
  tm_legend(outside = TRUE)

tm_shape(tract_map) + 
  tm_polygons("adj_pct_turnout", style="quantile", n = 5, palette = "Greens", border.col = "black", title="Predicted % Turnout") + 
  tm_layout(main.title="Adjusted Predicted Voter Turnout in NC Census Tracts, 2016", 
            scale=1, main.title.fontface = "bold", main.title.size=1, main.title.position = "center", 
            legend.title.size = 2, legend.text.size = 1) +
  tm_legend(outside = TRUE)

tract_map$fips <- as.character(tract_map$fips)
tract_map$name <- as.character(tract_map$name)

# write csv/shapefile
write_csv(st_drop_geometry(tract_map), "tract_map.csv")
st_write(tract_map, "tract_map.shp")
