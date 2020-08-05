Redistricting Project - 2016 NC Election Data

Author: Amanda Kmetz
Last Updated: 8/5/2020

Project R files should be run in the following order:

1. redist_data_1.R
	
	- Dependencies: NC_results.csv (NC 2016 Election Results)

2. redist_model_2.R

	- Dependencies: nc_county_vars.shp (county map, created by redist_data_1.R)
			nc_tract_vars.shp (tract map, created by redist_data_1.R)

3. redist_apply_3.R

	- Dependencies: NC_tracts.shp (tract shapefile)
			tract_map.csv (tract predictions in csv format, created by redist_model_2.R)
			NC_leg_dist_2016/2016_Contingent_Congressional_Plan_Corrected.shp (2016 NC Districts Plan)

4. redist_eval_4.R

	- Dependencies: tract_map.csv (tract predictions in csv format, created by redist_model_2.R)
			nc_rand_dist.csv (MCMC random redistricting plans for NC, randomly generated ahead of time)
			NC_tracts.shp (tract shapefile)
