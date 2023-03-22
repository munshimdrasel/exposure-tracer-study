#inverse distance average weighting

library(fst)
library( sf)
library( raster)
library( data.table)
library( ggplot2)
library(tidycensus, quietly = TRUE)
library(tidyverse)
library(tigris, quietly = TRUE)
library(viridis, quietly = TRUE)
library(knitr, quietly = TRUE)
library(scales, quietly = TRUE)
library(kableExtra, quietly = TRUE)
library( raster)
library( fasterize)
library( USAboundaries)
library( magrittr)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# setwd("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/exposure-tracer-study")
setwd("/projects/HAQ_LAB/mrasel/R/exposure-tracer-study")
# steps:
# 1. create spatial object of sources
# 2. create spatial grid of receptors
#     (instead of a grid, you could use spatial political
#      borders like census tracts)
# 3. apply function to each source that:
#     a) calculates distance between source and each receptor location
#     b) gathers the output, including calculating inverse distance
# 4. if applicable, multiply inverse distance by weighting factor, such as emissions or activity metric


## =============================================================
# 1. create example spatial data set of locations
# # use lat/lon with WGS84 coordinade reference system (crs)
#  —-if you find a lat/lon dataset and crs is not provided,
#    it's usually safe to assume WGS84
# see information here: https://docs.qgis.org/2.8/en/docs/gentle_gis_introduction/coordinate_reference_systems.html#:~:text=A%20coordinate%20reference%20system%20(CRS,real%20places%20on%20the%20earth.
## =============================================================
# create a data.table of lat/lons and years
# this would come from cohort participant locations/addresses
# link_locations <-
#   data.table( ID = 1:3,
#               longitude = c( -97, -98, -99),
#               latitude = c( 31, 30, 29))

#getting well data

well.data <- read.csv("./data/FracTrackerNationalWellFile_2021/FracTrackerNationalWells_Part3_TX.csv")

# removing duplicated rows
well.data <- well.data %>% distinct()

unique(well.data$Type)

oil.gas.well <- well.data %>% filter( Type %in% c("Oil / Gas Well"))
oil.gas.well$ID <- 1:nrow(oil.gas.well)
link_locations <- oil.gas.well

link_locations.sf <-
  st_as_sf( link_locations,
            coords = c( 'Long', 'Lat'),
            crs = 'WGS84')


box_use <- c( xmin = -101,
              xmax = -95,
              ymin = 25,
              ymax = 30)

tx.sf <- USAboundaries::us_states( states = 'Texas') #%>%st_transform( crs = p4s)


tx_crop.sf <-
  st_crop( tx.sf, box_use)

# well.date <- as.Date(unique(link_locations.sf$ran.date))

#date.list <- list()

#for (j in 1:length(well.date)) {
#link_locations.sf <- link_locations.sf #%>%  filter (ran.date %in% well.date[j])

well_locations_crop.sf <- st_crop(link_locations.sf, box_use)
# for (j in )
# well_locations_crop.sf <- st_crop( st_make_valid(link_locations.sf),  box_use)

# well_locations_crop.sf <-
#   st_crop((link_locations.sf),
#            box_use)

# link_locations.sf <-
#   st_as_sf( link_locations,
#             coords = c( 'longitude', 'latitude'),
#             crs = 'WGS84')

# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"


link_locations.sf.trans <-
  st_transform( well_locations_crop.sf, crs = p4s)

tx_crop.sf <-tx_crop.sf%>%st_transform( crs = p4s)



## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================
# get TX shapefile object, transform to our crs
# tx.sf <- USAboundaries::us_states( states = 'Texas') %>%
#   st_transform( crs = p4s)

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( tx_crop.sf) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 4000) %>%
  st_sf()

# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

#loading netcdf library path
dyn.load("/opt/ohpc/pub/mpi/openmpi4-gnu9/4.0.4/lib/libmpi.so.40")

array_num <- as.numeric( Sys.getenv("SLURM_ARRAY_TASK_ID"))
array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)


id.vec <- as.vector(unique(link_locations.sf.trans$ID))

df.list <- list()

for (i in 1: length(id.vec)){
  sel.id <- link_locations %>%  filter (ID %in% id.vec[i])
  ran.date <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days")
  
  sel.id.dup <- rbind(sel.id,  sel.id[rep(1, length(ran.date)-1), ])
  
  sel.id.date <- cbind(sel.id.dup,ran.date)
  
  # create a spatial object from the lat/lon
  # use the sf package for spatial processing
  ## NOTE: check the crs of your lat/lon -- i use WGS84 as an example
 
    
    # make a plot with all data so far
    # ggplot( ) +
    #   geom_sf( data = tx_crop.sf, fill = 'white', size = 2) +
    #   geom_sf( data = fishnet.sf,
    #            mapping = aes( fill = ID_recept), show.legend = F,
    #            color = 'grey50', size = .01, alpha = .9) +
    #   geom_sf( data = link_locations.sf.trans, show.legend = T,
    #            mapping = aes( group = as.factor( Type))) +
    #   theme_minimal() +
    #   theme( legend.position = "bottom",
    #          legend.title = element_blank())
    # 
    # ggsave("oil_gas_well_location.png", path="./plots/")
    
    ## =============================================================
    # 3. write/apply a function to calculate inverse distance for each source
    #     define a minimum distance--here, I use 2000 (1/2 the resolution of the grid)
    #     created above. There's no default for this, we should explore the sensitivity of the
    #     results to this value
    ## =============================================================
    inv_distancer <-
      function( n,
                source.sf,
                receptor.sf,
                minimum_dist = 2000){
        
        # calculate distances in m
        site_dist <-
          as.vector(
            st_distance( source.sf[n,],
                         receptor.sf)) #/ 1e3
        
        # minimum distance should be half resolution of grid
        site_dist[site_dist < minimum_dist] <- minimum_dist
        
        # output datset
        out <- data.table( ID = source.sf$ID[n],
                           ID_recept = receptor.sf$ID_recept,
                           inv_dist = 1 / site_dist,
                           Type=source.sf$Type[n],
                           date=source.sf$ran.date[n])
        return( out)
      }
    
    # apply the function to each source
    # exp_inverse_dist <-
    #   lapply(
    #     X = 1:nrow( link_locations.sf.trans),
    #     FUN = inv_distancer,
    #     source.sf = link_locations.sf.trans,
    #     receptor.sf = fishnet.sf) %>%
    #   rbindlist
    
    # if you want to do it fast (in parallel using all cores)
    #   and with a progress bar, use pbmclapply
    exp_inverse_dist <-
      pbmcapply::pbmclapply(
        1:nrow( link_locations.sf.trans),
        inv_distancer,
        source.sf = link_locations.sf.trans,
        receptor.sf = fishnet.sf) %>%
      rbindlist
    
    save(exp_inverse_dist, file=paste0("./data/oil_gas_well_idwe_", id.vec[i],".RData"))
  #df.list[[i]] <- exp_inverse_dist
}


#exposure.data =do.call(rbind, df.list) 


# write.fst(exposure.data, "data/exposure_inv_dist_storage_from_oil.fst")



# load ("./data/storage_from_oil_idwe.RData")
# 
# 
# # exp_inverse_dist_all <- read.fst("data/exposure_inv_dist_storage_from_oil.fst")
# 
# 
# ## =============================================================
# # 4. Now, you can multiply inv. dist by some scaling factor if you have it
# ## =============================================================
# 
# 
# 
# ## =============================================================
# # 5. sum exposure across sources
# ## =============================================================
# exp_inverse_dist_all <-
#   exposure.data[, .( exposure = sum( inv_dist)),
#                    by = ID_recept]
# 
# # link back with receptor spatial database to plot
# exp_inverse_dist_all.sf <-
#   merge( fishnet.sf,
#          exp_inverse_dist_all,
#          by = 'ID_recept')

# plot the exposure
# note that
# ggplot( exp_inverse_dist_all.sf,
#         aes( fill = exposure)) +
#   geom_sf( data = tx_crop.sf, fill = 'white', size = 2) +
#   geom_sf( size = 0, alpha = .9, color = NA) +
#   scale_fill_viridis_c( option="plasma") +
#   expand_limits( fill = 0) +
#   theme_minimal() +
#   theme( legend.position = 'bottom',
#          legend.text = element_text( angle = 30))
# 
# ggsave("inverse_distance_exposure.png", path="./plots/")
# 
# #population data from lonterm-coal exposure study by Henneman et al. longterm coal study
# 
# 
# ## =========================================================== ##
# # link spatial and population data
# ## =========================================================== ##
# # read in population data (from code/census_data_wrangle.R)
# pop_all_yr <- read.fst( './data/population_county.fst', as.data.table = TRUE)
# 
# # taking texas 2020 data
# tx_pop_2020 <- pop_all_yr %>%  filter( statefp==48)
# 
# # read in counties data
# us_counties.in <- us_counties()
# tx_counties.pop <- data.table( merge( us_counties.in, tx_pop_2020,
#                                       by = c( 'countyfp', 'statefp')))
# 
# # which counties aren't in the spatial data
# # county_state_match <- unique( tx_counties.pop[, .( countyfp, statefp)])[, ID := 1]
# # pop_all_yr.miss <- merge( county_state_match, pop_all_yr, all = TRUE,
# #                           by = c( 'countyfp', 'statefp'))
# 
# #just lower 48 states
# # states.lower <- state.name[ !( state.name %in% c( 'Hawaii', 'Alaska'))]
# # us_counties.pop <- us_counties.pop[ state_name %in% states.lower]
# 
# 
# # transform to consistent crs
# tx_counties.sf <- st_transform( st_as_sf( tx_counties.pop, 
#                                           sf_column_name = 'geometry',
#                                           crs = crs( us_counties.in)), crs = p4s)
# 
# 
# 
# ## =========================================================== ##
# ## # area weight unit population to hyads grid
# ## creates data table of gridded population data from counties
# ## =========================================================== ##
# grid_popwgt <- 
#   lapply( 2010:2020,
#           function( yr){
#             print( yr)
#             interp_col <- c( 'TOT_POP', 'White', 'Black', 'Native',
#                              'Asian', 'Pacific', 'Hispanic')
#             # interpolate county data to grid
#             interp <- st_interpolate_aw( tx_counties.sf[tx_counties.sf$year == yr,
#                                                         x], 
#                                          exp_inverse_dist_all.sf, extensive = T)
#             interp$year <- yr
#             return( interp)
#           }) 
# 
# grid_popwgt <- do.call(rbind, grid_popwgt)
# unique.geometry <- as.vector(grid_popwgt$geometry)
# 
# exp_inverse_dist_all.sf <- exp_inverse_dist_all.sf %>%  filter(geometry %in% unique.geometry )
# 
# #merging population data and inverse distance data
# combined <- inner_join(exp_inverse_dist_all.sf %>% as.data.frame(), grid_popwgt %>% as.data.frame(), by = "geometry")
# combined_2020 <- combined %>% filter(year==2020)
# 
# combined <- combined_2020
# combined$TOT_POP_idwe <- combined$exposure * combined$TOT_POP
# combined$Black_idwe <- (combined$exposure * combined$Black)/combined$TOT_POP
# combined$Asian_idwe <- (combined$exposure * combined$Asian)/combined$TOT_POP
# combined$Native_idwe <- (combined$exposure * combined$Native)/combined$TOT_POP
# combined$Pacific_idwe <- (combined$exposure * combined$Pacific)/combined$TOT_POP
# combined$White_idwe <- (combined$exposure * combined$White)/combined$TOT_POP
# combined$Hispanic_idwe <- (combined$exposure * combined$Hispanic)/combined$TOT_POP
# 
# combined <- combined %>% dplyr::select(-ID_recept)
# combined <- st_as_sf(combined)
# ggplot( combined,
#         aes( fill = Hispanic_idwe)) +
#   geom_sf( data = tx_crop.sf, fill = 'white', size = 2) +
#   geom_sf( size = 0, alpha = .9, color = NA) +
#   scale_fill_viridis_c( option="plasma") +
#   expand_limits( fill = 0) +
#   theme_minimal() +
#   theme( legend.position = 'bottom',
#          legend.text = element_text( angle = 30))
# 
# 
# #getting PM data from Air pollution exposure disparities across US population and income groups study
# 
# # SECTION 1 - READ AND CLEAN DATA---------------------------------------------------------
# part1 = read.csv("/Volumes/GoogleDrive/My Drive/R/pm25_and_disparity/data/data_part1.csv", header=TRUE)
# part2 = read.csv("/Volumes/GoogleDrive/My Drive/R/pm25_and_disparity/data/data_part2.csv", header=TRUE)
# part3 = read.csv("/Volumes/GoogleDrive/My Drive/R/pm25_and_disparity/data/data_part3.csv", header=TRUE)
# part4 = read.csv("/Volumes/GoogleDrive/My Drive/R/pm25_and_disparity/data/data_part4.csv", header=TRUE)
# part5 = read.csv("/Volumes/GoogleDrive/My Drive/R/pm25_and_disparity/data/data_part5.csv", header=TRUE)
# 
# final_pm_data = do.call("rbind", list(part1, part2, part3, part4, part5))
# rm(part1, part2, part3, part4, part5)
# # write.csv(final_pm_data, "./data/pm25_racial_income_disparities.csv")
# # final_pm_data = read.csv("./data/pm25_racial_income_disparities.csv", header=TRUE)
# no_na_all_years_pm_data = na.omit(final_pm_data)
# 
# ## Download source file, unzip and extract into table
# ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
# temp <- tempfile()
# download.file(ZipCodeSourceFile , temp)
# ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
# unlink(temp)
# names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
#                     "state.full", "state", "AdminName2", "FIPS", 
#                     "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
# 
# tx_zipcodes <- ZipCodes %>%  filter (state=="TX") %>% dplyr::select(zip,state,latitude,longitude)
# 
# tx_zip_geometry <- zctas(cb = T, class = "sf") %>% filter (ZCTA5CE20 %in% tx_zipcodes$zip) %>%
#   dplyr::select(ZCTA5CE20,geometry)
# tx_zip_geometry   <-
#   st_crop(tx_zip_geometry ,
#            box_use)
# 
# tx_pm <- no_na_all_years_pm_data %>%  filter (zcta %in% tx_zip_geometry$ZCTA5CE20)
# 
# names(tx_zip_geometry)[names(tx_zip_geometry) == 'ZCTA5CE20'] <- 'zcta'
# 
# tx_pm_geometry <- merge(tx_pm, tx_zip_geometry, by= "zcta")
# 
# 
# # transform to consistent crs
# tx_zip.sf <- st_transform( st_as_sf( tx_pm_geometry,  sf_column_name = 'geometry'), crs = p4s)
# 
# 
# 
# tx_zip.sf_2012 <- tx_zip.sf %>% filter(year==2012)
# tx_zip.sf_2012 %>% ggplot(   aes( fill = pm25)) +
#   geom_sf( data = tx_zip.sf_2012, fill = 'white', size = 2) +
#   geom_sf( size = 0, alpha = .9, color = NA) +
#   scale_fill_viridis_c( option="plasma") +
#   expand_limits( fill = 0) +
#   theme_minimal() +
#   theme( legend.position = 'bottom',
#          legend.text = element_text( angle = 30))
# 
# # ggsave("inverse_distance_exposure.png", path="./plots/")
# 
# 
# grid_popwgt <- 
#   lapply( 2015:2016,
#           function( yr){
#             print( yr)
#             interp_col <- c( "popdensity", 'population', 'poverty', 'education', "pct_blk", "pct_hisp",
#                              "pct_native", "pct_asian", "pct_white", "black_pop", "hisp_pop", "native_pop",
#                              "asian_pop", "white_pop", "medhouseholdincome", "pm25")
#             # interpolate county data to grid
#             interp <- st_interpolate_aw( tx_zip.sf[tx_zip.sf$year == yr,
#                                                         interp_col], 
#                                          exp_inverse_dist_all.sf, extensive = F)
#             interp$year <- yr
#             
#             return( interp)
#           }) 
# 
# grid_popwgt <- do.call(rbind, grid_popwgt)
# unique.geometry <- as.vector(grid_popwgt$geometry)
# 
# exp_inverse_dist_all.sf <- exp_inverse_dist_all.sf %>%  filter(geometry %in% unique.geometry )
# 
# 
# 
# #merging population data and inverse distance data
# combined <- inner_join(exp_inverse_dist_all.sf %>% as.data.frame(), grid_popwgt %>% as.data.frame(), by = "geometry")
# 
# combined$TOT_POP <- combined$black_pop + combined$black_pop +combined$hisp_pop +combined$native_pop +combined$asian_pop +
#   combined$white_pop 
# # combined$TOT_POP_idwe <- combined$exposure * combined$TOT_POP
# combined$Black_idwe <- (combined$exposure * combined$black_pop)*combined$pm25/combined$TOT_POP
# combined$Asian_idwe <- (combined$exposure * combined$asian_pop)/combined$TOT_POP
# combined$Native_idwe <- (combined$exposure * combined$native_pop)/combined$TOT_POP
# # combined$Pacific_idwe <- (combined$exposure * combined$native_pop)/combined$TOT_POP
# combined$White_idwe <- (combined$exposure * combined$white_pop)/combined$TOT_POP
# combined$Hispanic_idwe <- (combined$exposure * combined$hisp_pop)/combined$TOT_POP
# 
# combined <- combined %>% dplyr::select(-ID_recept)
# combined <- st_as_sf(combined)
# ggplot( combined,
#         aes( fill = Hispanic_idwe)) +
#   geom_sf( data = tx_crop.sf, fill = 'white', size = 2) +
#   geom_sf( size = 0, alpha = .9, color = NA) +
#   scale_fill_viridis_c( option="plasma") +
#   expand_limits( fill = 0) +
#   theme_minimal() +
#   theme( legend.position = 'bottom',
#          legend.text = element_text( angle = 30))

