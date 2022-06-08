library( sf)
library( raster)
library( data.table)
library( ggplot2)

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
link_locations <-
  data.table( ID = 1:3,
              longitude = c( -97, -98, -99),
              latitude = c( 31, 30, 29))

#getting well data

well.data <- read.csv("./data/FracTrackerNationalWellFile_2021/FracTrackerNationalWells_Part3_TX.csv")

# removing duplicated rows
well.data <- well.data %>% distinct()

oil.gas.well <- well.data %>% filter( Type=="Oil / Gas Well")
oil.gas.well$ID <- 1:nrow(oil.gas.well)
link_locations <- oil.gas.well

# create a spatial object from the lat/lon
# use the sf package for spatial processing
## NOTE: check the crs of your lat/lon -- i use WGS84 as an example
link_locations.sf <-
  st_as_sf( link_locations,
            coords = c( 'Long', 'Lat'),
            crs = 'WGS84')

# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

link_locations.sf.trans <-
  st_transform( link_locations.sf, crs = p4s)

## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================
# get TX shapefile object, transform to our crs
tx.sf <- USAboundaries::us_states( states = 'Texas') %>%
  st_transform( crs = p4s)

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( tx.sf) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 4000) %>%
  st_sf()

# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

# make a plot with all data so far
ggplot( ) +
  geom_sf( data = tx.sf, fill = 'white', size = 2) +
  geom_sf( data = fishnet.sf,
           mapping = aes( fill = ID_recept),
           color = 'grey50', size = .01, alpha = .9) +
  geom_sf( data = link_locations.sf.trans,
           mapping = aes( color = as.factor( ID))) +
  theme_minimal() +
  theme( legend.position = 'bottom')

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
                       inv_dist = 1 / site_dist)
    return( out)
  }

# apply the function to each source
exp_inverse_dist <-
  lapply(
    X = 1:nrow( link_locations.sf.trans),
    FUN = inv_distancer,
    source.sf = link_locations.sf.trans,
    receptor.sf = fishnet.sf) %>%
  rbindlist

# if you want to do it fast (in parallel using all cores)
#   and with a progress bar, use pbmclapply
exp_inverse_dist <-
  pbmcapply::pbmclapply(
    1:nrow( link_locations.sf.trans),
    inv_distancer,
    source.sf = link_locations.sf.trans,
    receptor.sf = fishnet.sf) %>%
  rbindlist

## =============================================================
# 4. Now, you can multiply inv. dist by some scaling factor if you have it
## =============================================================



## =============================================================
# 5. sum exposure across sources
## =============================================================
exp_inverse_dist_all <-
  exp_inverse_dist[, .( exposure = sum( inv_dist)),
                   by = ID_recept]

# link back with receptor spatial database to plot
exp_inverse_dist_all.sf <-
  merge( fishnet.sf,
         exp_inverse_dist_all,
         by = 'ID_recept')

# plot the exposure
# note that
ggplot( exp_inverse_dist_all.sf,
        aes( fill = exposure)) +
  geom_sf( data = tx.sf, fill = 'white', size = 2) +
  geom_sf( size = 0, alpha = .9, color = NA) +
  scale_fill_gradient( high = 'red', low = 'white') +
  expand_limits( fill = 0) +
  theme_minimal() +
  theme( legend.position = 'bottom',
         legend.text = element_text( angle = 30))
