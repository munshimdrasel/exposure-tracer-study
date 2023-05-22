# Script 02: inverse distance exposure calculation


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
# library(kableExtra, quietly = TRUE)
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

# =========================================================================================

# PART 01 Quantifying inverse distance exposure for different well types

# =========================================================================================




#getting well data from Frack Tracker website
# https://app.box.com/s/i7w2tm3tlp4fqmoe3pzelyjx5gbjj2lk/file/904886023237

well.data <- read.csv("./data/FracTrackerNationalWellFile_2021/FracTrackerNationalWells_Part3_TX.csv")

# removing duplicated rows
well.data <- well.data %>% distinct()

unique(well.data$Type)

oil.gas.well <- well.data #%>% filter( Type %in% c("Storage from Oil / Gas"))
oil.gas.well$ID <- 1:nrow(oil.gas.well)

link_locations <- oil.gas.well

link_locations.sf <-
  st_as_sf( link_locations,
            coords = c( 'Long', 'Lat'),
            crs = 'WGS84')


#cropping over Eagle Ford Shale area, Texas
box_use <- c( xmin = -101,
              xmax = -95,
              ymin = 25,
              ymax = 30)

tx.sf <- USAboundaries::us_states( states = 'Texas') #%>%st_transform( crs = p4s)

# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

tx_crop.sf <- st_crop( tx.sf, box_use) %>%st_transform( crs = p4s)

well_locations_crop.sf <- st_crop(link_locations.sf, box_use) # %>% st_transform(crs=p4s)


## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox( tx_crop.sf) %>%
  st_as_sfc() %>%
  st_make_grid( cellsize = 4000) %>%
  st_sf()

# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

#Running a batch job on Hopper cluster

#loading netcdf library path on hopper
dyn.load("/opt/ohpc/pub/mpi/openmpi4-gnu9/4.0.4/lib/libmpi.so.40")

#array job
array_num <- as.numeric( Sys.getenv("SLURM_ARRAY_TASK_ID"))
array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)


# split the big dataset into chunks
# https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
N_split <- 50
nrows <- nrow( well_locations_crop.sf )
nrows_split <- split( 1:nrows, rep_len( 1:N_split, nrows))

# select the rows that correspont to this array number
nrows_use <- nrows_split[[array_num]]

link_locations.sf.trans <- well_locations_crop.sf[nrows_use,] 

link_locations.sf.trans <- st_transform( link_locations.sf.trans , crs = p4s)

# id.vec <- as.vector(unique(link_locations.sf.trans$ID))

# provide an output message
message( paste( 'data successfully read and split into', N_split, 'chunks'))


inv_distancer <-
  function( n,
            source.sf,
            receptor.sf,
            minimum_dist = 2000){
    
    # calculate distances in m
    site_dist <-
      as.vector( st_distance( source.sf[n,],
                     receptor.sf)) #/ 1e3
    
    # minimum distance should be half resolution of grid
    site_dist[site_dist < minimum_dist] <- minimum_dist

    # output datset
    out <- data.table( ID = source.sf$ID[n],
                       ID_recept = receptor.sf$ID_recept,
                       inv_dist = 1 / site_dist,
                       Type=source.sf$Type[n],
                       date=source.sf$ran.date[n],
                       geometry=source.sf$geometry[n],
                       API= source.sf$API[n])
    
    #out <- out %>%  filter (inv_dist>10e-5) #taking exposure within 100km
    return( out)
  }

# apply the function to each source

# if you want to do it fast (in parallel using all cores and with a progress bar, use pbmclapply

exp_inverse_dist <-
    lapply(
      X= 1:nrow( link_locations.sf.trans ),
      FUN = inv_distancer,
      source.sf = link_locations.sf.trans,
      receptor.sf = fishnet.sf) %>%
    rbindlist

# define a file name, save the output
# filename.out <- paste0( './data/exp_inverse_dist_', array_num, '.csv')
# fwrite( exp_inverse_dist ,
#         filename.out)


# save(exp_inverse_dist , file=paste0("./data/exp_inverse_dist_",array_num,".RData"))

# # ## =============================================================
# # # 5. sum exposure across sources
# # ## =============================================================
exp_inverse_dist_all <-
  exp_inverse_dist[, .( exposure = sum( inv_dist)),
           by = c("ID_recept", "Type")]

# link back with receptor spatial database to plot
exp_inverse_dist_all.sf <-
  merge( fishnet.sf,
         exp_inverse_dist_all,
         by = 'ID_recept')

save(exp_inverse_dist_all.sf , file=paste0('./data/inverse_distance_data/exp_inverse_dist_',array_num,'.RData'))



# =========================================================================================
# PART 02 Plotting exposure for different well types
# =========================================================================================

# setwd("/projects/HAQ_LAB/mrasel/R/exposure-tracer-study")
idwe <- list.files( "./data/inverse_distance_data",  pattern = 'exp_inverse_dist_*',
                      full.names = TRUE)

all.idwe <- idwe %>%
  map_df(~ get(load(file = .x)))



# # plot the exposure

unique(all.idwe$Type)

all.idwe  %>%  filter (Type=="Oil / Gas Well") %>% ggplot(aes( fill = exposure))  +
  geom_sf( size = 0, alpha = .9, color = NA) +
  scale_fill_viridis_c( option="plasma") +
   geom_sf( data = tx_crop.sf, fill = 'white', alpha = 0.01, size = 2) +
  expand_limits( fill = 0) +
  theme_minimal() +
  theme( legend.position = 'bottom',
         legend.title = element_text(size=18),
         legend.text = element_text( angle = 30, size=16))

#saving plot
ggsave(paste0("oil_gas_idwe.png"), path = "./plots/", width=8, height=6, units="in")




