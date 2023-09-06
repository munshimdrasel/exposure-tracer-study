# Script: inverse distance exposure calculation

library( sf)
library( data.table)
library( ggplot2)
library(tidyverse)
library(tigris, quietly = TRUE)
library(viridis, quietly = TRUE)
library(knitr, quietly = TRUE)
library(scales, quietly = TRUE)
library( fasterize)
library( USAboundaries)
library( magrittr)


#working directory on my PC
setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/exposure-tracer-study")

#working directory on GMU Hopper cluster
# setwd ("/projects/HAQ_LAB/mrasel/R/exposure-tracer-study")

# steps:
# 1. create spatial object of sources
# 2. create spatial grid of receptors
#     (instead of a grid, you could use spatial political
#      borders like census tracts)
# 3. apply function to each source that:
#     a) calculates distance between source and each receptor location
#     b) gathers the output, including calculating inverse distance
# 4. if applicable, multiply inverse distance by weighting factor, such as emissions or activity metric



# ======================================================================================================
# PART 01 Quantifying inverse distance exposure for different well types
# ======================================================================================================

#getting well data from Frack Tracker website
# https://app.box.com/s/i7w2tm3tlp4fqmoe3pzelyjx5gbjj2lk/file/904886023237

well.data <- read.csv("./data/FracTrackerNationalWellFile_2021/FracTrackerNationalWells_Part3_TX.csv")

# removing duplicated rows
well.data <- well.data %>% distinct()


oil.gas.well <- well.data %>% filter( Type %in% c("Oil / Gas Well"))
oil.gas.well$ID <- 1:nrow(oil.gas.well) #assigning each grid cell as one unique ID 

link_locations <- oil.gas.well

# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"

link_locations.sf <-
  st_as_sf( link_locations,
            coords = c( 'Long', 'Lat'),
            crs = 'WGS84') %>% st_transform(crs=p4s) 

# https://www.eia.gov/maps/maps.htm
# Eagle ford shale area

efs.shp <- st_read( './data/EagleFord_Play_Boundary_Elevation_Isopach_EIA/ShalePlay_EagleFord_Boundary_EIA_Aug2015_v2.shp')

efs_poly <- efs.shp %>%  st_as_sfc() %>% st_transform(crs=p4s) 


#getting Texas's boundary data
tx.sf <- USAboundaries::us_states( states = 'Texas') %>%st_transform( crs = p4s)

# cropping over Eagle Ford Shale Area in Texas
tx_crop.sf <- st_intersection( tx.sf, efs_poly) 

#cropping wells data over Eagle Ford Shale area, TX
well_locations_crop.sf <- st_intersection(link_locations.sf, efs_poly) # %>% st_transform(crs=p4s)


## =============================================================
# 2. create grid of receptors
#     I'll create this over the state of TX
## =============================================================

# create a grid, use 4km resoulation
fishnet.sf <-
  st_bbox(efs_poly) %>%
  st_as_sfc() %>% st_transform(crs=p4s) %>% 
  st_make_grid( cellsize = 1330) %>% as.data.frame()


# define a receptor ID
fishnet.sf$ID_recept <- 1:nrow( fishnet.sf)

# cropping grids over Eagle Ford Shale Area in Texas
fishnet.sf <- st_intersection( tx_crop.sf, st_sf(fishnet.sf) )

#Running an array batch job on Hopper cluster

#loading netcdf library path on hopper
# dyn.load("/opt/ohpc/pub/mpi/openmpi4-gnu9/4.0.4/lib/libmpi.so.40")

#array job
array_num <- as.numeric( Sys.getenv("SLURM_ARRAY_TASK_ID"))
array_num <- ifelse( array_num == '' | is.na( array_num), 1, array_num)


# split the big dataset into chunks
# https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
N_split <- 50
nrows <- nrow( well_locations_crop.sf )
nrows_split <- split( 1:nrows, rep_len( 1:N_split, nrows))

# select the rows that correspond to this array number
nrows_use <- nrows_split[[array_num]]

link_locations.sf.trans <- well_locations_crop.sf[nrows_use,] 

# link_locations.sf.trans <- st_transform( link_locations.sf.trans , crs = p4s)

# id.vec <- as.vector(unique(link_locations.sf.trans$ID))

# provide an output message
message( paste( 'data successfully read and split into', N_split, 'chunks'))

#inverse distance calculation function
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


#  =============================================================
# 5. sum exposure across sources
# =============================================================
exp_inverse_dist_all <-
  exp_inverse_dist[, .( exposure = sum( inv_dist)),
                   by = c("ID_recept", "Type")]

# link back with receptor spatial database to plot
exp_inverse_dist_all.sf <-
  merge( fishnet.sf,
         exp_inverse_dist_all,
         by = c('ID_recept'))

save(exp_inverse_dist_all.sf , file=paste0('./data/inverse_distance_data/exp_inverse_dist_',array_num,'.RData'))


# =========================================================================================
# PART 02 Plotting exposure for different well types
# =========================================================================================

# setwd("/projects/HAQ_LAB/mrasel/R/exposure-tracer-study")
idwe <- list.files( "./data/inverse_distance_data",  pattern = 'exp_inverse_dist_*',
                    full.names = TRUE)

#reading exposure data from idwe files
all.idwe <- idwe %>%
  map_df(~ get(load(file = .x)))



# # plot the idwe exposure

unique(all.idwe$Type)

all.idwe  %>%  filter (Type=="Oil / Gas Well") %>% ggplot(aes( fill = exposure))  +
  geom_sf( size = 0, alpha = .9) +
  scale_fill_viridis_c( option="plasma") +
  expand_limits( fill = 0) +
  theme_minimal() +
  theme( legend.position = 'bottom',
         legend.title = element_text(size=18),
         legend.text = element_text( angle = 30, size=16))

#saving plot
ggsave(paste0("oil_gas_idwe.png"), path = "./plots/", width=8, height=6, units="in")




