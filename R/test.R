# setup R libraries used in this post
library(tidycensus, quietly = TRUE)
library(tidyverse)
library(sf, quietly = TRUE)
library(tigris, quietly = TRUE)
library(viridis, quietly = TRUE)
library(knitr, quietly = TRUE)
library(scales, quietly = TRUE)
library(kableExtra, quietly = TRUE)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

setwd("/Volumes/GoogleDrive/My Drive/R/exposure-tracer-study")

pick_state="TX"

#counties under eagle ford shale area
# https://eaglefordshale.com/counties#:~:text=La%20Salle%20County%2C%20TX

pick_county = c ("Atascosa", "Austin", "Bastrop", "Bee", "Brazos", "Burleson", "Colorado",
                 "DeWitt", "Dimmit", "Duval", "Fayette", "Frio", "Goliad", "Gonzales", "Grimes",
                 "Karnes", "La Salle", "Lavaca", "Lee", "Leon", "Live Oak", "Madison", "Maverick",
                 "McMullen", "Milam", "Robertson", "Washington", "Webb", "Wilson", "Zavala")


#getting Census population data (RACE)
# variable list here : https://api.census.gov/data/2020/acs/acs5/variables.html

total <- "B02001_001"
white <- "B02001_002"
black <- "B02001_003"
native <- "B02001_004"
asian <- "B02001_005"
pacific_islander <- "B02001_006"
some_other_race <- "B02001_007"
two_or_more_race <- "B02001_008"
two_race_with_some_other <- "B02001_009"
two_race_exclude_some_other <- "B02001_010"


# let's get the variables into a data frame where they will be easier to work with
df <- tribble(
  ~pop, ~variable,
  "total", "B02001_001",
  "white" , "B02001_002",
  "black" , "B02001_003",
  "native" , "B02001_004",
  "asian" , "B02001_005",
  "pacific_islander", "B02001_006",
  "some_other_race" , "B02001_007",
  "two_or_more_race" ,"B02001_008",
  "two_race_with_some_other" ,"B02001_009",
  "two_race_exclude_some_other" , "B02001_010")

#getting block group population data
race.list=list()

for (i in 1:length(pick_county)) {
  race_town2 <- get_acs(geography = "block group", # for CT, that means towns
                        state = pick_state,
                        county = pick_county[i],
                        geometry = "TRUE",
                        year = 2020,
                        survey = "acs5",
                        variables = c(total, white, black, native, asian,pacific_islander,
                                      some_other_race,two_or_more_race, two_race_with_some_other, 
                                      two_race_exclude_some_other),
                        summary_var = "B02001_001") %>%
    left_join(df, by = "variable") %>%
    mutate(race = factor(pop,
                         levels = c("total","white", "black", "native", "asian", "pacific_islander",
                                    "some_other_race","two_or_more_race", "two_race_with_some_other", 
                                    "two_race_exclude_some_other"),
                         labels = c("Total","White", "Black", "Native", "Asian", "Pacific Islander",
                                    "Some other race", "Two or more race", "Two race with some other",
                                    "Two race exclude some other")),
           pct = estimate / summary_est,
           pct_moe = moe_prop(estimate, summary_est, moe, summary_moe))
  
  # block_group_geometry <- block_groups(state = pick_state, county = pick_county[i], cb = TRUE)
  
  race_table <- as_tibble(race_town2 %>%
                            dplyr::select(NAME, estimate,moe, pct, pct_moe, race))
  
  race_table2 <- race_table %>%
    mutate(pct = percent(pct, accuracy = 1.0)) %>%
    select(NAME, race, Percent = pct) %>%
    spread(key = race, value = Percent) %>%
    arrange(`White`) 
  
  race.list[[i]] <- race_table2
}

race.data <- do.call(rbind,race.list)

#creting subgroups (block group, tract, county, state) of Name variable
race.data$block.group <- word(race.data$NAME,3)  #this code take only certain words based on sequence (3rd word here)
race.data$tract <- word(race.data$NAME,6)
race.data$county <- word(race.data$NAME,7)
race.data$state <- word(race.data$NAME,9)

#removing comma from tract and blockgroup
race.data$block.group <-gsub('.{1}$', '', race.data$block.group)
race.data$tract <-gsub('.{1}$', '', race.data$tract)

#adding 2nd part of county name (La Salle for example)

race.data$county <- ifelse(race.data$county=="La", "La Salle", race.data$county )
race.data$county <- ifelse(race.data$county=="Live", "Live Oak", race.data$county )


#table
race.data %>% dplyr::select(-NAME) %>% kable(caption = "Percent Race by Block group") %>%
  kableExtra::kable_styling() %>%
  kableExtra::footnote(general = "")


#median income data

datalist=list()
for (i in 1:length(pick_county)) {
  pick_state = "TX"
  # set cb = TRUE to keep boundaries tied to the coastline
  block_group_geometry <- block_groups(state = pick_state, county = pick_county[i], cb = TRUE)

  block_group_geometry_df <- as.data.frame(block_group_geometry %>%  dplyr::select(GEOID, geometry))
  
  income_block_groups <- get_acs(geography = "block group",
                                 state = "TX",
                                 county = pick_county[i],
                                 geometry = "TRUE",
                                 year = 2020,
                                 survey = "acs5",
                                 variables = "B19013_001") %>%
    filter(estimate > 0) %>%
    left_join(block_group_geometry_df, by = c("GEOID", "geometry")) #%>%
  # filter(TOWN %in% pick_towns)
  datalist[[i]] <- income_block_groups
} 

income.block.groups <- do.call(rbind, datalist)

# yy <- xx %>% filter (NAME=="Block Group 2, Census Tract 1.03, Wilson County, Texas")
ggplot() +
  geom_sf(data = income.block.groups,
          aes(fill = estimate), colour = "gray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Median Household \nIncome", begin = 0.1 ) +
  labs(title = "") +
  coord_sf(datum = NA, label_axes = "----") + theme_minimal() +
  ggtitle("Median Household Income at Eagle Ford Shale Area, TX")

#creting subgroups (block group, tract, county, state) of Name variable
income.block.groups$block.group <- word(income.block.groups$NAME,3) 
income.block.groups$tract <- word(income.block.groups$NAME,6)
income.block.groups$county <- word(income.block.groups$NAME,7)
income.block.groups$state <- word(income.block.groups$NAME,9)

#removing comma
income.block.groups$block.group <-gsub('.{1}$', '', income.block.groups$block.group)
income.block.groups$tract <-gsub('.{1}$', '', income.block.groups$tract)

#adding 2nd part of county name (La Salle for example)

income.block.groups$county <- ifelse(income.block.groups$county=="La", "La Salle", income.block.groups$county )
income.block.groups$county <- ifelse(income.block.groups$county=="Live", "Live Oak", income.block.groups$county )

#zooming to a county
county.webb <- income.block.groups %>% filter (county %in% c("Webb"))

# yy <- xx %>% filter (NAME=="Block Group 2, Census Tract 1.03, Wilson County, Texas")
ggplot() +
  geom_sf(data = county.webb,
          aes(fill = estimate), colour = "gray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Median Household \nIncome", begin = 0.1 ) +
  labs(title = "") +
  coord_sf(datum = NA, label_axes = "----") + theme_minimal() 

#zooming to tract
tract.webb <- county.webb %>%  filter (tract=="18.19")

# tract leveplot

ggplot() +
  geom_sf(data = tract.webb,
          aes(fill = estimate), colour = "gray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Median Household \nIncome", begin = 0.1 ) +
  labs(title = "") +
  coord_sf(datum = NA, label_axes = "----") + theme_minimal() 



#well locations
# https://www.fractracker.org/data/
# https://app.box.com/s/h2dnw9flxrx1by9fie03uk5s87nd28nw

# other data from fractracker
# https://app.box.com/s/i7w2tm3tlp4fqmoe3pzelyjx5gbjj2lk/folder/99383493462

#well location source of fractracker (Texas Rail Road comission) https://mft.rrc.texas.gov/link/4e9023eb-e4ee-45b8-81b7-aec1494c1e8e

well.data <- read.csv("./data/FracTrackerNationalWellFile_2021/FracTrackerNationalWells_Part3_TX.csv")


names(well.data)[names(well.data) == 'Long'] <- 'lon'
names(well.data)[names(well.data) == 'Lat'] <- 'lat'

# 
# map.plot <- usmap_transform(well.data)
# 
# map.oil.gas.well <- map.plot %>% filter( Type=="Oil / Gas Well")
# 
# unique(map.plot$Type)
# plot_usmap("counties", labels = F,  include = c("TX")) +
#   geom_point(data = map.oil.gas.well,
#              aes(x = x, y = y, color=Type, shape=Type), 
#              size = 1)  + theme(legend.text = element_text(size=15), legend.title= element_blank(),
#                                 legend.position = c(0.1,0.1)) 


#well data to sf
# 
# names(well.data)
# 
# # Convert lat/long to a sf
# 
# well_sf <- well.data %>% st_as_sf(coords = c("lon", "lat"), crs=4326)
# 
# well_sf_t <- st_transform(well_sf, crs=2163)
# 
# 
# # obtain state geometries
# states_sf <- get_urbn_map(map = "states", sf = TRUE)

# #blocks data
# https://data.capitol.texas.gov/dataset/2020-census-geography

blockgroups.sf <- st_read( './data/blockgroups/BlockGroups.shp')


# combine into single geom for easy plotting
tx_allblockgroups <- st_union( blockgroups.sf)

# limit over Eagle Ford region
extent_limit <-
  st_bbox(tx_allblockgroups)
box_use <- c( xmin = 800000,
              xmax = 1500000,
              ymin = 400000,
              ymax = 900000)


tx_crop_blockgroups.sf <-
  st_crop( tx_allblockgroups,
           box_use)
blockgroups_crop.sf <-
  st_crop( st_make_valid( blockgroups.sf),
           box_use)


# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
# (sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
#                                                                       26.83)))
# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = map.oil.gas.well, aes(x = lon, y = lat), size = 1, 
#              shape = 23, fill = "darkred") +
#   coord_sf(xlim = c(-101, -95), ylim = c(25, 35), expand =F)


well_sites <- well.data %>% filter(Type=="Oil / Gas Well") %>%
  st_as_sf (coords = c("lon", "lat"),
            crs = 4326, agr = "constant")

# well_sites <- well.data  %>% 
#   st_as_sf (coords = c("lon", "lat"), 
#             crs = 4326, agr = "constant")
# combine into single geom for easy plotting
well_allblocks <- st_union( well_sites)

well_sites <- well.data %>% filter(Type=="Oil / Gas Well") %>%
  st_as_sf (coords = c("lon", "lat"),
            crs = 4326, agr = "constant")

# limit over Eagle Ford region
extent_limit_well_blocks <-
  st_bbox(well_allblocks)

box_use <- c( xmin = -101,
              xmax = -95,
              ymin = 25,
              ymax = 30)


well_blocks_crop.sf <-
  st_crop( well_allblocks,
           box_use)
well_sites_crop.sf <-
  st_crop( st_make_valid(well_sites),
           box_use)

# check the boundary box in a plot
# ggplot( well_sites) +
#   geom_sf( fill = NA) +
#   geom_sf( data = st_as_sfc( st_bbox( well_blocks_crop.sf)), fill = NA) +
#   theme_minimal() +
#   geom_sf(data = well_sites,  size = 0.5, 
#           shape = 23, fill = "darkred")

# make a plot
ggplot( ) + 
  geom_sf( data = tx_crop_blockgroups.sf, fill = 'white') + 
  geom_sf( data = well_sites_crop.sf, size = 0.5, 
           shape = 23, fill = "darkred")   + 
  theme_minimal() + 
  theme( legend.position = c( .2, .3)) +
  ggtitle("Oil and Gas Well Location at Eagle Ford Shale area")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = well_sites,  size = 0.5,
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-101, -95), ylim = c(25.7, 31), expand =F)


