# remotes::install_github("walkerke/tidycensus")
library( data.table)
library( sf)
library( ggplot2)
library( viridis)
library(tidycensus)
library(tidyverse)
library(tigris)


#download pop data
# https://data.capitol.texas.gov/dataset/2020-census-geography
# Blocks_Pop.zip

#readme file to get to know with variables
# https://data.capitol.texas.gov/dataset/2b59f5ce-5fa4-4040-a550-caffbe8986c4/resource/cc4b411d-10a4-4fdd-accf-24a68397b19b/download/readme_2020censusdata.txt


#census key
# cc94cc692f0a6978d6d7c58adc152fe466e2c231

# census_api_key('cc94cc692f0a6978d6d7c58adc152fe466e2c231', install = TRUE, overwrite = TRUE)
#2020 

# ACS5 variables
# https://api.census.gov/data/2018/acs/acs5/variables.html
# https://www.r-bloggers.com/2020/07/automatically-build-data-tables-from-us-census-survey/
# https://www.r-bloggers.com/2019/04/working-with-new-haven-area-census-data-using-r/

# variable B19013_001 from the ACS5 survey block level household income data 

# sex by age B01001_001

#population B02001

#total population B02001

#Eagle Ford 
#tract level household income for Texas
tx.tract <- get_acs(state = 'TX', county= 'Atascosa', year = 2020, geography = 'block group', variables = 'B19013_001',
                    geometry = FALSE, survey = 'acs5', show_call = TRUE)

# tx.county <- get_acs(state = 'TX', year = 2020, geography = 'county', variables = c(medincome = "B19013_001"),
#                      geometry = FALSE, survey = 'acs5', show_call = TRUE)

# get_decennial(state="TX", year = 2020, geography = "block", variables = 'B19013_001E',
#         geometry = FALSE,  show_call = TRUE)

# get_decennial(state="TX", county= "Keynes", year = 2020, geography = "block", variables = 'B02001_001E',
#               geometry = FALSE,  show_call = TRUE)



# tx.county %>%
#   mutate(NAME = gsub(" County, Texas", "", NAME)) %>%
#   ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
#   geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
#   geom_point(color = "red", size = 3) +
#   labs(title = "Household income by county in Texas",
#        subtitle = "2020 American Community Survey",
#        y = "",
#        x = "ACS estimate (bars represent margin of error)")




pop_data_2020 <- read.table("./data/Blocks_Pop.txt",header = TRUE, sep = ",")

names(pop_data_2020)

pop_data_2020 %>%  filter (blkkey==5)




##LF
# 
# library( censusapi)
# library( magrittr)
# library( data.table)
# 
# # tutorials
# # https://jennhuck.github.io/workshops/tidycensus.html
# 
# Sys.setenv( CENSUS_KEY= 'cc94cc692f0a6978d6d7c58adc152fe466e2c231')
# 
# readRenviron("~/.Renviron")
# Sys.getenv( "CENSUS_KEY")
# 
# 
# apis <- listCensusApis() %>% as.data.table()
# 
# View(apis[ vintage == 2020])
# 
# # https://www.census.gov/data/developers/data-sets/acs-5year.html
# # https://www.census.gov/programs-surveys/acs/guidance.html
# variables_list <- 
#   listCensusMetadata(
#     name = "acs/acs5/",
#     vintage = '2020',
#     group = 'B19101',
#     type = "variables"
#   ) %>% as.data.table()
# geographies_list <- 
#   listCensusMetadata(
#     name = "acs/acs5/",
#     vintage = '2020',
#     group = 'B19101',
#     type = "geography"
#   ) %>% as.data.table()
# 
# variables_list[grep( 'Estimate!!Total:!!$15,000 to $19,999', label)]
# variables_list[grep( 'B19101', group)]
# 
# geographies_list[name == 'B19101']
# 
# as.data.table( variables_list)[!grep( 'B19101', name)]
# unique( as.data.table( geographies_list)[!grep( '^B', name)]$description)
# 
# geography = 150
# 
# # to get block groups, must specify county!
# sahie_counties <- getCensus(
#   name = "acs/acs5/",
#   vars = c("NAME", "group(B19101)"),
#   region = "block group:*", #"block group:*",
#   # state 48 is TX: https://www.census.gov/geographies/reference-files/time-series/geo/tallies.html
#   regionin = "state:48+county:029",
#   vintage = 2020,
#   show_call = TRUE
# ) %>% as.data.table()



vars <- load_variables(2020, "acs5", cache = TRUE) %>%
  mutate(table_id = str_sub(name, 1, 6),
         # Race generally is in parentheses after the concept name.
         # But for a few cases, something else is in parentheses first. So I
         # am going to blank out that stuff and then assume whatever I find inside
         # of parentheses is race.
         concept2 = str_replace_all(concept,
                                   c("\\(IN 2017 INFLATION-ADJUSTED DOLLARS\\)" = "",
                                     "\\(EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS\\)" = "",
                                     "\\(SSI\\)" = "",
                                     "\\(INCLUDING LIVING ALONE\\)" = "",
                                     "\\(IN MINUTES\\)" = "",
                                     "\\(DOLLARS\\)" = "",
                                     "\\(CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI\\)" = "--CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI--",
                                     "\\(CAR, TRUCK, OR VAN\\)" = "--CAR, TRUCK, OR VAN--",
                                     "\\(\\)" = ""
                                   )),
         race = str_extract(concept, "\\(.+\\)"),
         race = str_replace(race, "\\(", ""),
         race = str_replace(race, "\\)", ""))
# I should have been able to do this in one line, but it doesn't seem to work:
# race = str_extract(concept, "\\((.*?)\\)"))
B17010_variables <- vars %>%
  filter(table_id == "B17010", is.na(race)) %>%
  pluck("name")
poverty_acs <- get_acs(geography = "block group", # for CT, that means towns
                       state = "TX",
                       county = "Atascosa",
                       geometry = "FALSE", # no map at this time
                       year = 2020,
                       survey = "acs5",
                       variables = B17010_variables[2],
                       summary_var = B17010_variables[1]) %>%
  filter(estimate > 0) %>%
  mutate(TOWN = str_replace(NAME, " town, New Haven County, Connecticut", ""),
         pct_poverty = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 



pick_towns <- c("Woodbridge", "West Haven", "New Haven", "East Haven",
                "Bethany", "Orange", "Milford", "Branford", "Guilford",
                "North Haven", "Madison", "Hamden", "North Branford",
                "Wallingford")
# I'm saving these to make it easier to re-use same code for a different area.
pick_county = "New Haven"
pick_state = "CT"
# set cb = TRUE to keep boundaries tied to the coastline
town_geometry <- county_subdivisions(state = pick_state, county = pick_county, cb = TRUE)
tract_geometry <- tracts(state = pick_state, county = pick_county, cb = TRUE)
# let's find which tract is in which town
tract_centroid = st_centroid(tract_geometry)
# tract town has the geometry of town, not tract
tract_town <-st_join(town_geometry, tract_centroid,
                     join = st_intersects,
                     suffix = c(".TOWN", ".TRACT"))
tract_town_df <- tract_town %>%
  as_tibble() %>%
  select(TOWN = NAME.TOWN, GEOID = GEOID.TRACT) %>%
  mutate(near_nh = (TOWN %in% pick_towns))
area_tracts <- tract_geometry %>%
  left_join(tract_town_df, by = "GEOID") %>%
  filter(TOWN %in% pick_towns)
area_towns <- town_geometry %>% filter(NAME %in% pick_towns)
area_town_centroid <- st_centroid(area_towns) # use to place town labels

ggplot() +
  geom_sf(data = area_tracts,
          fill = "gray", colour = "darkgray", show.legend = FALSE) +
  geom_sf(data = area_towns, colour = "yellow", fill = NA) +
  geom_sf_text(data = area_town_centroid, aes(label = NAME), color = "yellow") +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal() +
  labs(title = "New Haven Area Towns",
       subtitle = "with census tract boundaries",
       caption = "Source: US Census, tidycensus package") 


poverty_tracts <- get_acs(geography = "tract",
                          state = "CT",
                          county = "New Haven",
                          geometry = "TRUE", # yes, get tract shapefiles
                          year = 2017,
                          survey = "acs5",
                          variables = B17010_variables[2],
                          summary_var = B17010_variables[1]) %>%
  filter(estimate > 0) %>%
  mutate(pct_poverty = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  left_join(tract_town_df, by = "GEOID") %>%
  filter(TOWN %in% pick_towns)
# poverty_tracts is similar to poverty_acs, but includes geometry and limits to area towns
ggplot() +
  geom_sf(data = poverty_tracts,
          aes(fill = pct_poverty), colour = "lightgray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Pct Poverty", begin = 0.1) +
  geom_sf(data = area_towns, colour = "darkgray", fill = NA) +
  geom_sf_text(data = area_town_centroid, aes(label = NAME), color = "darkgray") +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal() +
  labs(title = "Percentage of Families Below the Poverty Line",
       subtitle = "by census tract (margin of error by tract may be large)",
       caption = "Source: US Census American Community Survey 2013-2017 (variable B17010_002)\ntidycensus R package") 







# I'm saving these to make it easier to re-use same code for a different area.
pick_county = c ("Atascosa", "Austin", "Bastrop", "Bee", "Brazos", "Burleson", "Colorado",
                 "DeWitt", "Dimmit", "Duval", "Fayette", "Frio", "Goliad", "Gonzales", "Grimes",
                 "Karnes", "La Salle", "Lavaca", "Lee", "Leon", "Live Oak", "Madison", "Maverick",
                 "McMullen", "Milam", "Robertson", "Washington", "Webb", "Wilson", "Zavala")

datalist=list()
for (i in 1:length(pick_county)) {
  pick_state = "TX"
  # set cb = TRUE to keep boundaries tied to the coastline
  block_group_geometry <- block_groups(state = pick_state, county = pick_county[i], cb = TRUE)
  # town_geometry <- county_subdivisions(state = pick_state, county = pick_county, cb = TRUE)
  # tract_geometry <- tracts(state = pick_state, county = pick_county, cb = TRUE)
  # let's find which tract is in which town
  # block_group_centroid = st_centroid(block_group_geometry)
  # tract town has the geometry of town, not tract
  # tract_town <-st_join(block_group_geometry, block_group_centroid,
  #                      join = st_intersects,
  #                      suffix = c(".TOWN", ".TRACT"))
  # tract_town_df <- tract_town %>%
  #   as_tibble() %>%
  #   select(TOWN = NAME.TOWN, GEOID = GEOID.TRACT) %>%
  #   mutate(near_nh = (TOWN %in% pick_towns))
  # area_tracts <- tract_geometry %>%
  #   left_join(tract_town_df, by = "GEOID") %>%
  #   filter(TOWN %in% pick_towns)
  # area_towns <- town_geometry %>% filter(NAME %in% pick_towns)
  # area_town_centroid <- st_centroid(area_towns) # use to place town labels
  # 
  
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

xx <- do.call(rbind, datalist)

yy <- xx %>% filter (NAME=="Block Group 2, Census Tract 1.03, Wilson County, Texas")
ggplot() +
  geom_sf(data = xx,
          aes(fill = estimate), colour = "gray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Median Household \nIncome", begin = 0.1 ) +
  labs(title = "") +
  coord_sf(datum = NA, label_axes = "----") + theme_minimal() 


#New Haven post of Rblog

# setup R libraries used in this post
library(tidyverse, quietly = TRUE)
library(tidycensus, quietly = TRUE)
library(sf, quietly = TRUE)
library(tigris, quietly = TRUE)
library(viridis, quietly = TRUE)
library(knitr, quietly = TRUE)
library(scales, quietly = TRUE)
library(kableExtra, quietly = TRUE)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

vars <- load_variables(2020, "acs5", cache = TRUE) %>%
  mutate(table_id = str_sub(name, 1, 6),
         # Race generally is in parentheses after the concept name.
         # But for a few cases, something else is in parentheses first. So I
         # am going to blank out that stuff and then assume whatever I find inside
         # of parentheses is race.
         concept = str_replace_all(concept,
                                   c("\\(IN 2020 INFLATION-ADJUSTED DOLLARS\\)" = "",
                                     "\\(EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS\\)" = "",
                                     "\\(SSI\\)" = "",
                                     "\\(INCLUDING LIVING ALONE\\)" = "",
                                     "\\(IN MINUTES\\)" = "",
                                     "\\(DOLLARS\\)" = "",
                                     "\\(CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI\\)" = "--CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI--",
                                     "\\(CAR, TRUCK, OR VAN\\)" = "--CAR, TRUCK, OR VAN--",
                                     "\\(\\)" = ""
                                   )),
         race = str_extract(concept, "\\(.+\\)"),
         race = str_replace(race, "\\(", ""),
         race = str_replace(race, "\\)", ""))
# I should have been able to do this in one line, but it doesn't seem to work:
# race = str_extract(concept, "\\((.*?)\\)"))
B17010_variables <- vars %>%
  filter(table_id == "B17010", is.na(race)) %>%
  pluck("name")
poverty_acs <- get_acs(geography = "county subdivision", # for CT, that means towns
                       state = "CT",
                       county = "New Haven",
                       geometry = "FALSE", # no map at this time
                       year = 2017,
                       survey = "acs5",
                       variables = B17010_variables[2],
                       summary_var = B17010_variables[1]) %>%
  filter(estimate > 0) %>%
  mutate(TOWN = str_replace(NAME, " town, New Haven County, Connecticut", ""),
         pct_poverty = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 



pick_towns <- c("Woodbridge", "West Haven", "New Haven", "East Haven",
                "Bethany", "Orange", "Milford", "Branford", "Guilford",
                "North Haven", "Madison", "Hamden", "North Branford",
                "Wallingford")
branford <- poverty_acs %>% filter(TOWN == "Branford") # for example
poverty_acs <- poverty_acs %>%
  filter(TOWN %in% pick_towns) %>%
  arrange(desc(pct_poverty))
nh_pct_poverty <- percent(poverty_acs$estimate[[1]] / sum(poverty_acs$estimate), accuracy = 1)
nh_pct_families <- percent(poverty_acs$summary_est[[1]] / sum(poverty_acs$summary_est), accuracy = 1)
poverty_formatted <- poverty_acs %>%
  arrange(desc(pct_poverty)) %>%
  filter(TOWN %in% pick_towns) %>%
  mutate(pct_poverty = percent(pct_poverty, accuracy = 1),
         pct_moe = paste0("±", percent(pct_moe, accuracy = .1)),
         moe = paste0("±", moe),
         summary_moe = paste0("±", summary_moe)) %>%
  select(GEOID, Town = TOWN, `Below Poverty` = estimate, MOE = moe, `Total # of Families` = summary_est, `MOE of Families` = summary_moe, `% Poverty` = pct_poverty, `MOE of %` = pct_moe)
kable(poverty_formatted, format = "markdown", format.args = list(big.mark = ","),
      caption = "Families Below Poverty Line (from Table B17010)") %>%
  kableExtra::kable_styling() %>%
  kableExtra::footnote(general = "Source: US Census American Community Survey 2013-2017 (variable B17010_002)\ntidycensus R package")



pick_towns <- c("Woodbridge", "West Haven", "New Haven", "East Haven",
                "Bethany", "Orange", "Milford", "Branford", "Guilford",
                "North Haven", "Madison", "Hamden", "North Branford",
                "Wallingford")
# I'm saving these to make it easier to re-use same code for a different area.
pick_county = "New Haven"
pick_state = "CT"
# set cb = TRUE to keep boundaries tied to the coastline
town_geometry <- county_subdivisions(state = pick_state, county = pick_county, cb = TRUE)
tract_geometry <- tracts(state = pick_state, county = pick_county, cb = TRUE)
# let's find which tract is in which town
tract_centroid = st_centroid(tract_geometry)
# tract town has the geometry of town, not tract
tract_town <-st_join(town_geometry, tract_centroid,
                     join = st_intersects,
                     suffix = c(".TOWN", ".TRACT"))
tract_town_df <- tract_town %>%
  as_tibble() %>%
  select(TOWN = NAME.TOWN, GEOID = GEOID.TRACT) %>%
  mutate(near_nh = (TOWN %in% pick_towns))
area_tracts <- tract_geometry %>%
  left_join(tract_town_df, by = "GEOID") %>%
  filter(TOWN %in% pick_towns)
area_towns <- town_geometry %>% filter(NAME %in% pick_towns)
area_town_centroid <- st_centroid(area_towns) # use to place town labels

ggplot() +
  geom_sf(data = area_tracts,
          fill = "gray", colour = "darkgray", show.legend = FALSE) +
  geom_sf(data = area_towns, colour = "yellow", fill = NA) +
  geom_sf_text(data = area_town_centroid, aes(label = NAME), color = "yellow") +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal() +
  labs(title = "New Haven Area Towns",
       subtitle = "with census tract boundaries",
       caption = "Source: US Census, tidycensus package") 

poverty_tracts <- get_acs(geography = "tract",
                          state = "CT",
                          county = "New Haven",
                          geometry = "TRUE", # yes, get tract shapefiles
                          year = 2017,
                          survey = "acs5",
                          variables = B17010_variables[2],
                          summary_var = B17010_variables[1]) %>%
  filter(estimate > 0) %>%
  mutate(pct_poverty = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  left_join(tract_town_df, by = "GEOID") %>%
  filter(TOWN %in% pick_towns)
# poverty_tracts is similar to poverty_acs, but includes geometry and limits to area towns
ggplot() +
  geom_sf(data = poverty_tracts,
          aes(fill = pct_poverty), colour = "lightgray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Pct Poverty", begin = 0.1) +
  geom_sf(data = area_towns, colour = "darkgray", fill = NA) +
  geom_sf_text(data = area_town_centroid, aes(label = NAME), color = "darkgray") +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal() +
  labs(title = "Percentage of Families Below the Poverty Line",
       subtitle = "by census tract (margin of error by tract may be large)",
       caption = "Source: US Census American Community Survey 2013-2017 (variable B17010_002)\ntidycensus R package") 




# INCOME IN THE PAST 12 MONTHS B07411_0.5
income_tracts <- get_acs(geography = "tract",
                         state = "CT",
                         county = "New Haven",
                         geometry = "TRUE",
                         year = 2017,
                         survey = "acs5",
                         variables = "B19013_001") %>%
  filter(estimate > 0) %>%
  left_join(tract_town_df, by = "GEOID") %>%
  filter(TOWN %in% pick_towns)
ggplot() +
  geom_sf(data = income_tracts,
          aes(fill = estimate), colour = "gray") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Income", begin = 0.1,
                       trans = "log", breaks = c(20000, 30000, 50000, 100000, 150000)) +
  geom_sf(data = area_towns, colour = "white", fill = NA, size = 0.5) +
  geom_sf_text(data = area_town_centroid, aes(label = NAME), color = "darkgray") +
  coord_sf(datum = NA, label_axes = "----") +
  xlab("") + ylab("") + theme_minimal() +
  labs(title = "Median Household Income (Log Scale)",
       subtitle = "by census tract (margin of error by tract may be large)",
       caption = "Source: US Census American Community Survey 2013-2017 (variable B19013_001)\ntidycensus R package") 



area_towns_order <- c("Bethany", "Woodbridge", "Wallingford", "North Branford",
                      "Orange", "Hamden", "North Haven", "Guilford",
                      "West Haven", "New Haven", "Branford", "Madison",
                      "Milford", "East Haven")
income_tracts <- income_tracts %>%
  mutate(tract_name = str_sub(NAME, 8, (str_locate(NAME, ",")[, 1] - 1)),
         TOWN = factor(TOWN, levels = area_towns_order))
ggplot(data = income_tracts, aes(x = estimate/1000, y = fct_reorder(tract_name, estimate))) +
  geom_point() +
  geom_errorbarh(mapping = aes(xmin = (estimate - moe)/1000, xmax = (estimate + moe)/1000), height = 0) +
  facet_wrap(~ TOWN, scales = "free_y") +
  ylab(NULL) +
  labs(title = "90% Confidence Interval for Median Household Income",
       subtitle = "(Income is Displayed on a Log Scale)",
       caption = "Source: US Census American Community Survey 2013-2017 (variable B19013_001)\ntidycensus R package") +
  scale_x_log10(name = "Income (000's)", breaks = c(20, 30, 50, 100, 150)) +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=5))



vars2 <- vars %>% filter(table_id == "B08006") %>%
  separate(label, into = paste0("t", seq(1, 8)), remove = FALSE, sep = "!!") %>%
  select(-t1)
# to supplement picking out variables manually, in this step we will parse the
# label and use that info to help select variables.
vars2 <- vars2 %>% filter(is.na(t4)) %>%
  mutate(commute_mode = case_when(
    is.na(t3) ~ "All workers",
    str_detect(t3, "^Car") ~ "Vehicle",
    str_detect(t3, "Public") ~ "Public Transport",
    str_detect(t3, "Bicycle") ~ "Bicycle",
    str_detect(t3, "Walked") ~ "Walked",
    str_detect(t3, "Taxi") ~ "Taxi, motorcycle",
    str_detect(t3, "^Worked") ~ "At home",
    TRUE ~ "Other")
  )
for_summary <- vars2 %>% filter(commute_mode == "All workers") %>% pluck("name")
vars2 <- vars2 %>% filter(commute_mode != "Other", commute_mode != "All workers")
# At this point, variables are in vars2$name
# SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK B08006
commuters <- get_acs(geography = "county subdivision", # for CT, that means towns
                     state = "CT",
                     county = "New Haven",
                     geometry = "FALSE",
                     year = 2017,
                     survey = "acs5",
                     variables = vars2$name,
                     summary_var = for_summary) %>%
  filter(estimate > 0) %>%
  left_join(town_geometry %>% select(-NAME), by = "GEOID") %>%
  mutate(TOWN = str_replace(NAME, " town, New Haven County, Connecticut", ""),
         pct = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  filter(TOWN %in% pick_towns) %>%
  left_join(vars2 %>% select(variable = name, commute_mode), by = "variable")
commuters_table <- commuters %>%
  select(commute_mode, pct, TOWN) %>%
  spread(key = commute_mode, value = pct) %>%
  arrange(Vehicle) %>%
  mutate_if(is.numeric, percent)

kable(commuters_table, caption = "Mode of Transportation to Work") %>%
  kableExtra::kable_styling() %>%
  kableExtra::footnote(general = "Source: US Census American Community Survey 2013-2017 (variable B08006)\ntidycensus R package")


white_not_hispanic <- "B01001H_001"
white_alone <- "B02008_001"
black <- "B02009_001"
asian <- "B02011_001"
hispanic <- "B01001I_001"
not_us <- "B05001_006"
# let's get the variables into a data frame where they will be easier to work with
rcodes <- tribble(
  ~code, ~variable,
  "white_not_hispanic" , "B01001H_001",
  "white_alone" , "B02008_001",
  "black" , "B02009_001",
  "asian" , "B02011_001",
  "hispanic" , "B01001I_001",
  "not_us" , "B05001_006")
race_town2 <- get_acs(geography = "county subdivision", # for CT, that means towns
                      state = "CT",
                      county = "New Haven",
                      geometry = "FALSE",
                      year = 2017,
                      survey = "acs5",
                      variables = c(white_not_hispanic, white_alone, black, hispanic, asian, not_us),
                      summary_var = "B01001_001") %>%
  left_join(rcodes, by = "variable") %>%
  mutate(race = factor(code,
                       levels = c("white_alone", "white_not_hispanic", "hispanic", "black", "asian", "not_us"),
                       labels = c("White alone", "White not Hispanic", "Hispanic", "Black", "Asian", "Not US citizen")),
         pct = estimate / summary_est,
         pct_moe = moe_prop(estimate, summary_est, moe, summary_moe))
race_town <- town_geometry %>% left_join(race_town2 %>% select(-NAME), by = "GEOID") %>%
  filter(NAME %in% pick_towns)
race_table <- as_tibble(race_town %>%
                          select(Town = NAME, code, summary_est, pct, pct_moe, race))
race_table <- race_table %>%
  mutate(pct = percent(pct, accuracy = 1.0)) %>%
  select(Town, race, Percent = pct) %>%
  spread(key = race, value = Percent) %>%
  arrange(`White not Hispanic`) 

race_table %>% kable(caption = "Percent Race by Town (Categories are not mutually exclusive)") %>%
  kableExtra::kable_styling() %>%
  kableExtra::footnote(general = "Source: US Census American Community Survey 2013-2017\nvariables B01001H_001, B02008_001, B02009_001, B02011_001, B01001I_001, B05001_006\ntidycensus R package")

