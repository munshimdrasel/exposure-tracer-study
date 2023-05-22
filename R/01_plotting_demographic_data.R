#script_01- part 02: plotting demographic and income data from 2013-2019 upto block group level for Eagle Ford Shale area,Texas in US

library(fst)
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

#working directory on Hopper cluster
# setwd ("/projects/HAQ_LAB/mrasel/R/exposure-tracer-study")

#demographic variables list
# https://api.census.gov/data/2020/acs/acs5/variables.html

# reading demographic data 

load ("./data/demographic_data/combined.pop.income.block.groups.2013.2019.RData")


demo.combined <- combined.years %>% separate(NAME, c("block_group", "tract", "county", "state" ), ",") 

#removing first blank space of following columns
demo.combined$state <- sub('.', '', demo.combined$state)
demo.combined$county <- sub('.', '', demo.combined$county)
demo.combined$tract <- sub('.', '', demo.combined$tract)
demo.combined$block_group <- sub('.', '', demo.combined$block_group)

#getting texas data only
tx.pop.income <- demo.combined%>% filter(state=="Texas")

#removing county last 7 characters
tx.pop.income$county <- gsub('.{7}$', '', tx.pop.income$county) 

#population data plot on Eagle Ford Shale area
#county information taken from here https://eaglefordshale.com/counties

pick_county = c ("Atascosa", "Austin", "Bastrop", "Bee", "Brazos", "Burleson", "Colorado",
                 "DeWitt", "Dimmit", "Duval", "Fayette", "Frio", "Goliad", "Gonzales", "Grimes",
                 "Karnes", "La Salle", "Lavaca", "Lee", "Leon", "Live Oak", "Madison", "Maverick",
                 "McMullen", "Milam", "Robertson", "Washington", "Webb", "Wilson", "Zavala")

#converting wide data into long data
block.group.pop<-tx.pop.income %>% as.data.frame() %>% filter(county %in% pick_county) %>% 
  dplyr::select(-GEOID, -block_group, -tract, -county, -state, WhiteE, HispanicE, BlackE, AsianE, geometry)
block.group.pop<-melt(block.group.pop, id.vars=c("year", "geometry"), variable.name = "group")

#selecting selected groups White, Hispanic, Black, Asian
block.group.pop$group <- as.character(block.group.pop$group)
block.group.pop <- block.group.pop %>% filter(group %in% c("WhiteE", "HispanicE", "BlackE", "AsianE"))

#removing last character E from WhiteE and so on
block.group.pop$group<- substr(block.group.pop$group,1,nchar(block.group.pop$group)-1)
block.group.pop$group <- as.factor(block.group.pop$group)

#data frame to sf convert
block.group.pop<- st_as_sf(block.group.pop) 

#plotting demographic population
block.group.pop %>% filter(year %in% c(2013, 2019) & group %in% c("White", "Hispanic", "Black", "Asian")) %>% 
  ggplot( aes( fill = value)) +
  geom_sf( size = 0, alpha = .9, color = NA) + 
  scale_fill_viridis_c( name="Population", option="plasma",oob = scales::squish, limit=c(0,1000)) +
  expand_limits( fill = 0) +
  theme_minimal()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size=18),
        legend.text = element_text(angle=-45, size=16),
        legend.text.align = 0.1) + facet_grid(year~group)


#saving plot
ggsave(paste0("efs_population_2013_2019.png"), path = "./plots/", width=8, height=6, units="in")

