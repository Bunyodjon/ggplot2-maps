# Homework 5 
# Author: Bunyod 


library(tidyverse)
library(readxl)
# setwd("")

## USDA Food Environment Atlas

#https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/
excel_sheets("DataDownload.xlsx")
# County names and information
counties <- read_excel(path="DataDownload.xlsx", sheet="Supplemental Data - County")
head(counties)
counties$State <- tolower(counties$State)
counties$County <- tolower(counties$County)

# County Orchard Acreage from 2012
local_food <- read_excel(path="DataDownload.xlsx", sheet="LOCAL") %>%
  select(FIPS, County, ORCHARD_ACRES12, GHVEG_FARMS12)
head(local_food)
local_food$County <- tolower(local_food$County)


## Country boundary data from ggplot2
county_outlines <- map_data("county") 
head(county_outlines)


# Merge data sets  
county_food <- county_outlines %>% 
  left_join(counties, by=c("region"="State", "subregion"="County")) %>% 
  left_join(local_food, by=c("FIPS"="FIPS", "subregion"="County"))

# transform Orchard_Acres12 with log10 
county_food$log10orchard <- log10(county_food$ORCHARD_ACRES12)


# plot the graphs 

plot1 <- ggplot() +
    geom_polygon(data=county_food, aes(x=long, y=lat, fill=ORCHARD_ACRES12, group=group)) +
  scale_fill_continuous("Acres", trans = 'log10', low="#3DA4F6",high="#16324C") +
  coord_map() +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18, hjust=.5)) +
  labs(title="Acres of Orchards in US Counties",
       caption="Data source: USDA Food Environment Atlas, 2016")

# view the plot 
plot1
