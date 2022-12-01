library(readr)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(purrr)
library(scales)
library(ggthemes)
library(broom)
library(forcats)
library(geofacet)
library(tidyr)
library(dplyr)

homicides_url <- paste0("https://raw.githubusercontent.com/washingtonpost/",
                          "data-homicides/master/homicide-data.csv")
homicides <- read.csv(homicides_url)
milwaukee_tracts <- tracts("WI", "Milwaukee", 
cb = TRUE, class = "sf") %>% 
st_as_sf(milwaukee_tracts, coords = c("lon", "lat")) %>% 
st_set_crs(4269) 

class(milwaukee_tracts)
class(milwaukee_tracts$geometry)

milwaukee_homicides <- homicides %>% 
filter(city == "Milwaukee") %>% 
select(victim_race, uid, city, disposition, lat, lon) %>% 
dplyr::mutate(victim_race = forcats::fct_lump(victim_race, n = 3)) %>% 
mutate(unsolved_homicides = as.numeric(disposition != "Closed by arrest"))

plottybaby2 <- ggplot() + 
geom_sf(data = milwaukee_tracts, color = "black") + 
geom_point(data = milwaukee_homicides, aes(x = lon, y = lat, 
    col = victim_race), size = .52) + labs(x = "Longitude",
    y = "Latitude", col = "Victim Race") + 
  labs(x = "Longitude", y = "Latitude", col = "Victim Race") + theme_void()

milwaukee_labels <- as_labeller(c('0' = "Solved",
                                  '1' = "Unsolved"))

final_plotty_baby <- plottybaby2 + facet_wrap(.~unsolved_homicides, 
  ncol = 2, labeller = milwaukee_labels) +
ggtitle("Homicides in Milwaukee, WI") + 
theme(plot.title = element_text(hjust = .5))

final_plotty_baby











