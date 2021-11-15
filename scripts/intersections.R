#' intersections.R
#' 
#' Plots for the intersections around Fullarton / GO Road 
#' 
#'
#' Data Source
#' 
#' https://data.sa.gov.au/data/dataset/traffic-volumes-on-top-40-intersections-in-sa
#' 
#' 4 June 2021
#' 
#' 

# Libraries ---------------------------------------------------------------------------------------------------------


library(tidyverse)
library(janitor)
library(tidyquant)
library(scales)
library(ggthemes)


ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) 
  }

# Data Gathering ----------------------------------------------------------------------------------------------------

# All of the data have been downloaded to the data directory from the repository above 

# 2017 data
data_2017_tbl <- read_csv(file = "data/top-40-at-grade-arterial-road-intersections-for-all-vehicles-2017.csv", skip=2) %>%
  clean_names() %>%
  mutate(
    recorded_year = 2017
  )

# 2018 data 
data_2018_tbl <- read_csv(file = "data/top-40-at-grade-arterial-road-intersections-for-all-vehicles-2018.csv", skip=2) %>%
  clean_names() %>%
  mutate(
    recorded_year = 2018
  )

# 2019 data
data_2019_tbl <- read_csv(file = "data/top-40-at-grade-arterial-road-intersections-for-all-vehicles-2019.csv", skip=2) %>%
  clean_names() %>%
  mutate(
    recorded_year = 2019
  )

# 2020 data 
data_2020_tbl <- read_csv(file = "data/top-40-at-grade-arterial-road-intersections-for-all-vehicles-2020.csv", skip=2) %>%
  clean_names() %>%
  mutate(
    recorded_year = 2020
  )

# Tidy ----------------------------------------------------------------------------------------------

# combine the data into a single data frame
data_combined_tbl <- bind_rows(data_2017_tbl,
                               data_2018_tbl,
                               data_2019_tbl,
                               data_2020_tbl)

#data_combined_tbl <- data_combined_tbl %>%
#  mutate(recorded_year = make_date(year=recorded_year))


# Explore -------------------------------------------------------------------------------------------

# data frame structure
data_combined_tbl %>% glimpse()

# look at the distinct names for the location
# data_combined_tbl %>%
#   distinct(location) %>%
#   arrange(location) %>%
#   View()

# so looks like we just need the following locations 
data_combined_tbl %>%  
  filter(str_detect(location, "GLEN OSMOND") | str_detect(location, "FULLARTON") | str_detect(location, "UNLEY") | str_detect(location, "PORTRUSH"))  %>%
  filter(! str_detect(location, "PAYNEHAM")) %>%
  distinct(location) 
 
# assign the locations we want to a list 
location_lst <- data_combined_tbl %>%  
  filter(str_detect(location, "GLEN OSMOND") | str_detect(location, "FULLARTON") | str_detect(location, "UNLEY") | str_detect(location, "PORTRUSH"))  %>%
  filter(! str_detect(location, "PAYNEHAM")) %>%
  distinct(location) %>%
  pull()
 

# Visualise -----------------------------------------------------------------------------------------------------

# plot out the data in time series 
intersections_plot <- data_combined_tbl %>%  
  
  # convert values into thousands
  mutate(
    vehicle_exposure = vehicle_exposure / 1000
  ) %>%
  
  # only get the required locations
  filter(location %in% location_lst) %>% 
  
  # clean up location so that it is a bit tidier and shorter 
  mutate(
    location_text = str_remove_all(location, " ROAD"),
    location_text = str_trim(location, side="both")
  ) %>%
  
  # convert to title case
  mutate(
    location_text = str_to_title(location_text)
  ) %>%
  
  mutate(
    location_text = str_replace_all(location_text, "Road", "Rd")
  ) %>%
  
  # plot a line plot with points for the recorded year vs vehicle exposure with colour by location and a label of the number
  ggplot(aes(x=recorded_year, y=vehicle_exposure, colour=location_text, label=vehicle_exposure)) +
  geom_line(size=2) +
  geom_point(size=3) +
  geom_text(aes(label=vehicle_exposure),hjust=0, vjust=2) +
  
  # facet wrap by location
  facet_wrap(vars(location_text), ncol = 1) +
  ylim(0,80) +
  
  # theme stuff
  theme_clean() +
  scale_colour_tableau() +
  
  theme(legend.position = "") +
  
  # informative labels
  labs(
    title="Intersection Traffic Volumes in or around Glen Osmond Road",
    subtitle = str_glue("Derived from \"Traffic volumes on top 40 intersections in SA\". 
    
                         Source: https://data.sa.gov.au/data/dataset/traffic-volumes-on-top-40-intersections-in-sa
                         
                         Vehicle Exposure: A measure of traffic volumes passing through intersections. It is calculated 
                         by adding the estimated Annual Average Daily Traffic (est. AADT) volumes on all arms of the 
                         intersection and dividing the result by two. Vehicle Exposure estimates are based on data 
                         obtained from the most recent single day 11 hour manual turning count surveys conducted at 
                         the intersections."),
    x = "Recorded Year",
    y = "Vehicle Exposure ('000)",
    caption = "4 June 2021  @morebento https://github.com/morebento/adelaide_intersections ",
    colour = "Location"
  ) 

# save to a file
ggsave(
  file = "plots/glen_osmond_road_intersection_traffic.pdf", 
  plot = intersections_plot, 
  width = 210, 
  height = 297, 
  units = "mm"
)
