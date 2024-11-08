library(maps)
library(mapdata)
library(dplyr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2024-11-05')
democracy_data <- tuesdata$democracy_data

str(democracy_data)

belarus <- democracy_data[democracy_data$country_name == "Belarus", ]

df <- democracy_data

dictatorship <- df %>% 
  filter(regime_category %in% c("Royal dictatorship",                                                       
                                "Civilian dictatorship",                                                    
                                "Military dictatorship" ))