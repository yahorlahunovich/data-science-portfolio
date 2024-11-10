library(maps)
library(mapdata)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# dataset https://xmarquez.github.io/democracyData/reference/pacl_update.html#see-also-1
tuesdata <- tidytuesdayR::tt_load('2024-11-05')
democracy_data <- tuesdata$democracy_data

str(democracy_data)

belarus <- democracy_data[democracy_data$country_name == "Belarus", ]
poland <- democracy_data[democracy_data$country_name == "Poland", ]
ukraine <- democracy_data[democracy_data$country_name == "Ukraine", ]

df <- democracy_data

dictatorship <- df %>% 
  filter(regime_category %in% c("Royal dictatorship", "Civilian dictatorship", "Military dictatorship")) %>% 
  group_by(country_name) %>% 
  summarise(count = n())

df <- df %>%
  mutate(country_name = case_when(
    country_name == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
    country_name == "Cape Verde" ~ "Cabo Verde",
    country_name == "Central African Republic" ~ "Central African Rep.",
    country_name == "Congo, Dem. Rep." ~ "Dem. Rep. Congo",
    country_name == "Congo, Republic of" ~ "Congo",
    country_name == "Czech Republic" ~ "Czechia",
    country_name == "Côte d`Ivoire" ~ "Côte d'Ivoire",
    country_name == "Dominican Republic" ~ "Dominican Rep.",
    country_name == "Equatorial Guinea" ~ "Eq. Guinea",
    country_name == "Gambia, The" ~ "Gambia",
    country_name == "Korea, People's Republic" ~ "North Korea",
    country_name == "Korea, Republic of" ~ "South Korea",
    country_name == "Sao Tome and Principe" ~ "São Tomé and Principe",
    country_name == "Slovak Republic" ~ "Slovakia",
    country_name == "South Sudan" ~ "S. Sudan",
    country_name == "Swaziland" ~ "eSwatini",
    TRUE ~ country_name
  ))

world <- ne_countries(scale = "medium", returnclass = "sf")
world_with_count <- world %>%
  left_join(dictatorship, by = c("name" = "country_name")) %>% 
  filter(name != "Antarctica")

world_with_count <- world_with_count %>%
  mutate(
    count_category = case_when(
      is.na(count) ~ "0 years",
      count == 0 ~ "0 years",
      count > 0 & count <= 10 ~ "<10 years",
      count > 10 & count <= 30 ~ "10-30 years",
      count > 30 & count <= 50 ~ "30-50 years",
      count > 50 ~ "50+ years"
    )
  )

ggplot(data = world_with_count) +
  geom_sf(aes(fill = count_category), color = "white") +
  labs(
    title = "Global Dictatorship Trends by Country",
    subtitle = "Years of Dictatorship from 1950 to 2020",
    caption = "Made by Yahor Lahunovich\nData Source: [Add Source]"
  ) +
  scale_fill_manual(
    values = c(
      "<10 years" = "#FFD700",
      "0 years" = "lightgrey",
      "10-30 years" = "#FFA500",
      "30-50 years" = "#FF4500",
      "50+ years" = "#8B0000"
    ),
    guide = guide_legend(
      title = "Years of Dictatorship",
      title.position = "top",
      title.hjust = 0.5,
      direction = "vertical",
      override.aes = list(size = 6)
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(color = "black", hjust = 0.5, size = 29, face = "bold"),
    plot.subtitle = element_text(color = "darkgrey", hjust = 0.5, size = 14, face = "italic"),
    plot.caption = element_text(color = "grey40", face = "italic", size = 10, hjust = 1),
    legend.position = c(0.15, 0.35), 
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")




# democracy

democracy <- df %>% 
  group_by(country_name, is_democracy) %>% 
  summarise(count = n())


# is female monarch

female <- df %>% 
  mutate(female_monarch = ifelse(is_female_monarch == TRUE, 1, ifelse(
    is_female_president == TRUE, 1, 0
  ))) %>% 
  group_by(country_name, female_monarch) %>% 
  summarise(count = n())
f <- df %>% 
  group_by(country_name, monarch_name, is_female_monarch, is_female_president) %>% 
  summarise(count = n())

# fair election

elections <- df %>% 
  group_by(country_name, has_free_and_fair_election) %>% 
  summarise(count = n())