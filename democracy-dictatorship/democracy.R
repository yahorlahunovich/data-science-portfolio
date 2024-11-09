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
    TRUE ~ country_name # Keep other country names unchanged
  ))


ggplot(data = world_with_count) +
  geom_sf(aes(fill = cut(count, breaks = c(0, 10, 30, 50, 70, Inf), include.lowest = TRUE)), color = "white") +
  scale_fill_manual(
    values = c("lightgrey", "#FFD700", "#FFA500", "#FF4500", "#8B0000"),
    labels = c("No data", "Low inequality (<30)", "Moderate inequality (30-40)", "High inequality (41-50)", "Extreme inequality (>50)"),
    na.value = "lightgrey",
    guide = guide_legend(
      title = "Years of Dictatorship",
      title.position = "top",
      title.hjust = 0.5,
      direction = "vertical",
      override.aes = list(size = 6)  # Increases legend square size
    )
  ) +
  labs(
    title = "Global Dictatorship Trends by Country",
    subtitle = "Years of Dictatorship from 1950 to 2020",
    caption = "Made by Yahor Lahunovich\nData Source: [Add Source]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(color = "darkblue", hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(color = "darkgrey", hjust = 0.5, size = 14, face = "italic"),
    plot.caption = element_text(color = "grey40", face = "italic", size = 10, hjust = 1),
    legend.position = c(0.2, 0.3),  # Place the legend near South America
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),  # Size of the legend color squares
    legend.spacing.y = unit(0.2, "cm"),  # Space between legend items
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")
  elections <- df %>% 
  group_by(country_name, has_free_and_fair_election) %>% 
  summarise(count = n())

# democracy

democracy <- df %>% 
  group_by(country_name, is_democracy) %>% 
  summarise(count = n())


# is female monarch
