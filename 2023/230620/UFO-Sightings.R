## Packages --------------------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

## Read Data -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 25)

ufo_sightings <- tuesdata$ufo_sightings
places <- tuesdata$places
day_parts_map <- tuesdata$day_parts_map

## Data Wrangling --------------------------------------------------------------

count_by_city <- ufo_sightings %>% 
  filter(country_code == "US") %>% 
  filter(reported_date_time > '2000-01-01') %>% 
  group_by(state, city) %>% 
  summarize(count = n()) %>% 
  ungroup()

places <- places %>% 
  select(city, state, latitude, longitude, population)

combined_df <- inner_join(count_by_city,
                 places,
                 by = c("state" = "state", "city" = "city"),
                 multiple = "all")

top_100_cities <- combined_df %>%
  filter(population > 50000) %>% 
  mutate(rate = count / population) %>% 
  arrange(desc(rate)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 100)

US_map <- map_data("state") %>% 
  filter(region != "AK") %>% 
  filter(region != "HI")

## Load Fonts/Colors -----------------------------------------------------------
font_add_google("Rajdhani")
showtext_auto()

plotFont <- "Rajdhani"

bckgColor <- "#f2f3f5"

pointColorMain <- "#1432b8"

pointColorHighlight <- "#e61b10"

## Plot ------------------------------------------------------------------------

ggplot() +
  geom_path(data = US_map,
            aes(x = long,
                y = lat,
                group = group),
            linewidth = 0.3) +
  geom_point(data = top_100_cities,
             aes(x = longitude,
                 y = latitude,
                 size = rate,
                 ),
             show.legend = FALSE,
             color = ifelse(top_100_cities$rank <= 10, pointColorHighlight, pointColorMain),
             alpha = 0.5) +
  geom_label_repel(data = top_100_cities,
            aes(x = longitude,
                y = latitude,
                label = ifelse(rank <= 10, city, "")),
            min.segment.length = 0,
            fill = "white",
            force = 50,
            fontface = "bold",
            family = plotFont,
            xlim = c(-127.5, Inf),
            seed = 2,
            size = 2.5,
            max.overlaps = Inf) +
  labs(title = "**UFO SIGHTINGS IN THE UNITED STATES**",
       subtitle = "<span style='color:#1432b8'>**Top 100 cities**</span> with the most UFO sightings per capita since 2000. <span style='color:#e61b10'>**Top 10 cities**</span> are labeled. **Size** corresponds to number of sightings. <br>**Note:** Includes only cities with a minimum population of 50,000.",
       caption = "**#TidyTuesday** 2023, Week 25 | **Data** National UFO Reporting Center | **Plot** @jedjohnson4") +
  theme_void() +
  theme(
    plot.title = element_markdown(family = plotFont,
                                  size = 22,
                                  hjust = 0.5),
    plot.subtitle = element_markdown(family = plotFont,
                                     hjust = 0.5),
    plot.caption = element_markdown(family = plotFont,
                                    hjust = 0.5),
    plot.background = element_rect(fill = bckgColor,
                                   color = NA),
    plot.margin = unit(c(15, 10, 15, 10), "pt")
  )

ggsave(filename = "UFO-sightings.png",
       width = 8.75,
       height = 5.75,
       dpi = 750,
       units = "in")
