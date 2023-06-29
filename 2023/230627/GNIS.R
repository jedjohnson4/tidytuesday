## Load Packages ---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(gifski)
library(magick)
library(ggtext)
library(showtext)

## Read Data -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 26)
us_place_names <- tuesdata$us_place_names

US_map <- map_data("state")

## Wrangle Data ----------------------------------------------------------------

most_common_names <- us_place_names %>% 
  rename(long = prim_long_dec,
         lat = prim_lat_dec,
         name = feature_name) %>% 
  filter(state_name != "Alaska",
         state_name != "Hawaii",
         lat > 20) %>% 
  select(name,
         long,
         lat) %>% 
  group_by(name) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(-count) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 15) %>% 
  mutate(rank_char = ifelse(nchar(rank) == 1, paste0("0", rank), rank))

most_common_names_list <- most_common_names$name  

most_common_places <- us_place_names %>% 
  rename(long = prim_long_dec,
         lat = prim_lat_dec,
         name = feature_name) %>% 
  filter(state_name != "Alaska",
         state_name != "Hawaii",
         lat > 20) %>% 
  select(name,
         long,
         lat) %>% 
  filter(name %in% most_common_names_list)
  

most_common_places <- inner_join(most_common_names,
                                 most_common_places,
                                 by = c("name" = "name"),
                                 multiple = "all")

## Fonts/Colors ----------------------------------------------------------------

font_add_google("Rajdhani")
showtext_auto()

PLOT_FONT <- "Rajdhani"

UNHIGHLIGHTED_COLOR <- "#cdcfd1"

HIGHLIGHT_COLOR <- "#ffa600"

BACKGROUND_COLOR <- "white"

## Plots -----------------------------------------------------------------------

# Bar Plot ---------------------------------------------------------------------


BarPlot <- most_common_names %>% 
  ggplot(aes(x = reorder(name, -rank),
             y = count)) +
  
  ## Static Bar (do not include gganimate iteration variable)
  geom_bar(data = most_common_names %>% select(-rank_char),
           inherit.aes = FALSE,
           aes(x = reorder(name, -rank),
               y = count),
           fill = UNHIGHLIGHTED_COLOR,
           stat = "identity"
           ) +
  
  ## Static Name Labels
  geom_text(data = most_common_names %>% select(-rank_char),
            inherit.aes = FALSE,
            aes(x = reorder(name, -rank),
               y = count,
               label = name),
            hjust = 0,
            nudge_y = 5,
            size = 2,
            fontface = "bold",
            color = "grey70",
            family = PLOT_FONT
            ) +
  
  ## Dynamic Bar
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = HIGHLIGHT_COLOR) +
  
  ## Dynamic Name Label
  geom_text(aes(label = name),
            hjust = 0,
            nudge_y = 5,
            size = 2,
            fontface = "bold",
            color = "black",
            family = PLOT_FONT
  ) +
  
  labs(title = "15 MOST COMMON GEOGRAPHICAL NAMES IN US",
       subtitle = "The Geographical Names Information System contains named features in the United States, including \npopulated places, lakes, streams, summitts, valleys, and ridges. Below are the 15 most common \ngeographical names within the contiguous United States.") +
  
  coord_flip(clip = "off") +
  
  theme_void() +
  
  ## gganimate
  transition_manual(rank_char) +
  
  theme(
    text = element_text(family = PLOT_FONT),
    plot.title = element_text(size = 14,
                              face = "bold",
                              margin = unit(c(0,0,5,0), "pt")),
    plot.subtitle = element_text(size = 6,
                                 margin = unit(c(0,5,7.5,0), "pt")),
    plot.margin = unit(c(15,15,50,7.5), "pt"),
    plot.background = element_rect(fill = BACKGROUND_COLOR,
                                   color = NA),
    panel.grid.major.x = element_line(color = "#e8e8e8"),
    axis.text.x = element_text(size = 6,
                               color = "black",
                               margin = unit(c(5,0,0,0), "pt"))
  )

# ggsave(
#   filename = "barplot.png",
#   dpi = 500,
#   width = 4,
#   height = 4,
#   units = "in"
# )


BarPlotGif <- animate(BarPlot,
                      duration = 30,
                      fps = 0.5,
                      res = 500,
                      width = 4,
                      height = 4,
                      units = "in",
                      renderer = gifski_renderer())

# US Map -----------------------------------------------------------------------

MapPlot <- most_common_places %>%
  ggplot(aes(x = long,
             y = lat)) +
  
  geom_path(data = US_map,
            aes(group = group),
            linewidth = 0.25,
            color = "black") +
  
  geom_point(color = HIGHLIGHT_COLOR,
             alpha = 0.8) +
  
  labs(title = '{most_common_names_list[as.integer(frame_time)]}',
       caption = "**#TidyTuesday**, 2023 Week 26 | **Data** Geographic Names Information System | **Plot** @jedjohnson4") +
  
  transition_time(rank) +
  
  theme_void() + 
  
  theme(plot.caption = element_markdown(hjust = 1,
                                        size = 5,
                                        margin = unit(c(18.5,7.5,0,0), "pt")),
        plot.title = element_text(face = "bold",
                                  size = 14,
                                  hjust = 0.5),
        plot.margin = unit(c(30, 0, 15, 0), "pt"),
        plot.background = element_rect(fill = BACKGROUND_COLOR,
                                       color = NA))

MapPlotGif <- animate(MapPlot,
                      duration = 30,
                      fps = 0.5,
                      res = 500,
                      width = 5,
                      height = 4,
                      units = "in",
                      renderer = gifski_renderer())

## Combine GIFs ----------------------------------------------------------------

a_gif <- image_read(BarPlotGif)
b_gif <- image_read(MapPlotGif)

new_gif <- image_append(c(a_gif[1], b_gif[1]))

for(i in 2:15){
  combined <- image_append(c(a_gif[i], b_gif[i]))
  new_gif <- c(new_gif, combined)
}

anim_save(
  animation = new_gif,
  filename = "GNIS.gif",
  dpi = 500,
  width = 9,
  height = 4,
  units = "in"
)
