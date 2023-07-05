## Load Libraries --------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(ggrepel)

## Read Data -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 27)

historical_markers <- tuesdata$`historical_markers`
no_markers <- tuesdata$`no_markers`

## Wrangle Data ----------------------------------------------------------------

texas_county_markers <- historical_markers %>% 
  filter(state_or_prov == "Texas") %>% 
  mutate(county_or_parish = tolower(county_or_parish),
         county_or_parish = str_remove(county_or_parish, " county")) %>% 
  group_by(county_or_parish) %>% 
  summarize(count = n()) %>% 
  mutate(classification = case_when(
    count > 0 & count <= 25 ~ "1-25",
    count > 25 & count <= 50 ~ "26-50",
    count > 50 & count <= 100 ~ "51-100",
    count > 100 & count <= 200 ~ "101-200",
    count > 200 ~ "201+",
    TRUE ~ "Zero"
 ))

texas_county_markers$classification <- factor(texas_county_markers$classification,
            levels = c("Zero", "1-25", "26-50", "51-100", "101-200", "201+"))

texas_map <- map_data("county") %>% 
  filter(region == "texas") %>% 
  rename(county = subregion) %>% 
  mutate(county = case_when(
    county == "de witt" ~ "dewitt",
    TRUE ~ county
  ))

combined <- inner_join(x = texas_county_markers,
                       y = texas_map,
                       by = c("county_or_parish" = "county"),
                       multiple = "all")

important_facts <- historical_markers %>% 
  filter(marker_id == 25753 | marker_id == 128151 | marker_id == 125882 | marker_id == 30709) %>% 
  rename(long = longitude_minus_w,
         lat = latitude_minus_s) %>% 
  mutate(title = case_when(
    title == "Austin High School" ~ "AUSTIN, capital since 1839",
    title == "The Alamo Cenotaph" ~ "BATTLE OF THE ALAMO, 1836",
    title == "Gonzales Cannon" ~ "BATTLE OF GONZALES, 1835",
    title == "Battle of San Jacinto" ~ "BATTLE OF SAN JACINTO, 1836",
    TRUE ~ title
  )) %>% 
  mutate(nudge_x = case_when(
    marker_id == 25753 ~ -10.25,
    marker_id == 128151 ~ 8,
    marker_id == 125882 ~ 7,
    marker_id == 30709 ~ -4.5,
    TRUE ~ 0
  )) %>% 
  mutate(nudge_y = case_when(
    marker_id == 25753 ~ 2.5,
    marker_id == 128151 ~ 2.5,
    marker_id == 125882 ~ 0.25,
    marker_id == 30709 ~ -2,
    TRUE ~ 0
  ))

paragraph <- data.frame(x = -96.5,
                        y = 27.2,
                        label = "In 1835, the Texas Revolution began with the Battle of Gonzales.\nSanta Anna led Mexican troops to victory at the Battle of the Alamo.\nAfter a Texan victory at the Battle of San Jacinto, the Republic of Texas\nwas formed. The territory would later be annexed by the United States \nin 1845, leading to the Mexican-American war.")

## Load Colors/Fonts -----------------------------------------------------------

font_add_google("Rajdhani")
showtext_auto()

PLOT_FONT <- "Rajdhani"

COLOR_SCALE <- c("#9da7b0", "#7d8f9f", "#5c788e", "#39617d", "#004c6d")

## Plot Data -------------------------------------------------------------------

combined %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = classification)) +
  geom_polygon() +
  geom_point(inherit.aes = FALSE,
             data = important_facts,
             aes(x = long,
                 y = lat),
             shape = 15) + 
  geom_label_repel(inherit.aes = FALSE,
             data = important_facts,
             aes(x = long,
             y = lat,
             label = title),
             family = PLOT_FONT,
             label.r = 0,
             nudge_x = important_facts$nudge_x,
             nudge_y = important_facts$nudge_y) +
  geom_text(inherit.aes = FALSE,
                  data = paragraph,
                  aes(x = x,
                      y = y,
                      label = label),
            family = PLOT_FONT,
            size = 2.5,
            hjust = 0) +
  coord_map(clip = "off") +
  scale_fill_manual(values = COLOR_SCALE) +
  labs(title = "TEXAN PRIDE, HISTORICAL MARKERS WITHIN THE STATE",
       subtitle = "Texas has over **10,000** historical markers, more than **3x** any other state. Where are they located? <br>Featuring historical markers related to the Texas Revolution.",
       fill = "No. of Historical Markers",
       caption = "**#TidyTuesday**, 2023 Week 27 | **Data** Historical Marker Database | **Plot** @jedjohnson4") +
  theme_void() +
  theme(
    text = element_text(family = PLOT_FONT),
    plot.title = element_text(face = "bold",
                              size = 20),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(hjust = 0.5,
                                    margin = unit(c(0,0,5,0), "pt")),
    plot.background = element_rect(fill = "white",
                                   color = NA),
    legend.position = c(0.7, 0.875),
    legend.direction = "horizontal",
  ) +
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title.hjust = 0.5,
                             keywidth = unit(30, "pt"),
                             keyheight = unit(7.5, "pt")))

ggsave(filename = "Texas_Historical_Markers.png",
       dpi = 700,
       height = 5,
       width = 7,
       units = "in")
