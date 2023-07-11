## Load Packages ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)

## Read Data -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 28)
global_temps <- tuesdata$global_temps

## Wrangle Data ----------------------------------------------------------------

annual_trend <- global_temps %>% 
  rename(year = Year,
         deviation = `J-D`) %>% 
  select(year, deviation) %>% 
  filter(year < 2023)

for(number in 1:9){
  new_col_name <- paste0("lag", number)
  
  annual_trend <- annual_trend %>% 
    mutate(!!sym(new_col_name) := lag(deviation, number))
}

annual_trend <- annual_trend %>% 
  mutate(moving_avg = (deviation + lag1 + lag2 + lag3 + lag4 + lag5 + lag6 + lag7 + lag8 + lag9) / 10)

sd <- apply(annual_trend[,2:11], 1, sd)

annual_trend <- cbind(annual_trend, sd = sd)

## Load Fonts/Colors -----------------------------------------------------------

font_add_google("Rajdhani")
font_add(family = "fa-brands", regular = "~/Desktop/TidyTuesday/fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add(family = "fa-solid", regular = "~/Desktop/TidyTuesday/fonts/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()

DATASET <- "GISS Surface Temperature Analysis v4"
WEEK_NUMBER <- 28

TEXT_COLOR <- "black"
FONT_FAMILY <- "Rajdhani"
BACKGROUND_COLOR <- "#fffdf7"
HIGHLIGHT_COLOR <- "#bd0205"
AXIS_TEXT_SIZE <- 8

folder_icon <- paste0("<span style='font-family:fa-solid;color:", TEXT_COLOR, "'>&#xf07c;</span>")
github_icon <- paste0("<span style='font-family:fa-brands;color:", TEXT_COLOR, "'>&#xf09b;</span>")
twitter_icon <- paste0("<span style='font-family:fa-brands;color:", TEXT_COLOR, "'>&#xf099;</span>")
space <- paste0("<span style='color:", BACKGROUND_COLOR, "'>-</span>")

plot_caption <- paste0("**#TidyTuesday**, 2023 Week ", WEEK_NUMBER, space, "|", space, folder_icon, space, DATASET, space, "|", space, github_icon, " @jedjohnson4", space, "|", space, twitter_icon, " @jed4johnson")

## Plot ------------------------------------------------------------------------

annual_trend %>% 
  ggplot() +
  geom_ribbon(aes(x = year,
                  ymin = moving_avg - sd,
                  ymax = moving_avg + sd),
              fill = "gray",
              alpha = 0.5) + 
  geom_line(aes(x = year,
                y = moving_avg),
            color = "gray") +
  labs(title = "RISING GLOBAL TEMPERATURE",
       subtitle = "The GISS Surface Tempearture Analysis estimates global surface temperature change.<br>Plotted are the annual temperature deviations from the 1951-1980 average temperature<br>with a 10-year rolling mean \u00B11 standard deviation plotted in gray. <span style='color:#bd0205'>**Since 1980,**</span> the average<br>annual temperature deviation has continually increased.",
       caption = plot_caption) +
  geom_path(aes(x = year,
                y = deviation),
            lineend = "round",
            color = ifelse(annual_trend$year > 1977, HIGHLIGHT_COLOR, "black"),
            linewidth = 1) +
  geom_segment(aes(x = 1880,
                   xend = 2005,
                   y = 0,
                   yend = 0),
               linetype = "dashed") +
  annotate("text",
           x = 2020,
           y = 0,
           label = "Average\nfrom 1951-1980",
           family = FONT_FAMILY,
           size = 2.5) +
  geom_curve(aes(x = 2012,
                 xend = 2000,
                 y = -0.09,
                 yend = -0.04),
             curvature = -0.7,
             linewidth = 0.25,
             arrow = arrow(length = unit(2.5, "pt"), type = "closed")) +
  scale_x_continuous(breaks = c(1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  scale_y_continuous(labels = paste(c(-0.5, 0, 0.5, 1), "\u00b0C")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = BACKGROUND_COLOR,
                                   color = NA),
    panel.grid.major.y = element_line(color = "grey90"),
    axis.text = element_text(family = FONT_FAMILY,
                             size = AXIS_TEXT_SIZE),
    plot.margin = margin(c(10,10,10,10)),
    plot.title = element_text(family = FONT_FAMILY,
                              size = 18,
                              face = "bold"),
    plot.subtitle = element_markdown(family = FONT_FAMILY,
                                     size = 8),
    plot.caption = element_textbox_simple(family = FONT_FAMILY,
                                          halign = 0.5,
                                          size = 6,
                                          margin = margin(10,0,0,0)),
    plot.caption.position = "plot"
  )

ggsave(filename = "Global-Temps.png",
       dpi = 1000,
       width = 5,
       height = 5,
       units = "in")