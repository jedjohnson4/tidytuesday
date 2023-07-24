## Load Libraries ==============================================================

library(tidyverse)
library(ggtext)
library(showtext)
library(ggridges)
library(colorspace)

## Read Data ===================================================================

tuesdata <- tidytuesdayR::tt_load(2023, week = 29)
detectors <- tuesdata$detectors

## Data Wrangling ==============================================================

df <- detectors %>% 
  filter(model != "Human") %>% 
  filter(prompt != "Simplify like non-native") %>%
  mutate(model = case_when(
    model == "GPT3" ~ "GPT3 Generated",
    model == "GPT4" ~ "GPT4 Generated"
  )) %>% 
  select(.pred_AI, detector, model, prompt)

## Load Fonts/Colors ===========================================================

font_add_google("Rajdhani")
font_add(family = "fa-brands", regular = "~/Desktop/TidyTuesday/fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add(family = "fa-solid", regular = "~/Desktop/TidyTuesday/fonts/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()

DATASET <- "{detectors} R Package"
WEEK_NUMBER <- 29

TEXT_COLOR <- "black"
FONT_FAMILY <- "Rajdhani"
BACKGROUND_COLOR <- "#F1F5F8"

folder_icon <- paste0("<span style='font-family:fa-solid;color:", TEXT_COLOR, "'>&#xf07c;</span>")
github_icon <- paste0("<span style='font-family:fa-brands;color:", TEXT_COLOR, "'>&#xf09b;</span>")
twitter_icon <- paste0("<span style='font-family:fa-brands;color:", TEXT_COLOR, "'>&#xf099;</span>")
space <- paste0("<span style='color:", BACKGROUND_COLOR, "'>-</span>")

plot_caption <- paste0("**#TidyTuesday**, 2023 Week ", WEEK_NUMBER, space, "|", space, folder_icon, space, DATASET, space, "|", space, github_icon, " @jedjohnson4", space, "|", space, twitter_icon, " @jed4johnson")

## Plot Data ===================================================================

plot_labels <- data.frame(x = c(0.4, 0.6),
                          y = c(0.625, 0.625),
                          hjust = c(1, 0),
                          label = c("← Predicted Human", "Predicted AI →"))

df %>% 
  ggplot(aes(x = .pred_AI,
             y = detector,
             fill = detector)) +
  geom_density_ridges(show.legend = F) +
  facet_wrap(vars(model),
             scales = "free_x") +
  geom_text(data = plot_labels,
            inherit.aes = FALSE,
            aes(x = x,
                y = y,
                label = label,
                hjust = hjust),
            size = 2.5) +
  labs(title = "Detecting AI Generated Content",
       subtitle = "As GPT models become more advanced, can GPT detection models keep up? Plotted below is the ability<br>of various GPT detectors to determine the source of various texts produced by either GPT3 or GPT4. For<br>GPT3 texts, the prompts 'plain' (85%) and 'elevate using literary' (15%) were used. For GPT4 texts, the<br>prompts 'elevate using technical' (61%) and 'enhance like native' (39%) were given.",
       caption = plot_caption) +
  scale_x_continuous(breaks = c(0.5)) +
  coord_cartesian(clip = "off") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_void() +
  theme(
    plot.title = element_text(family = FONT_FAMILY,
                              color = "black",
                              face = "bold",
                              size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = FONT_FAMILY,
                                     color = "black",
                                     size = 10,
                                     margin = margin(t = 5, b = 7.5)),
    plot.caption = element_markdown(family = FONT_FAMILY,
                                    size = 7,
                                    hjust = 0.5,
                                    margin = margin(t = 10)),
    plot.caption.position = "plot",
    strip.text = element_text(family = FONT_FAMILY,
                              color = "black",
                              face = "bold",
                              size = 10,
                              margin = margin(b = 5)),
    axis.text.y = element_text(family = FONT_FAMILY,
                               color = "black",
                               size = 8,
                               hjust = 1,
                               vjust = 0.1,
                               margin = margin(r = 4)),
    panel.grid.major.y = element_line(color = "grey70",
                                      linewidth = 0.25,
                                      linetype = "solid"),
    panel.grid.major.x = element_line(color = "grey70",
                                      linewidth = 0.25,
                                      linetype = "dashed"),
    plot.margin = margin(10,10,10,10),
    plot.background = element_rect(color = NA,
                                   fill = BACKGROUND_COLOR)
  )

ggsave(filename = "AI_Detectors.png",
       width = 6,
       height = 4,
       dpi = 800,
       units = "in")