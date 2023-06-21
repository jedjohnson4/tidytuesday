library(tidyverse)
library(reshape2)
library(showtext)

font_add_google("Merriweather")
showtext_auto()

plotText <- "Merriweather"

data <- tidytuesdayR::tt_load(2023, week = 24)

df <- data$safi_data

foodInsecurity <- df %>% 
  select(village, months_lack_food)

for(month in month.abb){
  foodInsecurity <- foodInsecurity %>% 
    mutate(!!sym(month) := case_when(
      str_detect(string = months_lack_food, pattern = month) ~ 1,
      .default = 0
    ))
}

byVillage <- foodInsecurity %>% 
  group_by(village)

for(month in month.abb){
  if(month == "Jan"){
    output <- byVillage %>% 
      summarize(count = n(),
        !!sym(month) := round(sum(!!sym(month)) / count, 2) * 100)
  }else{
    output2 <- byVillage %>% 
      summarize(count = n(),
                !!sym(month) := round(sum(!!sym(month)) / count, 2) * 100)
    output <- cbind(output, output2)
  }
}

outputCleaned <- output %>% 
  select(c(1, month.abb))

outputCleanedMelted <- melt(outputCleaned, id.vars = "village")

outputCleanedMelted <- outputCleanedMelted %>% 
  mutate(xpos = case_when(
    variable == "Jan" | variable == "May" | variable == "Sep" ~ 1,
    variable == "Feb" | variable == "Jun" | variable == "Oct" ~ 2,
    variable == "Mar" | variable == "Jul" | variable == "Nov" ~ 3,
    variable == "Apr" | variable == "Aug" | variable == "Dec" ~ 4,
  )) %>% 
  mutate(ypos = case_when(
    variable == "Jan" | variable == "Feb" | variable == "Mar" | variable == "Apr" ~ 2,
    variable == "May" | variable == "Jun" | variable == "Jul" | variable == "Aug" ~ 1.5, 
    variable == "Sep" | variable == "Oct" | variable == "Nov" | variable == "Dec" ~ 1,
  ))

monthPositions <- data.frame(x = c(1, 2, 3, 4,
                                   1, 2, 3, 4,
                                   1, 2, 3, 4),
                             y = c(2, 2, 2, 2,
                                   1.5, 1.5, 1.5, 1.5,
                                   1, 1, 1, 1),
                             month = c("Jan", "Feb", "Mar", "Apr",
                                       "May", "Jun", "Jul", "Aug",
                                       "Sep", "Oct", "Nov", "Dec"))

outputCleanedMelted %>% 
  ggplot(aes(x = xpos, y = ypos, fill = value)) +
  geom_tile(height = 0.5,
            width = 1) +
  scale_fill_continuous(low = "#f7d692",
                        high = "#e33636",
                        limits = c(0, 100),
                        guide = guide_colorbar(ticks.colour = "black",
                                               frame.colour = "black")) +
  geom_text(inherit.aes = FALSE,
            data = monthPositions,
            aes(x = x,
                y = y,
                label = month),
            family = plotText,
            fontface = "bold",
            size = 5) +
  facet_wrap(. ~ village) +
  labs(title = "SAFI Survey",
       subtitle = "Results of a 2016-2017 Studying African Farmer-Led Irrigation Survey where 131 households across \nthree villages reported which months they experienced food insecurity in the previous year.",
       caption = "#TidyTuesday: 2023, Week 24 | Data: SAFI | GitHub: jedjohnson4 | Twitter: jed4johnson",
       fill = "% of Respondents") +
  theme(
    plot.background = element_rect(fill = "beige",
                                   color = NA),
    plot.title = element_text(family = plotText,
                              size = 22,
                              face = "bold",
                              vjust = 0.5),
    plot.subtitle = element_text(family = plotText,
                                 size = 12),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA,
                                color = "black",
                                size = 2),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "#eb4a44",
                                    color = "black",
                                    linewidth = 2),
    strip.text = element_text(family = plotText,
                              size = 18,
                              face = "bold",
                              color = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(family = plotText,
                                color = "black",
                                size = 12,
                                vjust = 0.81),
    legend.text = element_text(family = plotText,
                               vjust = 2),
    legend.title.align = 0.5,
    plot.caption = element_text(hjust = 0.5,
                                family = plotText),
    plot.margin = unit(c(10, 5, 5, 5), "pt")
  )

ggsave(path = "plots",
       filename = "SAFI.png",
       width = 9,
       height = 5,
       units = "in",
       dpi = 500)
