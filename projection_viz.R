library(tidyverse)
library(baseballr)
library(gt)
library(gtExtras)
library(mlbplotR)
library(readxl)
library(ggplot2)

df <- read_excel("C:/Users/student/Downloads/2024_hr_proj.xlsx")

ggplot(df) +
  geom_segment(aes(x = reorder(Hitter, Projected_HR), 
                   xend = reorder(Hitter, Projected_HR), 
                   y = Prop_Line, 
                   yend = Projected_HR), 
               color = "black") +
  geom_point(data = df, aes(x = reorder(Hitter, Prop_Line), 
                            y = Prop_Line, 
                            color = "Prop_Line"), 
             size = 3) +
  geom_point(data = df, aes(x = reorder(Hitter, Projected_HR), 
                            y = Projected_HR, 
                            color = ifelse(Projected_HR > Prop_Line, "Above Projection", "Below Projection")), 
             size = 3) +
  coord_flip() +
  scale_y_continuous(breaks = seq(15, max(df$Projected_HR), by = 5)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line(size = 0.5),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.ticks.x = element_line(color = "black", size = 0.5),
    panel.grid.major.y = element_line(color = "grey", size = 0.5),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "MLB 2024 Season Projected Homeruns",
    subtitle = "For Available Home Run Player Props on Fanduel",
    caption = "Calculated using the Marcel Projection System by Tom Tango | @Riordan5J"
  ) +
  xlab("Hitter") +
  ylab("2024 Season Projected Homeruns") +
  scale_color_manual(values = c("Prop_Line" = "black", "Above Projection" = "green", "Below Projection" = "red"),
                     labels = c("Prop_Line" = "FanDuel Line", "Above Projection" = "Above Projected HRs", "Below Projection" = "Below Projected HRs"),
                     name = "Legend")

