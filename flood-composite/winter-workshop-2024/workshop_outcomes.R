# Winter Workshop 2024 Outcomes 

library(tidyverse)
library(googlesheets4)

url_sheet <- "https://docs.google.com/spreadsheets/d/1aBUJE_79caUIA7BkSd1-rNk5Zuk0TDOkL8thafTC4Qo/edit?usp=sharing"
outcomes <- read_sheet(url_sheet, sheet = "Survey Summary")

plot <- ggplot(outcomes, aes(x = Count, y = reorder(Topic, Count))) +
  geom_bar(stat = "identity", fill = "#3aadfc") +
  scale_y_discrete(name = "") +
  scale_x_continuous(name = "Number of Responses") +
  theme_minimal() +
  labs(title = "Workshop Station 3: Results of Survey on Population Topic Priorities for the ESVA Climate Equity Atlas") +
  theme(
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_line(linewidth = 0.35),
    plot.title = element_text(hjust = 1.3)
    # panel.grid.major.y = element_blank(),
    # panel.grid.minor = element_blank(),
        )

plot

ggsave(plot = plot, filename = "station3_outcomes.png", bg = "white")
