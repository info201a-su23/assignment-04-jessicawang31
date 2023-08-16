library(dplyr)
library(stringr)
library(usmap)
library(ggplot2)

data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")

filtered_data <- data %>%
  filter(year == 2000) 

state_total_prison_pop <- filtered_data %>%
  group_by(state) %>%
  summarize(total_prison_pop_2000 = sum(total_prison_pop, na.rm = TRUE))

plot_usmap(data = state_total_prison_pop, values = "total_prison_pop_2000", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "purple", name = "Total Population", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Total Prison Population in the U.S. in 2000")
