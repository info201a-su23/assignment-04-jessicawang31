library(ggplot2)
library(dplyr)
library(tidyr)

filtered_data <- data %>% 
  filter(year == min(data$year, na.rm = TRUE) | year == max(data$year, na.rm = TRUE)) %>%
  replace_na(list(total_prison_pop = 0, black_prison_pop = 0))

filtered_data_min <- filtered_data %>% 
  filter(year == min(filtered_data$year, na.rm = TRUE)) 

filtered_data_max <- filtered_data %>% 
  filter(year == max(filtered_data$year, na.rm = TRUE))

black_prison_comp_df <- data.frame(
  year = factor(filtered_data$year),
  population_type = c("Total Population", "Total Population", "Black Population", "Black Population"),
  population_amount = c(filtered_data_min$total_prison_pop,
                        filtered_data_min$black_prison_pop,
                        filtered_data_max$total_prison_pop,
                        filtered_data_max$black_prison_pop)
)

black_prison_comp_dbar_graph <- ggplot(black_prison_comp_df, aes(x = year, y = population_amount, fill = population_type)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Total Population vs. Black Population in Prison in 1970 vs. 2018", 
       x = "Year", 
       y = "Population Amount", 
       fill = "Population Type") +
  scale_fill_manual(values = c("purple", "yellow"),
                    labels = c("Total Population", "Black Population")) +
  theme_bw()
black_prison_comp_dbar_graph