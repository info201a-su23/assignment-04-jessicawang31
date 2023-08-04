library(ggplot2)
library(dplyr)

current_year <- max(data$year)
my_county <- "King County"
my_state <- "WA"
last_ten_years <- current_year - 10

filtered_data <- data %>%
  filter(state == my_state & county_name == my_county) %>%
  filter(year >= last_ten_years & year <= current_year)

black_prison_trend_df <- data.frame(
  years = filtered_data$year, 
  male_prison_pop = filtered_data$black_male_prison_pop, 
  female_prison_pop = filtered_data$black_female_prison_pop
)

black_prison_trend_scatterplot <- ggplot(black_prison_trend_df, aes(x = years)) +
  geom_point(aes(y = male_prison_pop), na.rm = TRUE) +
  geom_smooth(aes(y = male_prison_pop, color = "Male"), method = lm, se = FALSE, na.rm = TRUE) +
  geom_point(aes(y = female_prison_pop), na.rm = TRUE) +
  geom_smooth(aes(y = female_prison_pop, color = "Female"), method = lm, se = FALSE, linetype = "dashed", na.rm = TRUE) +
  labs(title = "Trend of Black Males vs. Black Females in Prison in King County, 
                                    WA the last 10 Years", 
       x = "Years", 
       y = "Prison Population", 
       color = "Gender") +
  scale_color_manual(values = c("Male" = "yellow", "Female" = "purple")) +
  theme_bw()
black_prison_trend_scatterplot