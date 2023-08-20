library(dplyr)

data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")

# My variable: black_prison_pop

# Which county has the greatest average for my variable? 

black_average_population <- data %>%
  group_by(county_name) %>%
  summarize(total_black_average_population = mean(black_prison_pop, na.rm = TRUE)) %>%
  filter(!is.nan(total_black_average_population))

max_black_average_population <- max(black_average_population$total_black_average_population)
max_black_average_population_row <- black_average_population %>%
  filter(total_black_average_population == max_black_average_population)

print(max_black_average_population_row)

# Where is my variable the highest?
  
max_value <- max(data$black_prison_pop, na.rm = TRUE)
highest_incarceration_county <- data %>%
  filter(black_prison_pop == max_value)
highest_incarceration_county_name = highest_incarceration_county$county_name
print(highest_incarceration_county_name)

# Where is my variable the lowest?

lowest_incarceration_county <- data %>%
  arrange(black_prison_pop) %>%
  head(1)
lowest_incarceration_county_name = lowest_incarceration_county$county_name
print(lowest_incarceration_county_name)

# How much has my variable change for the male population over the last N = 10 years in King County, WA?

current_year <- max(data$year)
my_county <- "King County"
my_state <- "WA"
last_ten_years <- current_year - 10

filtered_data <- data %>% 
  filter(state == my_state & county_name == my_county) %>% 
  filter(year >= last_ten_years & year <= current_year)

non_na_values <- sum(!is.na(filtered_data$black_male_prison_pop))

if (non_na_values > 0) {
  max_value <- max(filtered_data$black_male_prison_pop, na.rm = TRUE)
  min_value <- min(filtered_data$black_male_prison_pop, na.rm = TRUE)
  male_change_over_years <- max_value - min_value
  print(male_change_over_years)
}

# How much has my variable change for the female population over the last N = 10 years in King County, WA?

current_year <- max(data$year)
my_county <- "King County"
my_state <- "WA"
last_ten_years <- current_year - 10

filtered_data <- data %>% 
  filter(state == my_state & county_name == my_county) %>% 
  filter(year >= last_ten_years & year <= current_year)

non_na_values <- sum(!is.na(filtered_data$black_female_prison_pop))

if (non_na_values > 0) {
  max_value <- max(filtered_data$black_female_prison_pop, na.rm = TRUE)
  min_value <- min(filtered_data$black_female_prison_pop, na.rm = TRUE)
  female_change_over_years <- max_value - min_value
  print(female_change_over_years)
}