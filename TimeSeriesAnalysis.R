#time series analysis

#Packages

install.packages("forecast")
install.packages("tseries")

library(forecast)
library(tseries)

head(results)

#preparing required data and adding columns for time series analysis
results <- results %>%
  mutate(Goal_Difference = home_score - away_score) %>%
  mutate(
    team_result = case_when(
      home_team == "England" ~ Goal_Difference,        # England at home
      away_team == "England" ~ -Goal_Difference,       # England away
      TRUE ~ NA_real_                               # England doesn't play
    ),
    opponent = case_when(
      home_team == "England" ~ away_team,            # England at home
      away_team == "England" ~ home_team,            # England away
      TRUE ~ NA_character_                           # England doesn't play
    ),
    home_away = case_when(
      home_team == "England" ~ "Home",                # England at home
      away_team == "England" ~ "Away",                # England away
      TRUE ~ NA_character_                           # England doesn't play
    )
  )%>%
  select(-goal_difference)

# Create a sequence of all dates in the dataset
all_dates <- data.frame(date = seq(min(results$date, na.rm = TRUE), max(results$date, na.rm = TRUE), by = "day"))

# Left join to include all dates and handle missing matches
team_results <- left_join(all_dates, results, by = "date")

#Filter out NA values if you need a dataset with only the matches where the team played
england_only <- team_results %>% filter(!is.na(team_result))

head(results)
show(results)


#time series steps

#Step 1. Aggregate the data on yearly basis
# Group by year and calculate mean goal difference (excluding NA values)

team_results_yearly <- team_results %>%
  mutate(year = year(date)) %>%  # Extract year from date
  group_by(year) %>%
  summarise(mean_goal_difference = mean(team_result, na.rm = TRUE), .groups = "drop")

View(team_results_yearly)

#Step2 Creating the time series object
# Create time series object with yearly frequency

england_ts <- ts(team_results_yearly$mean_goal_difference, start = min(team_results_yearly$year), frequency = 1)
print(england_ts)

# Step3 Visualization of the mean gd of England team across the 3 centuries
autoplot(england_ts) +
  ggtitle("England's Mean Goal Difference Over Time") +
  xlab("Year") +
  ylab("Mean Goal Difference")

#Step4 analysis of the team's mean gd by century

# Group by century and calculate mean goal difference
team_results_by_century <- team_results %>%
  mutate(century = (year(date) - 1) %/% 100 + 1) %>%  #To  Calculate century
  group_by(century) %>%
  summarise(mean_goal_difference = mean(team_result, na.rm = TRUE), .groups = "drop")

# Print the data to see mean goal difference by century
print(team_results_by_century)


# Plot mean goal difference by century
ggplot(team_results_by_century, aes(x = century, y = mean_goal_difference)) +
  geom_bar(stat = "identity") +
  ggtitle("Mean Goal Difference by Century") +
  xlab("Century") +
  ylab("Mean Goal Difference")


#Step5 Analysis of England's home vs away performances across the 3 centuries

# Analysis by Home vs. Away across Centuries 
team_results_by_home_away <- team_results %>%
  mutate(century = (year(date) - 1) %/% 100 + 1) %>%
  filter(!is.na(home_away)) %>%  #Remove NA values from home_away
  group_by(century, home_away) %>%
  summarise(mean_goal_difference = mean(team_result, na.rm = TRUE), .groups = "drop")

# Print the data
print(team_results_by_home_away)

# Plot using ggplot2 (Bar Chart - Dodged)
ggplot(team_results_by_home_away, aes(x = factor(century), y = mean_goal_difference, fill = home_away)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Mean Goal Difference by Century and Home/Away") +
  xlab("Century") +
  ylab("Mean Goal Difference") +
  labs(fill = "Home/Away") +
  theme_bw() 

# Plot using ggplot2 (Line Chart)
ggplot(team_results_by_home_away, aes(x = factor(century), y = mean_goal_difference, color = home_away, group = home_away)) +
  geom_line() +
  geom_point() +
  ggtitle("Mean Goal Difference by Century and Home/Away (Line Chart)") +
  xlab("Century") +
  ylab("Mean Goal Difference") +
  labs(color = "Home/Away") +
  theme_bw() 



