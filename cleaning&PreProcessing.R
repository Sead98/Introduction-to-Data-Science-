
library(WDI)
library(tidyverse)
library(readr)
library(readxl)

install.packages("plotly")
install.packages("janitor")
library(janitor)
library(plotly)

#Loading ds
goalscorers <- read.csv("D:/International Football datset/goalscorers.csv")
results <- read.csv("D:/International Football datset/results.csv")
shootout <- read.csv("D:/International Football datset/shootouts.csv")

show(goalscorers)

#	Check structure:
str(goalscorers)
str(results) 
str(shootout) 

#	Check summary: 
summary(goalscorers)
#	Preview data:
head(goalscorers)

#Structured the column names for consistency
goalscorers <- clean_names(goalscorers)
results <- clean_names(results)
shootout <- clean_names(shootout)

show(goalscorers)  

#Checking for missing values

colSums(is.na(goalscorers))
colSums(is.na(results))
colSums(is.na(shootout))



# Replace NA with "Unknown" for scorer column in goalscorers
goalscorers$scorer[is.na(goalscorers$scorer)] <- "Unknown" 

# Replace NA with "-1" for scorer minute in goalscorers
goalscorers$minute[is.na(goalscorers$minute)] <- -1


show(shootout)

# Replace empty strings with "unknown" in a first shooter column of shootout
shootout$first_shooter[shootout$first_shooter == ""] <- "unknown"



show(goalscorers)


#package for working with dates
install.packages("lubridate")
library(lubridate)

colnames(shootout)
# converting date format to yyyy-mm-dd for results$date
results <- results %>%
  mutate(date = parse_date_time(date, orders = c("ymd", "dmy")),
         date = as.Date(date)) 

show(results$date)
str(results)

#Converting date format to yyyy-mm-dd for shootout
str(shootout)
shootout$date <- as.Date(shootout$date, format = "%d-%m-%Y")
head(shootout)
show(shootout$date)


#Converting date format to yyyy-mm-dd for goalscorer
str(goalscorers)
goalscorers$date <- as.Date(goalscorers$date, format = "%d-%m-%Y")
head(goalscorers)
show(goalscorers$date)

library(dplyr)
#adding a new column century in results which specifies the century of the game
str(results)
results <- results %>%
  mutate(century = case_when(
    year(date) < 1901 ~ "19th",
    year(date) >= 1901 & year(date) < 2001 ~ "20th",
    year(date) >= 2001 ~ "21st"
  ))
head(results)

#Adding goal difference column in results to indicate gd for the match
results <- results %>%
  mutate(goal_difference = abs(home_score - away_score))

# Group by gd to compute avg 
  avg_goal_diff <- results %>%
    group_by(century) %>%
    summarize(avg_goal_difference = mean(goal_difference, na.rm = TRUE))
  print(avg_goal_diff)
  
head(goalscorers)
head(results)

#Analyse goal scoring pattern
  goals_by_era <- goalscorers %>%
    inner_join(results %>% select(date, century), by = "date") %>%
    group_by(century) %>%
    summarize(
      total_goals = n(),
      own_goals = sum(own_goal, na.rm = TRUE),
      penalties = sum(penalty, na.rm = TRUE))
    

  print(goals_by_era)  
  