#Two way Anova test

head(team_results)

# Ensure tournament and century are factors in determining relation
team_results$tournament <- as.factor(team_results$tournament)
team_results$century <- as.factor(team_results$century)

# Remove rows with NA values in team_result and complete combinations
team_results_complete <- team_results %>%
  filter(!is.na(team_result)) %>%
  group_by(tournament, century) %>%
  filter(n() > 0) %>% # Keep only combinations with at least 1 observation
  ungroup()

# Two-Way ANOVA (using complete data)
model <- aov(team_result ~ tournament * century, data = team_results_complete)

summary(model)

#Check the assumptions of ANOVA
plot(model)

# Post-hoc Tests (TukeyHSD)
tukey_results <- TukeyHSD(model)

# Print the Tukey results
print(tukey_results)

