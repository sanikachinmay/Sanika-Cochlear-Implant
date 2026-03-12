##mann-whitney test##
per_patient_avg <- test2%>%
  group_by(mrn,var_name,hearing_age_group) %>%
  summarise(
    Score1 = mean(receptive_ae, na.rm = TRUE),
    Score2 = mean(expressive_ae, na.rm = TRUE),
    .groups = "drop"
  )


wilcox_results <- per_patient_avg %>%
  group_split(hearing_age_group) %>%
  map(~ wilcox.test(Score1 ~ var_name, data = .x))

wilcox_results <- per_patient_avg %>%
  group_split(hearing_age_group) %>%
  map(~ wilcox.test(Score2 ~ var_name, data = .x))

# Print results
wilcox_results

##kruskal-wallis test##
table(test2$first_implant_age_categorical)
per_patient_avg <- test2%>%
  group_by(mrn,var_name,hearing_age_group) %>%
  summarise(
    Score1 = mean(receptive_ae, na.rm = TRUE),
    Score2 = mean(expressive_ae, na.rm = TRUE),
    .groups = "drop"
  )

kruskal_results <- per_patient_avg %>%
  group_split(hearing_age_group) %>%
  map(~ kruskal.test(Score1 ~ var_name, data = .x))

kruskal_results <- per_patient_avg %>%
  group_split(hearing_age_group) %>%
  map(~ kruskal.test(Score2 ~ var_name, data = .x))

# Print results
kruskal_results