# PURPOSE: Run Welch's t-tests to assess whether treatments differ in mean values
#
# AUTHOR: TAS
# DATE: JUNE 9
################################################################################

# Load packages ####

# load data ####
data = read.csv(file = "data_cleaned/fin_CT_measurements_hemitrichia.csv")
data$treatment <- as.factor(data$treatment)

# calculate ratios
data_wide <- data %>%
  pivot_wider(
    id_cols = c(individual_ID, treatment),
    names_from = hemitrichia_side,
    values_from = c(CSA, Imax)
  )

# Calculate dorsal/ventral ratios
data_ratios <- data_wide %>%
  mutate(
    CSA_ratio = CSA_dorsal / CSA_ventral,
    Imax_ratio = Imax_dorsal / Imax_ventral
  )

# View result
print(data_ratios)

# Variables to test
ray_vars <- c("CSA_ratio", "Imax_ratio")

# Treatment pairs to compare
treatment_pairs <- list(
  c("Rock", "Pelagic"),
  c("Rock", "Sand"),
  c("Sand", "Pelagic")
)

# Initialize results dataframe
results <- data.frame(
  Variable = character(),
  Group1 = character(),
  Group2 = character(),
  Welch_t = numeric(),
  df = numeric(),
  P_value_two_sided = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and treatment pair
for (var_name in ray_vars) {
  for (pair in treatment_pairs) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    # Extract non-missing values for each group
    vals1 <- na.omit(data_ratios[[var_name]][data_ratios$treatment == group1])
    vals2 <- na.omit(data_ratios[[var_name]][data_ratios$treatment == group2])
    
    # Skip if either group has fewer than 2 values
    if (length(vals1) < 2 | length(vals2) < 2) next
    
    # Run Welch's t-test
    t_test <- t.test(vals1, vals2, var.equal = FALSE)
    
    # Store results
    results <- rbind(results, data.frame(
      Variable = var_name,
      Group1 = group1,
      Group2 = group2,
      Welch_t = t_test$statistic,
      df = t_test$parameter,
      P_value_two_sided = t_test$p.value
    ))
  }
}

# View summary
print(results)

# Save results
write.csv(
  results,
  "output_files/table_S_satanoperca_welch_ttests_DV_asymmetry.csv",
  row.names = FALSE
)
