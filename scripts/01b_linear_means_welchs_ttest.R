# PURPOSE: Run Welch's t-tests to assess whether treatments differ in mean values
# -Script runs a runs a loop for all linear measurements on radials and rays
# -Output are saved to a CSV file.
#
# AUTHOR: TAS
# DATE: JUNE 12
################################################################################

# Load data ####
data = read.csv(file = "data_cleaned/fin_linear_measurements_SL_standardized.csv")
data$Treatment <- as.factor(data$Treatment)

# Variables to test
ray_vars <- c(
  "Radial_1_Length", "Radial_2_Length", "Radial_3_Length", "Radial_4_Length", 
  "Radial_1_Width", "Radial_2_Width", "Radial_3_Width", "Radial_4_Width",
  "Marginal_Ray_3_Measurement_1", "Marginal_Ray_3_Measurement_2", "Marginal_Ray_3_Measurement_3", "Marginal_Ray_3_Measurement_4",
  "Marginal_Ray_4_Measurement_1", "Marginal_Ray_4_Measurement_2", "Marginal_Ray_4_Measurement_3", "Marginal_Ray_4_Measurement_4",
  "Marginal_Ray_5_Measurement_1", "Marginal_Ray_5_Measurement_2", "Marginal_Ray_5_Measurement_3", "Marginal_Ray_5_Measurement_4"
)

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
    vals1 <- na.omit(data[[var_name]][data$Treatment == group1])
    vals2 <- na.omit(data[[var_name]][data$Treatment == group2])
    
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
  "output_files/table_S_satanoperca_welch_ttests_linear.csv",
  row.names = FALSE
)
