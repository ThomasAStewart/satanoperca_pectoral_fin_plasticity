# PURPOSE: Run F-tests to assess whether treatments differ in variance
#
# AUTHOR: TAS
# DATE: JUNE 9
#
################################################################################

# Load data
data = read.csv(file = "data_cleaned/fin_linear_measurements_SL_standardized.csv")
data$Treatment <- as.factor(data$Treatment)

# Variable list
ray_vars <- c(
  "Radial_1_Length", "Radial_2_Length", "Radial_3_Length", "Radial_4_Length", 
  "Radial_1_Width", "Radial_2_Width", "Radial_3_Width", "Radial_4_Width",
  "Marginal_Ray_3_Measurement_1", "Marginal_Ray_3_Measurement_2", "Marginal_Ray_3_Measurement_3", "Marginal_Ray_3_Measurement_4",
  "Marginal_Ray_4_Measurement_1", "Marginal_Ray_4_Measurement_2", "Marginal_Ray_4_Measurement_3", "Marginal_Ray_4_Measurement_4",
  "Marginal_Ray_5_Measurement_1", "Marginal_Ray_5_Measurement_2", "Marginal_Ray_5_Measurement_3", "Marginal_Ray_5_Measurement_4"
)

# Treatment group pairs
treatment_pairs <- list(
  c("Rock", "Pelagic"),
  c("Rock", "Sand"),
  c("Sand", "Pelagic")
)

# Initialize results dataframe
f_test_results <- data.frame(
  Variable = character(),
  Group1 = character(),
  Group2 = character(),
  Observed_Var_Ratio = numeric(),
  F_statistic = numeric(),
  df1 = numeric(),
  df2 = numeric(),
  P_value_two_sided = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and treatment pair
for (var_name in ray_vars) {
  for (pair in treatment_pairs) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    # Extract data for each group, remove NAs
    vals1 <- na.omit(data[[var_name]][data$Treatment == group1])
    vals2 <- na.omit(data[[var_name]][data$Treatment == group2])
    
    # Skip if either group has fewer than 2 values
    if (length(vals1) < 2 | length(vals2) < 2) next
    
    # Run F-test
    ftest <- var.test(vals1, vals2)
    
    # Store result
    f_test_results <- rbind(f_test_results, data.frame(
      Variable = var_name,
      Group1 = group1,
      Group2 = group2,
      Observed_Var_Ratio = var(vals1) / var(vals2),
      F_statistic = ftest$statistic,
      df1 = ftest$parameter[1],
      df2 = ftest$parameter[2],
      P_value_two_sided = ftest$p.value
    ))
  }
}

# View results
print(f_test_results)

# Save results
write.csv(
  f_test_results,
  "output_files/table_S4satanoperca_f_test_var_linear.csv",
  row.names = FALSE
)
