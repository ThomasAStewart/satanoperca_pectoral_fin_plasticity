# PURPOSE: Run Welch's t-tests to assess whether treatments differ in mean values
#
# AUTHOR: TAS
# DATE: JUNE 9
################################################################################

# Load packages ####

# Load data ####
data = read.csv(file = "data_cleaned/fin_CT_measurements_hemitrichia_top_bottom_sum.csv")
data$treatment <- as.factor(data$treatment)

# Variables to test
ray_vars <- c("csa_standardized")

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
    vals1 <- na.omit(data[[var_name]][data$treatment == group1])
    vals2 <- na.omit(data[[var_name]][data$treatment == group2])
    
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

results$P_value_BH <- ave(results$P_value_two_sided, results$Variable, FUN = function(p) p.adjust(p, method = "BH"))

# Save results
write.csv(
  results,
  "output_files/table_S_satanoperca_welch_ttests_CSA.csv",
  row.names = FALSE
)
