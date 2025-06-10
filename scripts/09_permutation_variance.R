# PURPOSE: Create a permutation test of whether the CSA of fin rays differ in mean values
#
# AURTHOR: TAS
# DATE: JUNE 10
################################################################################

# load packages ####
library(permute)


# load data ####
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

# Number of permutations
n_perm <- 10000

# Initialize results dataframe
var_ratio_results <- data.frame(
  Variable = character(),
  Group1 = character(),
  Group2 = character(),
  Observed_Var_Ratio = numeric(),
  P_value_two_sided = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and treatment pair
for (var_name in ray_vars) {
  for (pair in treatment_pairs) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    # Extract data for each group, handle missing values
    vals1 <- na.omit(data[[var_name]][data$treatment == group1])
    vals2 <- na.omit(data[[var_name]][data$treatment == group2])
    
    # Skip comparison if either group has fewer than 2 values
    if (length(vals1) < 2 | length(vals2) < 2) next
    
    # Observed variance ratio
    obs_ratio <- var(vals1) / var(vals2)
    
    # Create combined vector and group labels
    combined_vals <- c(vals1, vals2)
    combined_groups <- c(rep(group1, length(vals1)), rep(group2, length(vals2)))
    
    # Permutation loop
    perm_ratios <- numeric(n_perm)
    for (i in 1:n_perm) {
      perm_labels <- sample(combined_groups)
      perm_vals1 <- combined_vals[perm_labels == group1]
      perm_vals2 <- combined_vals[perm_labels == group2]
      
      # Only calculate if both groups have at least 2 values
      if (length(perm_vals1) >= 2 && length(perm_vals2) >= 2) {
        perm_ratios[i] <- var(perm_vals1) / var(perm_vals2)
      } else {
        perm_ratios[i] <- NA
      }
    }
    
    # Two-sided p-value: test if permuted ratio is as or more extreme
    pval <- mean(
      perm_ratios >= obs_ratio | perm_ratios <= 1 / obs_ratio,
      na.rm = TRUE
    )
    
    # Store result
    var_ratio_results <- rbind(var_ratio_results, data.frame(
      Variable = var_name,
      Group1 = group1,
      Group2 = group2,
      Observed_Var_Ratio = obs_ratio,
      P_value_two_sided = pval
    ))
  }
}

# View final results
print(var_ratio_results)

# Save results
write.csv(
  results,
  "output_files/table_S1_satanoperca_permutation_means_CSA.csv",
  row.names = FALSE
)

