# PURPOSE: Create a permutation test of whether the treatments differ in mean variance
# -Script runs a runs a loop for all linear measurements on radials and rays
# -Output are saved to a CSV file.
#
# AUTHOR: TAS
# DATE: JUNE 12
################################################################################

# Load packages
library(permute)

# Load data
data = read.csv(file = "data_cleaned/fin_linear_measurements_SL_standardized.csv")
data$Treatment <- as.factor(data$Treatment)

# Variables to test
ray_vars <- c("Radial_1_Length", "Radial_2_Length", "Radial_3_Length", "Radial_4_Length", 
              "Radial_1_Width", "Radial_2_Width", "Radial_3_Width", "Radial_4_Width")

# Treatment group pairs
treatment_pairs <- list(
  c("Rock", "Pelagic"),
  c("Rock", "Sand"),
  c("Sand", "Pelagic")
)

# Set number of permutations
nperm <- 10000

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
    vals1 <- na.omit(data[[var_name]][data$Treatment == group1])
    vals2 <- na.omit(data[[var_name]][data$Treatment == group2])
    
    # Skip comparison if either group has fewer than 2 values
    if (length(vals1) < 2 | length(vals2) < 2) next
    
    # Observed variance ratio
    obs_ratio <- var(vals1) / var(vals2)
    
    # Create combined vector and group labels
    combined_vals <- c(vals1, vals2)
    combined_groups <- c(rep(group1, length(vals1)), rep(group2, length(vals2)))
    
    # Permutation loop
    perm_ratios <- numeric(nperm)
    for (i in 1:nperm) {
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

var_ratio_results$P_value_BH <- ave(var_ratio_results$P_value_two_sided, var_ratio_results$Variable, FUN = function(p) p.adjust(p, method = "BH"))


# Save results
write.csv(
  var_ratio_results,
  "output_files/table_S_satanoperca_permutation_var_linear.csv",
  row.names = FALSE
)
