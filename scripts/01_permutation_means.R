# PURPOSE: Create a permutation test of whether the treatments differ in mean values
#
# Notes: To do-- compare and discuss with Michelle about approach. Specificaly, it differs in accounting for SE when comparing means. 
#
# AURTHOR: TAS
# DATE: JUNE 3
################################################################################

# load packages ####
library(permute)

# load data ####
setwd("Documents/papers/to_submit/satanoperca/satanoperca_project/")

data = read.csv(file = "data/data_cleaned/Fin to SL ratios.csv")
data$Treatment <- as.factor(data$Treatment)

# Variables to test
ray_vars <- c("Radial_1_Length", "Radial_2_Length", "Radial_3_Length", "Radial_4_Length", 
              "Radial_1_Width", "Radial_2_Width", "Radial_3_Width", "Radial_4_Width",
              "Marginal_Ray_3_Measurement_1", "Marginal_Ray_3_Measurement_2", "Marginal_Ray_3_Measurement_3", "Marginal_Ray_3_Measurement_4",
              "Marginal_Ray_4_Measurement_1", "Marginal_Ray_4_Measurement_2", "Marginal_Ray_4_Measurement_3", "Marginal_Ray_4_Measurement_4",
              "Marginal_Ray_5_Measurement_1", "Marginal_Ray_5_Measurement_2", "Marginal_Ray_5_Measurement_3", "Marginal_Ray_5_Measurement_4")

# Treatment pairs to compare
treatment_pairs <- list(
  c("Rock", "Pelagic"),
  c("Rock", "Sand"),
  c("Sand", "Pelagic")
)

# Number of permutations
n_perm <- 10000

# Define test statistic function
compute_stat <- function(x, g, group1, group2) {
  vals1 <- x[g == group1]
  vals2 <- x[g == group2]
  
  mean_diff <- mean(vals1) - mean(vals2)
  se <- sqrt(var(vals1)/length(vals1) + var(vals2)/length(vals2))
  t_stat <- mean_diff / se
  return(t_stat)
}

# Initialize results dataframe
results <- data.frame(
  Variable = character(),
  Group1 = character(),
  Group2 = character(),
  Observed_t = numeric(),
  P_value_two_sided = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and treatment pair
for (var_name in ray_vars) {
  for (pair in treatment_pairs) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    # Six groups, e.g., sand dorsal rock ventral
    
    # Extract non-missing values for each group
    vals1 <- as.vector(na.omit(data[[var_name]][data$Treatment == group1]))
    vals2 <- as.vector(na.omit(data[[var_name]][data$Treatment == group2]))
    
    # Skip if one of the groups has <2 values
    if (length(vals1) < 2 | length(vals2) < 2) {
      next
    }
    
    # Combine into single vector and label vector
    values <- c(vals1, vals2)
    groups <- c(rep(group1, length(vals1)), rep(group2, length(vals2)))
    
    # Compute observed statistic
    obs_stat <- compute_stat(values, groups, group1, group2)
    
    # Permutation
    perm_stats <- replicate(n_perm, {
      shuffled_groups <- sample(groups)
      compute_stat(values, shuffled_groups, group1, group2)
    })
    
    # Two-sided p-value
    p_value <- mean(abs(perm_stats) >= abs(obs_stat))
    
    # Store results
    results <- rbind(results, data.frame(
      Variable = var_name,
      Group1 = group1,
      Group2 = group2,
      Observed_t = obs_stat,
      P_value_two_sided = p_value
    ))
  }
}

# View summary
print(results)

# Save results
write.csv(
  results,
  "/Users/tomstewart/Documents/papers/to_submit/satanoperca/satanoperca_project/output_files/satanoperca_permutation_means.csv",
  row.names = FALSE
)

