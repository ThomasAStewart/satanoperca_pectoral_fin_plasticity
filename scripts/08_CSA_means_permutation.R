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
    
    # Extract non-missing values for each group
    vals1 <- as.vector(na.omit(data[[var_name]][data$treatment == group1]))
    vals2 <- as.vector(na.omit(data[[var_name]][data$treatment == group2]))
    
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
  "output_files/table_S1_satanoperca_permutation_means_CSA.csv",
  row.names = FALSE
)

