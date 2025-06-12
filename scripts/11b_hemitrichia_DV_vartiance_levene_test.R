# PURPOSE: Run Levene's tests to assess whether treatments differ in variance
#
# AUTHOR: TAS
# DATE: JUNE 12
#
################################################################################


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

# Variables to compare
ray_vars <- c("CSA_ratio", "Imax_ratio")


# Treatment pairs to compare
treatment_pairs <- list(
  c("Rock", "Pelagic"),
  c("Rock", "Sand"),
  c("Sand", "Pelagic")
)


# Initialize results dataframe
levene_results <- data.frame(
  Variable = character(),
  Group1 = character(),
  Group2 = character(),
  Levene_F = numeric(),
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
    
    # Subset to relevant treatment pair and drop unused levels
    sub_data <- droplevels(subset(data_ratios, treatment %in% c(group1, group2)))
    
    # Drop rows with NA for the current variable
    sub_data <- sub_data[!is.na(sub_data[[var_name]]), ]
    
    # Check sample sizes
    group_counts <- table(sub_data$treatment)
    
    # Proceed only if both groups are present and have at least 2 values
    if (all(c(group1, group2) %in% names(group_counts)) &&
        all(group_counts[c(group1, group2)] >= 2)) {
      
      test_result <- leveneTest(sub_data[[var_name]] ~ sub_data$treatment)
      
      levene_results <- rbind(levene_results, data.frame(
        Variable = var_name,
        Group1 = group1,
        Group2 = group2,
        Levene_F = test_result$`F value`[1],
        df1 = test_result$Df[1],
        df2 = test_result$Df[2],
        P_value_two_sided = test_result$`Pr(>F)`[1]
      ))
      
    } else {
      warning(paste("Skipping", var_name, "for", group1, "vs", group2, ": insufficient non-NA values"))
    }
  }
}

# View results
print(levene_results)

# Save results
write_csv(levene_results, "output_files/table_S_satanoperca_levene_test_hemitrichia_.csv")
