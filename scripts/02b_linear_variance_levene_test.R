# PURPOSE: Run Levene's tests to assess whether treatments differ in variance
#
# AUTHOR: TAS
# DATE: JUNE 12
#
################################################################################

# Load packages
library(car)
library(readr)

# Load data
data <- read_csv("data_cleaned/fin_linear_measurements_SL_standardized.csv")
data$Treatment <- as.factor(trimws(data$Treatment))  # Clean up any whitespace

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
    sub_data <- droplevels(subset(data, Treatment %in% c(group1, group2)))
    
    # Drop rows with NA for the current variable
    sub_data <- sub_data[!is.na(sub_data[[var_name]]), ]
    
    # Check sample sizes
    group_counts <- table(sub_data$Treatment)
    
    # Proceed only if both groups are present and have at least 2 values
    if (all(c(group1, group2) %in% names(group_counts)) &&
        all(group_counts[c(group1, group2)] >= 2)) {
      
      test_result <- leveneTest(sub_data[[var_name]] ~ sub_data$Treatment)
      
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
write_csv(levene_results, "output_files/table_S_satanoperca_levene_test_linear.csv")
