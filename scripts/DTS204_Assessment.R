# DTS 204 LAB ASSESSMENT
# Statistical Computing, Inference and Modelling
# Dataset: Palmer Penguins
#
# PROJECT STRUCTURE:
#
# DTS204_Assessment/
# ├── data/
# │   └── DTS_204_Dataset.xlsx
# ├── outputs/
# │   ├── figures/
# │   └── reports/
# └── scripts/
#     └── DTS204_Assessment.R


# 1. INSTALL AND LOAD REQUIRED PACKAGES

required_packages <- c(
  "readxl",
  "dplyr",
  "ggplot2",
  "corrplot"
)

missing_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)


# 2. CREATE OUTPUT FOLDERS

if (!dir.exists("../outputs")) {
  dir.create("../outputs")
}

if (!dir.exists("../outputs/figures")) {
  dir.create("../outputs/figures")
}

if (!dir.exists("../outputs/reports")) {
  dir.create("../outputs/reports")
}


# 3. IMPORT THE DATASET

dataset_path <- "../data/DTS_204_Dataset.xlsx"

if (!file.exists(dataset_path)) {
  stop(
    paste(
      "The dataset was not found at:",
      dataset_path,
      "\nMake sure DTS_204_Dataset.xlsx is inside the data folder."
    )
  )
}

penguin_data <- read_excel(dataset_path)

cat("\n================ DATA PREVIEW ================\n")
print(head(penguin_data))

cat("\n================ DATA STRUCTURE ==============\n")
str(penguin_data)

cat("\n================ DATA DIMENSIONS =============\n")
cat("Rows:", nrow(penguin_data), "\n")
cat("Columns:", ncol(penguin_data), "\n")

cat("\n================ COLUMN NAMES ================\n")
print(names(penguin_data))


# 4. CHECK FOR MISSING VALUES

missing_summary <- colSums(is.na(penguin_data))

cat("\n================ MISSING VALUES ===============\n")
print(missing_summary)

cat(
  "Total missing values:",
  sum(missing_summary),
  "\n"
)


# 5. DESCRIPTIVE STATISTICS

measurement_data <- penguin_data %>%
  select(
    bill_length_mm,
    bill_depth_mm,
    flipper_length_mm,
    body_mass_g
  )

summary_table <- data.frame(
  variable = names(measurement_data),
  mean = sapply(
    measurement_data,
    mean,
    na.rm = TRUE
  ),
  median = sapply(
    measurement_data,
    median,
    na.rm = TRUE
  ),
  minimum = sapply(
    measurement_data,
    min,
    na.rm = TRUE
  ),
  maximum = sapply(
    measurement_data,
    max,
    na.rm = TRUE
  ),
  range = sapply(
    measurement_data,
    function(x) {
      max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    }
  ),
  standard_deviation = sapply(
    measurement_data,
    sd,
    na.rm = TRUE
  )
)

summary_table <- summary_table %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

cat("\n================ SUMMARY STATISTICS ===========\n")
print(summary_table)

write.csv(
  summary_table,
  "../outputs/summary_statistics.csv",
  row.names = FALSE
)


# 6. DATA CLEANING

numeric_complete <- penguin_data %>%
  filter(
    !is.na(bill_length_mm),
    !is.na(bill_depth_mm),
    !is.na(flipper_length_mm),
    !is.na(body_mass_g)
  )

sex_complete <- penguin_data %>%
  filter(!is.na(sex))

cat("\n================ CLEANING SUMMARY =============\n")
cat("Original rows:", nrow(penguin_data), "\n")

cat(
  "Rows available for numerical analysis:",
  nrow(numeric_complete),
  "\n"
)

cat(
  "Rows with recorded sex:",
  nrow(sex_complete),
  "\n"
)

cat(
  "Rows removed from numerical analysis:",
  nrow(penguin_data) - nrow(numeric_complete),
  "\n"
)


# 7. TWO-SAMPLE T-TEST
# Compare the mean body masses of Adelie and Gentoo penguins

adelie_gentoo <- penguin_data %>%
  filter(
    species %in% c("Adelie", "Gentoo"),
    !is.na(body_mass_g)
  )

species_mass_summary <- adelie_gentoo %>%
  group_by(species) %>%
  summarise(
    sample_size = n(),
    mean_body_mass = mean(body_mass_g),
    standard_deviation = sd(body_mass_g),
    .groups = "drop"
  ) %>%
  mutate(
    mean_body_mass = round(mean_body_mass, 3),
    standard_deviation = round(standard_deviation, 3)
  )

body_mass_test <- t.test(
  body_mass_g ~ species,
  data = adelie_gentoo,
  alternative = "two.sided",
  conf.level = 0.95
)

cat("\n================ TWO-SAMPLE T-TEST ============\n")

cat(
  "H0: The mean body mass is the same for Adelie and Gentoo penguins.\n"
)

cat(
  "H1: The mean body mass differs between Adelie and Gentoo penguins.\n\n"
)

print(species_mass_summary)
print(body_mass_test)

if (body_mass_test$p.value < 0.05) {
  t_test_decision <- "Reject H0"

  t_test_interpretation <- paste(
    "There is sufficient statistical evidence that",
    "the mean body mass differs between Adelie and Gentoo penguins."
  )
} else {
  t_test_decision <- "Fail to reject H0"

  t_test_interpretation <- paste(
    "There is insufficient statistical evidence that",
    "the mean body mass differs between Adelie and Gentoo penguins."
  )
}

cat("\nDecision:", t_test_decision, "\n")
cat("Interpretation:", t_test_interpretation, "\n")


# 8. ONE-SAMPLE PROPORTION TEST

sex_complete <- sex_complete %>%
  mutate(
    sex = tolower(trimws(as.character(sex)))
  )

number_male <- sum(sex_complete$sex == "male")
number_known_sex <- nrow(sex_complete)

observed_male_proportion <- (
  number_male / number_known_sex
)

male_test <- prop.test(
  x = number_male,
  n = number_known_sex,
  p = 0.50,
  alternative = "two.sided",
  conf.level = 0.95,
  correct = FALSE
)

cat("\n================ PROPORTION TEST ===============\n")
cat("H0: The proportion of male penguins is 0.50.\n")

cat(
  "H1: The proportion of male penguins is not equal to 0.50.\n\n"
)

cat("Male penguins:", number_male, "\n")

cat(
  "Penguins with recorded sex:",
  number_known_sex,
  "\n"
)

cat(
  "Observed male proportion:",
  round(observed_male_proportion, 4),
  "\n"
)

print(male_test)

if (male_test$p.value < 0.05) {
  proportion_decision <- "Reject H0"

  proportion_interpretation <- paste(
    "The proportion of male penguins is",
    "significantly different from 50%."
  )
} else {
  proportion_decision <- "Fail to reject H0"

  proportion_interpretation <- paste(
    "The proportion of male penguins is not",
    "significantly different from 50%."
  )
}

cat("\nDecision:", proportion_decision, "\n")

cat(
  "Interpretation:",
  proportion_interpretation,
  "\n"
)


# 9. CORRELATION ANALYSIS

correlation_values <- numeric_complete %>%
  select(
    bill_length_mm,
    bill_depth_mm,
    flipper_length_mm,
    body_mass_g
  )

correlation_matrix <- cor(
  correlation_values,
  method = "pearson"
)

rounded_correlation_matrix <- round(
  correlation_matrix,
  3
)

cat("\n================ CORRELATION MATRIX ============\n")
print(rounded_correlation_matrix)

write.csv(
  rounded_correlation_matrix,
  "../outputs/correlation_matrix.csv",
  row.names = TRUE
)

png(
  filename = "../outputs/figures/correlation_heatmap.png",
  width = 1200,
  height = 900,
  res = 150
)

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE,
  title = "Correlation Heatmap of Penguin Measurements",
  mar = c(0, 0, 2, 0)
)

dev.off()


# 10. SIMPLE LINEAR REGRESSION
# Predict body mass using flipper length

simple_regression <- lm(
  body_mass_g ~ flipper_length_mm,
  data = numeric_complete
)

simple_results <- summary(simple_regression)
simple_coefficients <- coef(simple_regression)

simple_intercept <- simple_coefficients["(Intercept)"]
simple_slope <- simple_coefficients["flipper_length_mm"]

cat("\n================ SIMPLE REGRESSION =============\n")
print(simple_results)

cat("\nRegression equation:\n")

cat(
  "Predicted body mass =",
  round(simple_intercept, 3),
  "+",
  round(simple_slope, 3),
  "x flipper length\n"
)

cat(
  "R-squared:",
  round(simple_results$r.squared, 4),
  "\n"
)

cat(
  "Adjusted R-squared:",
  round(simple_results$adj.r.squared, 4),
  "\n"
)

cat(
  "Residual standard error:",
  round(sigma(simple_regression), 3),
  "\n"
)

simple_regression_interpretation <- paste(
  round(simple_results$r.squared * 100, 2),
  "% of the variation in body mass is explained",
  "by flipper length in the simple regression model."
)

cat(
  "Interpretation:",
  simple_regression_interpretation,
  "\n"
)


# 11. MULTIPLE LINEAR REGRESSION
# Predict body mass using several measurements

multiple_regression <- lm(
  body_mass_g ~
    flipper_length_mm +
    bill_length_mm +
    bill_depth_mm,
  data = numeric_complete
)

multiple_results <- summary(multiple_regression)
multiple_coefficients <- coef(multiple_regression)

cat("\n================ MULTIPLE REGRESSION ===========\n")
print(multiple_results)

cat("\nRegression equation:\n")

cat(
  "Predicted body mass =",
  round(multiple_coefficients["(Intercept)"], 3),
  "+",
  round(multiple_coefficients["flipper_length_mm"], 3),
  "x flipper length +",
  round(multiple_coefficients["bill_length_mm"], 3),
  "x bill length +",
  round(multiple_coefficients["bill_depth_mm"], 3),
  "x bill depth\n"
)

cat(
  "R-squared:",
  round(multiple_results$r.squared, 4),
  "\n"
)

cat(
  "Adjusted R-squared:",
  round(multiple_results$adj.r.squared, 4),
  "\n"
)

cat(
  "Residual standard error:",
  round(sigma(multiple_regression), 3),
  "\n"
)

multiple_regression_interpretation <- paste(
  round(multiple_results$r.squared * 100, 2),
  "% of the variation in body mass is explained",
  "by flipper length, bill length and bill depth."
)

cat(
  "Interpretation:",
  multiple_regression_interpretation,
  "\n"
)


# 12. MODEL COMPARISON

regression_comparison <- data.frame(
  model = c(
    "Simple regression",
    "Multiple regression"
  ),
  r_squared = c(
    simple_results$r.squared,
    multiple_results$r.squared
  ),
  adjusted_r_squared = c(
    simple_results$adj.r.squared,
    multiple_results$adj.r.squared
  ),
  residual_standard_error = c(
    sigma(simple_regression),
    sigma(multiple_regression)
  )
)

regression_comparison <- regression_comparison %>%
  mutate(
    r_squared = round(r_squared, 4),
    adjusted_r_squared = round(
      adjusted_r_squared,
      4
    ),
    residual_standard_error = round(
      residual_standard_error,
      3
    )
  )

cat("\n================ MODEL COMPARISON ===============\n")
print(regression_comparison)

write.csv(
  regression_comparison,
  "../outputs/model_comparison.csv",
  row.names = FALSE
)

if (
  multiple_results$adj.r.squared >
    simple_results$adj.r.squared
) {
  better_model <- "Multiple regression"

  model_comparison_interpretation <- paste(
    "The multiple regression model provides a better fit",
    "because it has the higher adjusted R-squared value."
  )
} else {
  better_model <- "Simple regression"

  model_comparison_interpretation <- paste(
    "The simple regression model provides a better or comparable fit",
    "based on its adjusted R-squared value."
  )
}

cat("Preferred model:", better_model, "\n")

cat(
  "Interpretation:",
  model_comparison_interpretation,
  "\n"
)


# 13. SCATTER PLOT WITH REGRESSION LINE

scatter_chart <- ggplot(
  numeric_complete,
  aes(
    x = flipper_length_mm,
    y = body_mass_g
  )
) +
  geom_point(
    alpha = 0.65
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Relationship Between Body Mass and Flipper Length",
    subtitle = "The line represents the fitted simple linear regression model",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  ) +
  theme_minimal()

ggsave(
  filename = "../outputs/figures/scatter_plot.png",
  plot = scatter_chart,
  width = 8,
  height = 6,
  dpi = 300
)


# 14. BOXPLOT OF BODY MASS BY SPECIES

boxplot_data <- penguin_data %>%
  filter(
    !is.na(species),
    !is.na(body_mass_g)
  )

boxplot_chart <- ggplot(
  boxplot_data,
  aes(
    x = species,
    y = body_mass_g,
    fill = species
  )
) +
  geom_boxplot(
    show.legend = FALSE
  ) +
  labs(
    title = "Body Mass Distribution by Penguin Species",
    x = "Penguin Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal()

ggsave(
  filename = "../outputs/figures/species_boxplot.png",
  plot = boxplot_chart,
  width = 8,
  height = 6,
  dpi = 300
)


# 15. HISTOGRAM WITH NORMAL DISTRIBUTION CURVE

mass_values <- penguin_data %>%
  filter(!is.na(body_mass_g))

mass_average <- mean(
  mass_values$body_mass_g
)

mass_standard_deviation <- sd(
  mass_values$body_mass_g
)

histogram_chart <- ggplot(
  mass_values,
  aes(x = body_mass_g)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    alpha = 0.7
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mass_average,
      sd = mass_standard_deviation
    ),
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Penguin Body Mass",
    subtitle = "The curve represents a fitted normal distribution",
    x = "Body Mass (g)",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "../outputs/figures/body_mass_histogram.png",
  plot = histogram_chart,
  width = 8,
  height = 6,
  dpi = 300
)


# 16. SAVE KEY RESULTS

important_results <- data.frame(
  measure = c(
    "Total observations",
    "Complete numerical observations",
    "Observations with recorded sex",
    "Male penguins",
    "Observed male proportion",
    "T-test statistic",
    "T-test p-value",
    "Proportion test statistic",
    "Proportion test p-value",
    "Simple regression R-squared",
    "Simple regression adjusted R-squared",
    "Multiple regression R-squared",
    "Multiple regression adjusted R-squared",
    "Preferred regression model"
  ),
  value = c(
    as.character(nrow(penguin_data)),
    as.character(nrow(numeric_complete)),
    as.character(number_known_sex),
    as.character(number_male),
    as.character(round(observed_male_proportion, 4)),
    as.character(round(unname(body_mass_test$statistic), 4)),
    as.character(body_mass_test$p.value),
    as.character(round(unname(male_test$statistic), 4)),
    as.character(male_test$p.value),
    as.character(round(simple_results$r.squared, 4)),
    as.character(round(simple_results$adj.r.squared, 4)),
    as.character(round(multiple_results$r.squared, 4)),
    as.character(round(multiple_results$adj.r.squared, 4)),
    better_model
  )
)

write.csv(
  important_results,
  "../outputs/key_results.csv",
  row.names = FALSE
)


# 17. SAVE FULL ANALYSIS TO A TEXT FILE

report_file <- "../outputs/reports/analysis_results.txt"

sink(report_file)

cat("DTS 204 PENGUINS ANALYSIS RESULTS\n")
cat("=================================\n\n")

cat("DATASET INFORMATION\n")
cat("-------------------\n")
cat("Total rows:", nrow(penguin_data), "\n")
cat("Total columns:", ncol(penguin_data), "\n")
cat("Complete numerical rows:", nrow(numeric_complete), "\n")
cat("Rows with recorded sex:", number_known_sex, "\n\n")

cat("COLUMN NAMES\n")
cat("------------\n")
print(names(penguin_data))

cat("\nMISSING VALUES\n")
cat("--------------\n")
print(missing_summary)
cat("Total missing values:", sum(missing_summary), "\n")

cat("\nSUMMARY STATISTICS\n")
cat("------------------\n")
print(summary_table)

cat("\nTWO-SAMPLE T-TEST\n")
cat("-----------------\n")
cat(
  "H0: Mean body mass is the same for Adelie and Gentoo penguins.\n"
)
cat(
  "H1: Mean body mass differs between Adelie and Gentoo penguins.\n\n"
)
print(species_mass_summary)
print(body_mass_test)
cat("\nDecision:", t_test_decision, "\n")
cat("Interpretation:", t_test_interpretation, "\n")

cat("\nPROPORTION TEST\n")
cat("----------------\n")
cat("H0: The proportion of male penguins is 0.50.\n")
cat(
  "H1: The proportion of male penguins is not equal to 0.50.\n\n"
)
cat("Male penguins:", number_male, "\n")
cat("Known-sex observations:", number_known_sex, "\n")
cat(
  "Observed male proportion:",
  round(observed_male_proportion, 4),
  "\n"
)
print(male_test)
cat("\nDecision:", proportion_decision, "\n")
cat(
  "Interpretation:",
  proportion_interpretation,
  "\n"
)

cat("\nCORRELATION MATRIX\n")
cat("------------------\n")
print(rounded_correlation_matrix)

cat("\nSIMPLE LINEAR REGRESSION\n")
cat("------------------------\n")
print(simple_results)
cat("\nRegression equation:\n")
cat(
  "Predicted body mass =",
  round(simple_intercept, 3),
  "+",
  round(simple_slope, 3),
  "x flipper length\n"
)
cat(
  "\nInterpretation:",
  simple_regression_interpretation,
  "\n"
)

cat("\nMULTIPLE LINEAR REGRESSION\n")
cat("--------------------------\n")
print(multiple_results)
cat("\nRegression equation:\n")
cat(
  "Predicted body mass =",
  round(multiple_coefficients["(Intercept)"], 3),
  "+",
  round(multiple_coefficients["flipper_length_mm"], 3),
  "x flipper length +",
  round(multiple_coefficients["bill_length_mm"], 3),
  "x bill length +",
  round(multiple_coefficients["bill_depth_mm"], 3),
  "x bill depth\n"
)
cat(
  "\nInterpretation:",
  multiple_regression_interpretation,
  "\n"
)

cat("\nMODEL COMPARISON\n")
cat("----------------\n")
print(regression_comparison)
cat("\nPreferred model:", better_model, "\n")
cat(
  "Interpretation:",
  model_comparison_interpretation,
  "\n"
)

sink()


# 18. COMPLETION MESSAGE

cat("\n=================================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("=================================================\n")

cat("\nTables created:\n")
cat("- outputs/summary_statistics.csv\n")
cat("- outputs/correlation_matrix.csv\n")
cat("- outputs/model_comparison.csv\n")
cat("- outputs/key_results.csv\n")

cat("\nFigures created:\n")
cat("- outputs/figures/correlation_heatmap.png\n")
cat("- outputs/figures/scatter_plot.png\n")
cat("- outputs/figures/species_boxplot.png\n")
cat("- outputs/figures/body_mass_histogram.png\n")

cat("\nReport output created:\n")
cat("- outputs/reports/analysis_results.txt\n")

cat("\nOpen analysis_results.txt to view the complete results.\n")
