

# Needed libraries 
library(tidyverse)     #streamlined tools for clean, readable data analysis.
library(haven)         # To load .sav files
library(lme4)          # Linear Mixed Models
library(lmerTest)      # p-values for LMMs 
library(performance)   # ICC and model diagnostics
library(broom.mixed)   # Tidy model outputs
library(psych)         # Descriptives and correlation
library(flexplot)      #for relationships and diagnostics.

# importing data set
data <- read_sav("P6003.A4.sav") 

# Sub-setting of new needed data set from original data set
data_needed <- data %>% select(
  id,      # Unique participant ID (Level 2)
  day,     # Measurement day (Level 1)
  swl,     # Satisfaction with life
  tipm.E,  # Extraversion
  tipm.N  )  # Neuroticism)

# Inspect the resulting data frame
print(data_needed)

glimpse(data_needed)

# rename data set from data_needed to data for consistency 

data <- data_needed

# Confirm data structure
str(data)

# Recheck variable names

names(data)
#  Check for missing values
sum(is.na(data))




# List of variables to check
vars <- c("id", "day", "swl", "tipm.E", "tipm.N")

# Check if all variables exist in the dataset
if (all(vars %in% names(data))) {
  # Count missing values for each variable
  missing_summary <- sapply(data[vars], function(x) sum(is.na(x)))
  
  # Print the summary
  print(missing_summary)
} else {
  # Notify if some variables are missing in the dataset
  print("One or more variables in 'vars' do not exist in the dataset.")
}


library(naniar)

# Perform Little's MCAR test
mcar_test(data)



# Count number of participants and observations
n_id <- length(unique(data$id))       
n_obs <- nrow(data)                   
cat("Number of participants (id):", n_id, "\n")
cat("Number of observations:", n_obs, "\n")

library(dplyr)
library(gt)

# Compute descriptive stats 
apa_desc <- data %>%
  select(swl, tipm.E, tipm.N) %>%
  summarise(
    swl = list(c(
      Mean = round(mean(swl, na.rm = TRUE), 2),
      SD = round(sd(swl, na.rm = TRUE), 2),
      Min = round(min(swl, na.rm = TRUE), 2),
      Max = round(max(swl, na.rm = TRUE), 2),
      Median = round(median(swl, na.rm = TRUE), 2),
      IQR = round(IQR(swl, na.rm = TRUE), 2),
      Range = round(diff(range(swl, na.rm = TRUE)), 2)
    )),
    tipm.E = list(c(
      Mean = round(mean(tipm.E, na.rm = TRUE), 2),
      SD = round(sd(tipm.E, na.rm = TRUE), 2),
      Min = round(min(tipm.E, na.rm = TRUE), 2),
      Max = round(max(tipm.E, na.rm = TRUE), 2),
      Median = round(median(tipm.E, na.rm = TRUE), 2),
      IQR = round(IQR(tipm.E, na.rm = TRUE), 2),
      Range = round(diff(range(tipm.E, na.rm = TRUE)), 2)
    )),
    tipm.N = list(c(
      Mean = round(mean(tipm.N, na.rm = TRUE), 2),
      SD = round(sd(tipm.N, na.rm = TRUE), 2),
      Min = round(min(tipm.N, na.rm = TRUE), 2),
      Max = round(max(tipm.N, na.rm = TRUE), 2),
      Median = round(median(tipm.N, na.rm = TRUE), 2),
      IQR = round(IQR(tipm.N, na.rm = TRUE), 2),
      Range = round(diff(range(tipm.N, na.rm = TRUE)), 2)
    ))
  ) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Stats") %>%
  tidyr::unnest_wider(Stats)

# Format APA-style table with abbreviations
apa_table <- apa_desc %>%
  mutate(Variable = case_when(
    Variable == "swl" ~ "*swl*",
    Variable == "tipm.E" ~ "*tipm.E*",
    Variable == "tipm.N" ~ "*tipm.N*"
  )) %>%
  gt() %>%
  tab_header(
    title = md("**Table 1**  
Descriptive Statistics for swl, tipm.E, and tipm.N")
  ) %>%
  cols_label(
    Variable = "Variable",
    Mean = "M",
    SD = "SD",
    Min = "Min",
    Max = "Max",
    Median = "Median",
    IQR = "IQR",
    Range = "Range"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_options(
    table.align = "center",
    column_labels.font.weight = "bold",
    table.font.size = 12,
    heading.title.font.size = 12,
    data_row.padding = px(4),
    table.border.top.style = "none",
    table.border.bottom.style = "solid",
    table.border.bottom.color = "black"
  )

# Show the table
apa_table




# Create histograms to visualize distributions of key variable

# Histogram for Satisfaction with Life
p1 <- ggplot(data, aes(x = swl)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "skyblue", color = "black", na.rm = TRUE) +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$swl, na.rm = TRUE), sd = sd(data$swl, na.rm = TRUE)),
                color = "blue", size = 1) +
  labs(title = "Distribution of Satisfaction with Life", x = "Satisfaction with Life", y = "Density")

# Histogram for Extraversion
p2 <- ggplot(data, aes(x = tipm.E)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "lightgreen", color = "black", na.rm = TRUE) +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$tipm.E, na.rm = TRUE), sd = sd(data$tipm.E, na.rm = TRUE)),
                color = "darkgreen", size = 1) +
  labs(title = "Distribution of Extraversion", x = "Extraversion Score", y = "Density")

# Histogram for Neuroticism
p3 <- ggplot(data, aes(x = tipm.N)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "salmon", color = "black", na.rm = TRUE) +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$tipm.N, na.rm = TRUE), sd = sd(data$tipm.N, na.rm = TRUE)),
                color = "red", size = 1) +
  labs(title = "Distribution of Neuroticism", x = "Neuroticism Score", y = "Density")

# Display plots
p1
p2
p3


#  Create scatterplots to visualize relationships between variables
library(ggplot2)
library(dplyr)

# Select relevant variables
plot_data <- data %>%
  select(swl, tipm.E, tipm.N)

# Extraversion Plot
p4 <- ggplot(plot_data, aes(x = tipm.E, y = swl)) +
  geom_point(alpha = 0.6, size = 2, na.rm = TRUE, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 1, na.rm = TRUE) +
  labs(
    title = "Extraversion and Satisfaction with Life",
    x = "Extraversion Score",
    y = "Satisfaction with Life Score"
  ) +
  theme_minimal(base_size = 14)

# Neuroticism Plot 
p5 <- ggplot(plot_data, aes(x = tipm.N, y = swl)) +
  geom_point(alpha = 0.6, size = 2, na.rm = TRUE, color = "#de2d26") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", linewidth = 1, na.rm = TRUE) +
  labs(
    title = "Neuroticism and Satisfaction with Life",
    x = "Neuroticism Score",
    y = "Satisfaction with Life Score"
  ) +
  theme_minimal(base_size = 14)

# Display the plots

p4  # Extraversion plot
p5  # Neuroticism plot



library(dplyr)
library(psych)
library(gt)


# Select relevant variables
vars <- data %>% select(swl, tipm.E, tipm.N)

# Compute covariance matrix (uses pairwise complete observations)
cov_matrix <- cov(vars, use = "pairwise.complete.obs")

# View the result
print(cov_matrix)



# Compute bivariate correlation matrix and p-values using abbreviations
corr_results <- psych::corr.test(
  data %>% select(swl, tipm.E, tipm.N),
  use = "pairwise"
)

r <- round(corr_results$r, 2)
p <- round(corr_results$p, 3)

# Format correlations with asterisks
format_r <- function(r, p) {
  stars <- if (p < .001) {
    "***"
  } else if (p < .01) {
    "**"
  } else if (p < .05) {
    "*"
  } else {
    ""
  }
  paste0(r, stars)
}

# Create final correlation + descriptives table using abbreviations
apa_corr <- tibble::tibble(
  Variable = c("swl", "tipm.E", "tipm.N"),
  M = round(sapply(data[c("swl", "tipm.E", "tipm.N")], mean, na.rm = TRUE), 2),
  SD = round(sapply(data[c("swl", "tipm.E", "tipm.N")], sd, na.rm = TRUE), 2),
  swl = c(
    "1.00",
    format_r(r["swl", "tipm.E"], p["swl", "tipm.E"]),
    format_r(r["swl", "tipm.N"], p["swl", "tipm.N"])
  ),
  tipm.E = c(
    format_r(r["tipm.E", "swl"], p["tipm.E", "swl"]),
    "1.00",
    format_r(r["tipm.E", "tipm.N"], p["tipm.E", "tipm.N"])
  ),
  tipm.N = c(
    format_r(r["tipm.N", "swl"], p["tipm.N", "swl"]),
    format_r(r["tipm.N", "tipm.E"], p["tipm.N", "tipm.E"]),
    "1.00"
  )
)

# Create the APA-style table with gt and abbreviations
gt(apa_corr) %>%
  tab_header(
    title = md("Means, Standard Deviations, and Bivariate Correlations for Key Variables")
  ) %>%
  cols_label(
    Variable = "Variable",
    M = "M",
    SD = "SD",
    swl = "*swl*",
    tipm.E = "*tipm.E*",
    tipm.N = "*tipm.N*"
  ) %>%
  fmt_number(columns = c(M, SD), decimals = 2) %>%
  tab_footnote(
    footnote = md("**Note.** *N* = 4,252. Values are Pearson correlations. *p* < .05 = *, *p* < .01 = **, *p* < .001 = ***."),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    data_row.padding = px(4),
    table.align = "center"
  )

# Assess multicollinearity
library(car)
library(dplyr)
library(gt)

# Fit the linear model 
lm_model <- lm(swl ~ tipm.E + tipm.N, data = data)

# Get VIF
vif_vals <- car::vif(lm_model)

# Compute Tolerance (1/VIF)
tolerance <- 1 / vif_vals

# Create data frame
vif_table <- data.frame(
  Predictor = names(vif_vals),
  VIF = round(vif_vals, 2),
  Tolerance = round(tolerance, 2)
)

# APA-style table using gt with abbreviations
vif_table %>%
  gt() %>%
  tab_header(
    title = md("Variance Inflation Factors (VIF) and Tolerance Values for Multicollinearity Assessment") 
  ) %>%
  cols_label(
    Predictor = "Predictor",
    VIF = "VIF",
    Tolerance = "1 / VIF (Tolerance)"
  ) %>%
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    table_body.hlines.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none",
    data_row.padding = px(4),
    table.align = "center"
  )


library(lme4)
library(performance)

# Baseline null model (intercepts only)
baseline_model <- lmer(swl ~ 1 +
                         (1 | id) +
                         (1 | day),
                       data = data, REML = FALSE)
summary(baseline_model)

# Intra-class correlation
performance::icc(baseline_model)

# Hypothesis 1: Extraversion only
model_H1 <- lmer(swl ~ tipm.E +
                   (1 | id) + (1 | day),
                 data = data, REML = FALSE)
summary(model_H1)

# Hypothesis 2: Neuroticism only
model_H2 <- lmer(swl ~ tipm.N +
                   (1 | id) + (1 | day),
                 data = data, REML = FALSE)
summary(model_H2)

# Hypothesis 3: Full model with both predictors and random slopes
model_H3 <- lmer(swl ~ tipm.E + tipm.N +
                   (1 + tipm.E + tipm.N | id) +
                   (1 + tipm.E + tipm.N | day),
                 data = data, REML = FALSE)
summary(model_H3)

# Model comparisons
anova(model_H1, model_H3)
anova(model_H2, model_H3)

library(flexplot)

#Get diagnostics
visualize(model_H3, plot = "residuals" )

# Check autocorrelation
acf(residuals(model_H3))

library(broom.mixed)
library(kableExtra)




# Ensure you have the final model summary and CI
summary_H3 <- summary(model_H3)
ci_H3 <- confint(model_H3, method = "Wald")


# Extract fixed effect names
fixed_names <- rownames(summary_H3$coefficients)

# Filter confidence intervals to include only fixed effects
ci_fixed <- ci_H3[fixed_names, , drop = FALSE]



# Build the fixed effects table
fixed_table <- data.frame(
  Parameter = fixed_names,
  Estimate = summary_H3$coefficients[, "Estimate"],
  SE = summary_H3$coefficients[, "Std. Error"],
  CI_lower = ci_fixed[, 1],
  CI_upper = ci_fixed[, 2],
  p = summary_H3$coefficients[, "Pr(>|t|)"]
)



# Model fit statistics
r2_vals <- performance::r2(model_H3)
icc_val <- performance::icc(model_H3)$ICC_adjusted

model_fit <- data.frame(
  Metric = c(
    "Intraclass Correlation Coefficient (ICC)",
    "R² Marginal (Fixed Effects)",
    "R² Conditional (Fixed + Random Effects)"
  ),
  Value = round(c(icc_val, r2_vals$R2_marginal, r2_vals$R2_conditional), 3)
)

# Display fixed effects table
kable(fixed_table, digits = 3, caption = "Table 2: Fixed Effects from Final Model") %>%
  kable_styling(full_width = FALSE)

# Display model fit table
kable(model_fit, digits = 3, caption = "Table 3: Model Fit Statistics") %>%
  kable_styling(full_width = FALSE)






