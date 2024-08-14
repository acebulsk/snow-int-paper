# run stats to determine if increase in i/p with wind and canopy snow load is significant
library(tidyverse)
library(broom)

# wind vs i/p

wind_ip <- met_intercept |>
  select(datetime, name, u, IP)

## try pearsons R on non-binned data (tons of noise, likely wont work)

wind_ip_cor <- wind_ip |>
  # filter(u < 2) |>
  group_by(name) |>
  summarize(
    correlation = cor(u, IP, method = "pearson"),
    test_results = list(cor.test(u, IP, method = "pearson")),
    .groups = "drop"
  ) |>
  mutate(tidy_results = map(test_results, tidy)) |>
  unnest(tidy_results) |>
  select(name, correlation, statistic, p.value, conf.low, conf.high) |>
  mutate(
    significant = ifelse(p.value < 0.05, "Yes", "No"),
    significance_level = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

wind_ip_cor

# Create scatterplots for each dataset
ggplot(wind_ip, aes(x = u, y = IP)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ name, scales = "free") +
  labs(x = "Wind Speed (m/s)", y = "Interception Efficiency (-)") +
  theme_minimal()

## try anova across all wind speed bins ----

wind_ip <- met_intercept |>
  select(datetime, name, u, wind_binned, IP) |>
  mutate(wind_binned = factor(wind_binned))

wind_ip_sparse <- wind_ip |> filter(name == 'sparse_forest')

# Analyze each dataset
anova_res_sparse <- aov(IP ~ wind_binned, data = wind_ip_sparse)
summary(anova_res_sparse)

# Define custom contrasts for one-sided tests
levels <- levels(wind_ip_sparse$wind_binned)[1:length(unique(wind_ip_sparse$wind_binned))]
n_levels <- length(levels)

# Create contrast matrix for one-sided comparisons (next > previous)
contrasts <- matrix(0, nrow = n_levels-1, ncol = n_levels)
for (i in 1:(n_levels - 1)) {
  contrasts[i, i] <- -1
  contrasts[i, i + 1] <- 1
}
rownames(contrasts) <- paste(levels[-1], "-", levels[-n_levels])

library(multcomp)

# Run custom post hoc test
one_sided_results <- glht(anova_res_sparse,
                          linfct = mcp(wind_binned = contrasts),
                          alternative = "greater")

# Summary of Tukey's HSD test
summary(one_sided_results)

# Function to perform ANOVA
anova_wind_data <- function(data) {
  # Perform ANOVA
  anova_model <- aov(IP ~ wind_binned, data = data)
  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Combine ANOVA and post-hoc results
  return(anova_results)
}

grouped_anova_res <- wind_ip |>
  group_by(name) |>
  group_modify(~anova_wind_data(.x))

# Post hoc analysis

# Function to perform ANOVA post hoc analysis
posthoc_wind_data <- function(data, alternative_hypothesis) {
  # Perform ANOVA
  anova_model <- aov(IP ~ wind_binned, data = data)
  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Perform GLHT for adjacent bins
  levels <- levels(data$wind_binned)[1:length(unique(data$wind_binned))]
  n_levels <- length(levels)

  # Create contrast matrix for adjacent comparisons
  contrasts <- matrix(0, nrow = n_levels - 1, ncol = n_levels)
  for (i in 1:(n_levels - 1)) {
    contrasts[i, i] <- -1
    contrasts[i, i + 1] <- 1
  }
  rownames(contrasts) <- paste(levels[-n_levels], "-", levels[-1])

  # Perform GLHT
  library(multcomp)
  glht_result <- glht(anova_model,
                      linfct = mcp(wind_binned = contrasts),
                      alternative = alternative_hypothesis
  )

  # Extract and tidy GLHT results
  posthoc <- tidy(summary(glht_result)) %>%
    mutate(
      analysis_type = "Post-hoc",
      contrast = rownames(contrasts)
    )

  # Combine ANOVA and post-hoc results
  bind_rows(anova_results, posthoc)
}

grouped_anova_posthoc_res <- wind_ip |>
  group_by(name) |>
  group_modify(~posthoc_wind_data(.x, 'greater')) |>
  mutate(is_significant = ifelse(adj.p.value < 0.05, T, F))


# Function to generate summary statement
generate_summary <- function(dataset_results) {
  anova_result <- dataset_results %>% filter(analysis_type == "ANOVA")
  posthoc_results <- dataset_results %>% filter(analysis_type == "Post-hoc")

  anova_significant <- anova_result$p.value[1] < 0.05

  significant_bins <- posthoc_results %>%
    filter(adj.p.value < 0.05, estimate > 0) %>%
    pull(contrast)

  summary <- paste0(
    "ANOVA: ", ifelse(anova_significant, "Significant", "Not significant"), "\n",
    "Significantly greater bins: ",
    ifelse(length(significant_bins) > 0,
           paste(significant_bins, collapse = ", "),
           "None"),
    "\n\n"
  )

  return(summary)
}

# Generate and print summary for each dataset
summaries <- grouped_anova_posthoc_res %>%
  group_by(name) %>%
  group_modify(~tibble(summary = generate_summary(.x))) %>%
  mutate(named_summary = paste0(name, ':\n', summary)) |>
  pull(named_summary)

cat("Analysis Summary:\n\n")
cat(paste(summaries, collapse = ""))
# Visualize the data
ggplot(wind_ip, aes(x = wind_binned, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Wind Speed Bin", y = "Y Value", title = "Y Value by Wind Speed Bin for Each Dataset") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## T test between manually defined threshold

# fails normality test so should probabliy use wilcox.test
wind_ip_ttest <- wind_ip |>
  mutate(group = ifelse(u < 1, 'calm', 'windy'))

normality_test <- wind_ip_ttest |>
  group_by(name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    levene_test = list(leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))

## t test (are the means of two groups significantly different) ----
# useing a one-tailed t-test with the greater option to see if windy is greater than calm
# Null Hypothesis (H₀): The mean of the first group is less than or equal to the mean of the second group. H0:μ1≤μ2H0​:μ1​≤μ2​
# Alternative Hypothesis (H₁): The mean of the first group is greater than the mean of the second group. H1:μ1>μ2H1​:μ1​>μ2

t_test <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    t_test = list(t.test(IP[group == "calm"], IP[group == "windy"], alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(t_test)) |>
  mutate(windy_greater_than_calm = ifelse(p.value > 0.05, T, F))

# now test if calm is greater than windy (i.e., for the medium density forest SCL)
# For alternative = 'less'
# Null Hypothesis (H₀): The mean (or median) of the first group is greater than or equal to the mean (or median) of the second group.
#
# Formally: H0:μ1≥μ2H0​:μ1​≥μ2​ (for means)
# Formally: H0:Median1≥Median2H0​:Median1​≥Median2​ (for medians)
#
# Here, μ1μ1​ and μ2μ2​ represent the population means of the two groups, and Median₁ and Median₂ represent the population medians.
#
# Alternative Hypothesis (H₁): The mean (or median) of the first group is less than the mean (or median) of the second group.
#
# Formally: H1:μ1<μ2H1​:μ1​<μ2​ (for means)
# Formally: H1:Median1<Median2H1​:Median1​<Median2​ (for medians)

t_test_2 <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"], alternative = "less") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(calm_greater_than_windy = ifelse(p.value > 0.05, TRUE, FALSE))

## wilcox.test (are the medians of two groups significantly different) ----

w_test_1 <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"], alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(windy_greater_than_calm = ifelse(p.value > 0.05, T, F))

# now test if calm is greater than windy (i.e., for the medium density forest SCL)
# For alternative = 'less'
# Null Hypothesis (H₀): The mean (or median) of the first group is greater than or equal to the mean (or median) of the second group.
#
# Formally: H0:μ1≥μ2H0​:μ1​≥μ2​ (for means)
# Formally: H0:Median1≥Median2H0​:Median1​≥Median2​ (for medians)
#
# Here, μ1μ1​ and μ2μ2​ represent the population means of the two groups, and Median₁ and Median₂ represent the population medians.
#
# Alternative Hypothesis (H₁): The mean (or median) of the first group is less than the mean (or median) of the second group.
#
# Formally: H1:μ1<μ2H1​:μ1​<μ2​ (for means)
# Formally: H1:Median1<Median2H1​:Median1​<Median2​ (for medians)

w_test_2 <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"], alternative = "less") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(calm_greater_than_windy = ifelse(p.value > 0.05, TRUE, FALSE))
