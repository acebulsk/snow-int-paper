# run stats to determine if increase in i/p with wind and canopy snow load is significant
library(tidyverse)
library(broom)
library(modelr)

# TODO tried aggregating the 15min IP obs to hourly and daily but still show significant noise... should try and aggregate the troughfall and precip data separately and then aggregate to properly show this...

# Functions ----

# Function to perform ANOVA
anova_fn <- function(data, x_var) {
  # Perform ANOVA
  anova_model <- aov(as.formula(paste0('IP ~ ', x_var)), data = data)
  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Combine ANOVA and post-hoc results
  return(anova_results)
}

# Function to perform ANOVA post hoc analysis
anova_posthoc_fn <- function(data, x_var, alternative_hypothesis) {
  # Ensure x_var is a factor and has correct levels
  data[[x_var]] <- factor(data[[x_var]], levels = unique(data[[x_var]]))

  # Perform ANOVA
  anova_model <- aov(as.formula(paste0('IP ~ ', x_var)), data = data)

  anova_results <- tidy(anova_model) %>%
    mutate(analysis_type = "ANOVA")

  # Perform GLHT for adjacent bins
  levels <- levels(data[[x_var]])
  n_levels <- length(levels)

  # Create contrast matrix for adjacent comparisons
  if (n_levels < 2) {
    return(anova_results)  # Return only ANOVA results if less than 2 levels
  }

  contrasts <- matrix(0, nrow = n_levels - 1, ncol = n_levels)
  for (i in 1:(n_levels - 1)) {
    contrasts[i, i] <- -1
    contrasts[i, i + 1] <- 1
  }
  rownames(contrasts) <- paste(levels[-n_levels], "-", levels[-1])

  # Perform GLHT
  contrast_list <- list(contrasts)
  names(contrast_list) <- x_var

  glht_result <- multcomp::glht(anova_model,
                                # linfct = mcp(temp_binned = contrasts), # typically its this but we make some adjustements to work as a function
                                linfct = do.call(multcomp::mcp, contrast_list),
                                alternative = alternative_hypothesis)

  # Extract and tidy GLHT results
  posthoc <- tidy(summary(glht_result)) %>%
    mutate(
      analysis_type = "Post-hoc",
      contrast = rownames(contrasts)
    )

  # Combine ANOVA and post-hoc results
  bind_rows(anova_results, posthoc)
}

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

# air temp vs i/p ----

# at_ip <- met_intercept |>
#   mutate(datetime = format(datetime, "%Y-%m-%d 00:00:00")) |>
#   select(datetime, name, t, IP) |>
#   group_by(datetime, name) |>
#   summarise(t = mean(t, na.rm = T),
#             IP = mean(IP, na.rm = T),
#             n = n()) |>
#   filter(n > 10)

at_ip <- met_intercept |>
  select(datetime, name, t, IP)

## test significance of linear models ----

lm_nest <- at_ip |>
  group_by(name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ t, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(t, resid, colour = name)) +
  geom_point(aes(group = name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(name, adj.r.squared, p.value)


preds <- unnest(lm_nest, preds) |>
  select(name, t, IP, pred) |>
  left_join(model_summaries, by = "name")

preds |>
  ggplot() +
  geom_point(aes(t, IP), alpha = 0.5) +
  geom_line(aes(t, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~name) +
  geom_text(data = model_summaries,
            aes(x = Inf, y = Inf,
                label = sprintf("R² = %.3f\np = %.3e", adj.r.squared, p.value)),
            hjust = 1.1, vjust = 1.1, size = 3) +
  theme_bw() +
  labs(x = "t", y = "IP") +
  theme(legend.position = "bottom")

## try pearsons R on non-binned data ----

# same as unsquared R2.....

at_ip_cor <- at_ip |>
  # filter(t < 2) |>
  group_by(name) |>
  summarize(
    correlation = cor(t, IP, method = "pearson"),
    test_results = list(cor.test(t, IP, method = "pearson")),
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

at_ip_cor



## try anova across all wind speed bins ----

at_ip <- met_intercept |>
  select(datetime, name, t, temp_binned, IP) |>
  mutate(temp_binned = factor(temp_binned))

grouped_anova_res <- at_ip |>
  group_by(name) |>
  group_modify(~anova_fn(.x, 'temp_binned'))

# Post hoc analysis

# Run the function in the pipeline
grouped_anova_posthoc_res <- at_ip |>
  # filter(name == 'dense_forest') |>
  group_by(name) |>
  group_modify(~anova_posthoc_fn(.x, 'temp_binned', 'greater')) |>
  mutate(is_significant = ifelse(adj.p.value < 0.05, TRUE, FALSE))

# Generate and print summary for each dataset
summaries <- grouped_anova_posthoc_res %>%
  group_by(name) %>%
  group_modify(~tibble(summary = generate_summary(.x))) %>%
  mutate(named_summary = paste0(name, ':\n', summary)) |>
  pull(named_summary)

cat("Analysis Summary:\n\n")
cat(paste(summaries, collapse = ""))
# Visualize the data
ggplot(at_ip, aes(x = temp_binned, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Wind Speed Bin", y = "Y Value", title = "Y Value by Wind Speed Bin for Each Dataset") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## T test between manually defined threshold

# fails normality test so should probabliy use wilcox.test
at_ip_ttest <- at_ip |>
  mutate(group = ifelse(t < t_th, 'cold', 'warm'))

# Visualize the data
ggplot(at_ip_ttest, aes(x = group, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = element_blank(), y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

normality_test <- at_ip_ttest |>
  group_by(name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- at_ip_ttest |>
  group_by(name) |>
  summarise(
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))

## t test (are the means of two groups significantly different) ----
# useing a one-tailed t-test with the greater option to see if windy is greater than calm
# Null Hypothesis (H₀): The mean of the first group is less than or equal to the mean of the second group. H0:μ1≤μ2H0​:μ1​≤μ2​
# Alternative Hypothesis (H₁): The mean of the first group is greater than the mean of the second group. H1:μ1>μ2H1​:μ1​>μ2

t_test <- at_ip_ttest |>
  group_by(name) |>
  summarise(
    t_test = list(t.test(IP[group == "cold"], IP[group == "warm"],
                         alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(t_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

## wilcox.test (are the medians of two groups significantly different) ----

w_test_1 <- at_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "cold"], IP[group == "warm"],
                              alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

# wind vs i/p ----

wind_ip <- met_intercept |>
  select(datetime, name, u, IP)

## test significance of linear models ----

lm_nest <- wind_ip |>
  group_by(name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ u, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(u, resid, colour = name)) +
  geom_point(aes(group = name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(name, adj.r.squared, p.value)


preds <- unnest(lm_nest, preds) |>
  select(name, u, IP, pred) |>
  left_join(model_summaries, by = "name")

preds |>
  ggplot() +
  geom_point(aes(u, IP), alpha = 0.5) +
  geom_line(aes(u, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~name) +
  geom_text(data = model_summaries,
            aes(x = Inf, y = Inf,
                label = sprintf("R² = %.3f\np = %.3e", adj.r.squared, p.value)),
            hjust = 1.1, vjust = 1.1, size = 3) +
  theme_bw() +
  labs(x = "u", y = "IP") +
  theme(legend.position = "bottom")

## try pearsons R on non-binned data ----

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

## try anova across all wind speed bins ----

wind_ip <- met_intercept |>
  select(datetime, name, u, wind_binned, IP) |>
  mutate(wind_binned = factor(wind_binned))

grouped_anova_res <- wind_ip |>
  group_by(name) |>
  group_modify(~anova_fn(.x, 'wind_binned'))

# Post hoc analysis

grouped_anova_posthoc_res <- wind_ip |>
  group_by(name) |>
  group_modify(~anova_posthoc_fn(.x, 'wind_binned', 'greater')) |>
  mutate(is_significant = ifelse(adj.p.value < 0.05, T, F))

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
  mutate(group = ifelse(u < u_th, 'calm', 'windy'))

# Visualize the data
ggplot(wind_ip_ttest, aes(x = group, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = element_blank(), y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
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
    t_test = list(t.test(IP[group == "windy"], IP[group == "calm"],
                         alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(t_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

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
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"],
                              alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

## wilcox.test (are the medians of two groups significantly different) ----

w_test_1 <- wind_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "windy"], IP[group == "calm"],
                              alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

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
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"],
                              alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

# snow load vs i/p ----

w_ip <- met_intercept |>
  filter(is.na(weighed_tree_canopy_load_mm) == F) |>
  select(datetime, name, w = weighed_tree_canopy_load_mm, IP)

## test significance of linear models ----

lm_nest <- w_ip |>
  group_by(name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ w, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(w, resid, colour = name)) +
  geom_point(aes(group = name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(name, adj.r.squared, p.value)

preds <- unnest(lm_nest, preds) |>
  select(name, w, IP, pred) |>
  left_join(model_summaries, by = "name")

preds |>
  ggplot() +
  geom_point(aes(w, IP), alpha = 0.5) +
  geom_line(aes(w, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~name) +
  geom_text(data = model_summaries,
            aes(x = Inf, y = Inf,
                label = sprintf("R² = %.3f\np = %.3e", adj.r.squared, p.value)),
            hjust = 1.1, vjust = 1.1, size = 3) +
  theme_bw() +
  labs(x = "w", y = "IP") +
  theme(legend.position = "bottom")

## try pearsons R on non-binned data ----

tree_ip_cor <- w_ip |>
  # filter(w < 2) |>
  group_by(name) |>
  summarize(
    correlation = cor(w, IP, method = "pearson"),
    test_results = list(cor.test(w, IP, method = "pearson")),
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

tree_ip_cor

## try anova across all tree speed bins ----

w_ip <- met_intercept |>
  filter(is.na(weighed_tree_canopy_load_mm) == F) |>
  select(datetime, name, w = weighed_tree_canopy_load_mm, tree_binned, IP) |>
  mutate(tree_binned = factor(tree_binned))

grouped_anova_res <- w_ip |>
  group_by(name) |>
  group_modify(~anova_fn(.x, 'tree_binned'))

# Post hoc analysis

grouped_anova_posthoc_res <- w_ip |>
  group_by(name) |>
  group_modify(~anova_posthoc_fn(.x, 'tree_binned', 'greater')) |>
  mutate(is_significant = ifelse(adj.p.value < 0.05, T, F))

# Generate and print summary for each dataset
summaries <- grouped_anova_posthoc_res %>%
  group_by(name) %>%
  group_modify(~tibble(summary = generate_summary(.x))) %>%
  mutate(named_summary = paste0(name, ':\n', summary)) |>
  pull(named_summary)

cat("Analysis Summary:\n\n")
cat(paste(summaries, collapse = ""))
# Visualize the data
ggplot(w_ip, aes(x = tree_binned, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Canopy Snow Load Bin (mm)", y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

w_ip <- met_intercept |>
  select(datetime, name, w = weighed_tree_canopy_load_mm, tree_binned, IP) |>
  mutate(tree_binned = factor(tree_binned))

grouped_anova_res <- w_ip |>
  group_by(name) |>
  group_modify(~anova_fn(.x, 'tree_binned'))

# Post hoc analysis

grouped_anova_posthoc_res <- w_ip |>
  group_by(name) |>
  group_modify(~anova_posthoc_fn(.x, 'tree_binned', 'greater')) |>
  mutate(is_significant = ifelse(adj.p.value < 0.05, T, F))

# Generate and print summary for each dataset
summaries <- grouped_anova_posthoc_res %>%
  group_by(name) %>%
  group_modify(~tibble(summary = generate_summary(.x))) %>%
  mutate(named_summary = paste0(name, ':\n', summary)) |>
  pull(named_summary)

cat("Analysis Summary:\n\n")
cat(paste(summaries, collapse = ""))
# Visualize the data
ggplot(w_ip, aes(x = tree_binned, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Canopy Snow Load Bin (mm)", y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## T test between manually defined threshold

# fails normality test so should probabliy use wilcox.test
tree_ip_ttest <- w_ip |>
  filter(is.na(w) == F) |>
  mutate(group = factor(ifelse(w < w_th, 'low', 'high'),
                        levels = c('low', 'high')))

# Visualize the data
ggplot(tree_ip_ttest, aes(x = group, y = IP, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Canopy Snow Load Bin (mm)", y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

normality_test <- tree_ip_ttest |>
  group_by(name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- tree_ip_ttest |>
  group_by(name) |>
  summarise(
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))

## t test (are the means of two groups significantly different) ----
# useing a one-tailed t-test with the greater option to see if treey is greater than calm
# Null Hypothesis (H₀): The mean of the first group is less than or equal to the mean of the second group. H0:μ1≤μ2H0​:μ1​≤μ2​
# Alternative Hypothesis (H₁): The mean of the first group is greater than the mean of the second group. H1:μ1>μ2H1​:μ1​>μ2

t_test <- tree_ip_ttest |>
  group_by(name) |>
  summarise(
    t_test = list(t.test(IP[group == "low"], IP[group == "high"], alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(t_test)) |>
  mutate(low_greater_than_high = ifelse(p.value > 0.05, T, F))

## wilcox.test (are the medians of two groups significantly different) ----

w_test_1 <- tree_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "low"], IP[group == "high"], alternative = "greater") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(low_greater_than_high = ifelse(p.value < 0.05, T, F))

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

w_test_2 <- tree_ip_ttest |>
  group_by(name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"], alternative = "less") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(calm_greater_than_windy = ifelse(p.value > 0.05, TRUE, FALSE))
