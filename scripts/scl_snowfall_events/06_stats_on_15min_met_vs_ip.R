# run stats to determine if increase in i/p with wind and canopy snow load is
# significant for the 15 minute interval measurements


library(tidyverse)
library(broom)
library(modelr)


# aggregate 15 min data ----

agg_interval <- F # should we aggregate the 15 min raw data to hourly before processing?
avg_period <- '1 hours'

good_wind <- 'u' # this is the qc and gap filled wind from FFR ultrasonic/3cup/pwlrmyoung
good_temp <- 't' # this is the mid tree FFR air temp

t_th <- -5
u_th <- 1
w_th <- 10

tf_periods_15min <- throughfall_periods_long |>
  left_join(ffr_met |> select(datetime, p, t, u)) |>
  left_join(q_tf_scl) |>
  left_join(w_tree_zrd, by = c('datetime', 'trough_name')) |>
  select(-c(storm_id, bad_troughs))

print(paste0('Aggregating to met and precip data to ', avg_period))

q_tf_met_tree_agg <- tf_periods_15min |>
  mutate(datetime = ceiling_date(datetime, unit = avg_period)) |> # ceiling ensures the timestamp corresponds to preeceeding records
  group_by(datetime, trough_name) |>
  summarise(p = sum(p),
            d_tf = sum(d_tf),
            IP = (p-d_tf)/p,
            t = mean(t),
            u = mean(u),
            weighed_tree_canopy_load_mm = last(weighed_tree_canopy_load_mm)
            )|>
  filter(p > 0,
         p > d_tf,
         d_tf > 0
  ) |>
  left_join(scl_lai_cc_fltr)

# air temp vs i/p ----

# at_ip <- met_intercept |>
#   mutate(datetime = format(datetime, "%Y-%m-%d 00:00:00")) |>
#   select(datetime, trough_name, t, IP) |>
#   group_by(datetime, trough_name) |>
#   summarise(t = mean(t, na.rm = T),
#             IP = mean(IP, na.rm = T),
#             n = n()) |>
#   filter(n > 10)

at_ip <- q_tf_met_tree_agg |>
  # filter(t > -20) |>
  select(datetime, trough_name, t, IP)

## test significance of linear models ----

lm_nest <- at_ip |>
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ t, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))
x_col <- 't'
at_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = x_col) |>
  select(trough_name,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(t, resid, colour = trough_name)) +
  geom_point(aes(group = trough_name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(trough_name, adj.r.squared, p.value)


preds <- unnest(lm_nest, preds) |>
  select(trough_name, t, IP, pred) |>
  left_join(model_summaries, by = "trough_name")

preds |>
  ggplot() +
  geom_point(aes(t, IP), alpha = 0.5) +
  geom_line(aes(t, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~trough_name) +
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
  # filter(t < 2, t > -15) |>
  group_by(trough_name) |>
  summarize(
    correlation = cor(t, IP, method = "pearson"),
    test_results = list(cor.test(t, IP, method = "pearson")),
    .groups = "drop"
  ) |>
  mutate(tidy_results = map(test_results, tidy)) |>
  unnest(tidy_results) |>
  select(trough_name, correlation, statistic, p.value, conf.low, conf.high) |>
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

# only sig correlation is for the mixed trough at -0.16 and stays the same removing outliers at low/high temps.

## T test between manually defined threshold

# fails normality test so should probabliy use wilcox.test
at_ip_ttest <- at_ip |>
  mutate(group = ifelse(t < t_th, 'cold', 'warm'))

# Visualize the data
ggplot(at_ip_ttest, aes(x = group, y = IP, fill = trough_name)) +
  geom_boxplot() +
  facet_wrap(~trough_name, scales = "free_y") +
  labs(x = element_blank(), y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

normality_test <- at_ip_ttest |>
  group_by(trough_name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- at_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))

## wilcox.test (are the medians of two groups significantly different) ----

# null hyp is that cold is greater or equal IP, alternative is that cold events have lower IP

at_wcox_test <- at_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == 'cold'], IP[group == 'warm'],
                              alternative = 'less') %>% tidy()),
    median_low = median(IP[group == 'cold'], na.rm = TRUE),
    median_high = median(IP[group == 'warm'], na.rm = TRUE),
    n_samples_low = sum(!is.na(IP[group == 'cold'])),
    n_samples_high = sum(!is.na(IP[group == 'warm']))
  ) %>%
  unnest(cols = c(w_test)) %>%
  mutate(
    reject_null_hyp = ifelse(p.value < 0.05, 'yes', 'no'),
    test_type = "Air Temperature",
    null_hypothesis = paste0("Median IP (Ta < ", t_th, "°C) ≥ Median IP (Ta ≥ ", t_th, "°C)"),
  ) %>%
  select(test_type, trough_name, null_hypothesis, p.value, n_samples_low, n_samples_high, median_low, median_high, reject_null_hyp)


# insig for all, i.e., accept the null hyp that cold is greater or equal.

# wind vs i/p ----

wind_ip <- q_tf_met_tree_agg |>
  select(datetime, trough_name, u, IP)

## test significance of linear models ----

lm_nest <- wind_ip |>
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ u, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

wind_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = 'u') |>
  select(trough_name,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(u, resid, colour = trough_name)) +
  geom_point(aes(group = trough_name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(trough_name, adj.r.squared, p.value)


preds <- unnest(lm_nest, preds) |>
  select(trough_name, u, IP, pred) |>
  left_join(model_summaries, by = "trough_name")

preds |>
  ggplot() +
  geom_point(aes(u, IP), alpha = 0.5) +
  geom_line(aes(u, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~trough_name) +
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
  group_by(trough_name) |>
  summarize(
    correlation = cor(u, IP, method = "pearson"),
    test_results = list(cor.test(u, IP, method = "pearson")),
    .groups = "drop"
  ) |>
  mutate(tidy_results = map(test_results, tidy)) |>
  unnest(tidy_results) |>
  select(trough_name, correlation, statistic, p.value, conf.low, conf.high) |>
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

# significant correlations for all, closed and sparse positive ~0.2 and mixed is negative -0.19

## T test between manually defined threshold

wind_ip_ttest <- wind_ip |>
  mutate(group = ifelse(u < u_th, 'calm', 'windy'))

# Visualize the data
ggplot(wind_ip_ttest, aes(x = group, y = IP, fill = trough_name)) +
  geom_boxplot() +
  facet_wrap(~trough_name, scales = "free_y") +
  labs(x = element_blank(), y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# fails normality test so should probabliy use wilcox.test

normality_test <- wind_ip_ttest |>
  group_by(trough_name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- wind_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))


## wilcox.test (are the medians of two groups significantly different) ----

#null hypothesisis that the first group is greater or equal to the second group i.e., if the median of the first group is significantly less than the second we reject
w_test <- wind_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == "calm"], IP[group == "windy"],
                              alternative = "less") %>% tidy())
  ) %>%
  unnest(cols = c(w_test)) |>
  mutate(reject_null_hyp = ifelse(p.value < 0.05, T, F))

wind_wcox_test <- wind_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == 'calm'], IP[group == 'windy'],
                              alternative = 'less') %>% tidy()),
    median_low = median(IP[group == 'calm'], na.rm = TRUE),
    median_high = median(IP[group == 'windy'], na.rm = TRUE),
    n_samples_low = sum(!is.na(IP[group == 'calm'])),
    n_samples_high = sum(!is.na(IP[group == 'windy']))
  ) %>%
  unnest(cols = c(w_test)) %>%
  mutate(
    reject_null_hyp = ifelse(p.value < 0.05, 'yes', 'no'),
    test_type = "Wind Speed",
    null_hypothesis = "Median IP (u < 1 m/s) ≥ Median IP (u ≥ 1 m/s)"
  ) %>%
  select(test_type, trough_name, null_hypothesis, p.value, n_samples_low, n_samples_high, median_low, median_high, reject_null_hyp)



# calm winds do not have significantly greater IP for the closed and sparse trough (p < 0.05). Test is insignificant for the mixed trough.


# snow load vs i/p ----

w_ip <- q_tf_met_tree_agg |>
  filter(is.na(weighed_tree_canopy_load_mm) == F) |>
  select(datetime, trough_name, w = weighed_tree_canopy_load_mm, IP)

## test significance of linear models ----

lm_nest <- w_ip |>
  group_by(trough_name) |>
  nest() |>
  mutate(model = map(data, ~lm(IP ~ w, data = .x)),
         resids = map2(data, model, add_residuals),
         preds = map2(data, model, add_predictions),
         glance = map(model, broom::glance))

w_model_summaries <- lm_nest |>
  unnest(glance) |>
  mutate(n = df.residual + 2,
         name = 'u') |>
  select(trough_name,
         name,
         r.squared,
         adj.r.squared,
         p.value,
         n)

resids <- unnest(lm_nest, resids)

resids |>
  ggplot(aes(w, resid, colour = trough_name)) +
  geom_point(aes(group = trough_name), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

model_summaries <- lm_nest |>
  unnest(glance) |>
  select(trough_name, adj.r.squared, p.value)

preds <- unnest(lm_nest, preds) |>
  select(trough_name, w, IP, pred) |>
  left_join(model_summaries, by = "trough_name")

preds |>
  ggplot() +
  geom_point(aes(w, IP), alpha = 0.5) +
  geom_line(aes(w, pred, linetype = "Model Fit"), colour = 'red', linetype = 'dashed') +
  facet_wrap(~trough_name) +
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
  group_by(trough_name) |>
  summarize(
    correlation = cor(w, IP, method = "pearson"),
    test_results = list(cor.test(w, IP, method = "pearson")),
    .groups = "drop"
  ) |>
  mutate(tidy_results = map(test_results, tidy)) |>
  unnest(tidy_results) |>
  select(trough_name, correlation, statistic, p.value, conf.low, conf.high) |>
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

# sig neg. correlation for all three

## T test between manually defined threshold

#  normality test passes but for consistancy with above will use wilcox.test
tree_ip_ttest <- w_ip |>
  filter(is.na(w) == F) |>
  mutate(group = factor(ifelse(w < w_th, 'light', 'heavy'),
                        levels = c('light', 'heavy')))

# Visualize the data
ggplot(tree_ip_ttest, aes(x = group, y = IP, fill = trough_name)) +
  geom_boxplot() +
  facet_wrap(~trough_name, scales = "free_y") +
  labs(x = "Canopy Snow Load Bin (mm)", y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

normality_test <- tree_ip_ttest |>
  group_by(trough_name, group) |>
  summarise(
    shapiro = list(shapiro.test(IP) %>% tidy())
  ) %>%
  unnest(cols = c(shapiro)) |>
  mutate(is_normal = ifelse(p.value > 0.05, T, F))

variance_test <- tree_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    levene_test = list(car::leveneTest(IP ~ group) %>% tidy())
  ) %>%
  unnest(cols = c(levene_test)) |>
  mutate(is_equal_variance = ifelse(p.value > 0.05, T, F))

## wilcox.test (are the medians of two groups significantly different) ----

tree_wcox_test <- tree_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == 'light'], IP[group == 'heavy'],
                              alternative = 'greater') %>% tidy()),
    median_low = median(IP[group == 'light'], na.rm = TRUE),
    median_high = median(IP[group == 'heavy'], na.rm = TRUE),
    n_samples_low = sum(!is.na(IP[group == 'light'])),
    n_samples_high = sum(!is.na(IP[group == 'heavy']))
  ) %>%
  unnest(cols = c(w_test)) %>%
  mutate(
    reject_null_hyp = ifelse(p.value < 0.05, 'yes', 'no'),
    test_type = "Snow Load 1",
    null_hypothesis = "Median IP (L < 10 mm) ≤ Median IP (L ≥ 10 mm)"
  ) %>%
  select(test_type, trough_name, null_hypothesis, p.value, n_samples_low, n_samples_high, median_low, median_high, reject_null_hyp)




## Wilcox test again for the initial small increase ----

#  normality test passes but for consistancy with above will use wilcox.test
tree_ip_ttest <- w_ip |>
  filter(!is.na(w), w < 10) |>
  mutate(group = factor(ifelse(w < 5, 'light', 'heavy'),
                        levels = c('light', 'heavy')))

tree_wcox_test2 <- tree_ip_ttest |>
  group_by(trough_name) |>
  summarise(
    w_test = list(wilcox.test(IP[group == 'light'], IP[group == 'heavy'],
                              alternative = 'less') %>% tidy()),
    median_low = median(IP[group == 'light'], na.rm = TRUE),
    median_high = median(IP[group == 'heavy'], na.rm = TRUE),
    n_samples_low = sum(!is.na(IP[group == 'light'])),
    n_samples_high = sum(!is.na(IP[group == 'heavy']))
  ) %>%
  unnest(cols = c(w_test)) %>%
  mutate(
    reject_null_hyp = ifelse(p.value < 0.05, 'yes', 'no'),
    test_type = "Snow Load 2",
    null_hypothesis = "Median IP (L < 5 mm) ≥ Median IP (5 mm ≤ L < 10 mm)"
  ) %>%
  select(test_type, trough_name, null_hypothesis, p.value, n_samples_low,
         n_samples_high, median_low, median_high, reject_null_hyp)

wcox_tests_out <- rbind(at_wcox_test, wind_wcox_test, tree_wcox_test, tree_wcox_test2) |>
  mutate(
    p.value = case_when(
      p.value < 0.001 ~ "< 0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  )

saveRDS(wcox_tests_out, 'data/lysimeter-data/processed/lysimter_hourly_avg_wilcox_stats.rds')
# Visualize the data
ggplot(tree_ip_ttest, aes(x = group, y = IP, fill = trough_name)) +
  geom_boxplot() +
  facet_wrap(~trough_name, scales = "free_y") +
  labs(x = "Canopy Snow Load Bin (mm)", y = "IP") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Kruskal-Wallis test ----
# Similar non-parametric test as the wilcox test but
# designed for determining if the median is significantly different between more
# than two groups

## SPARSE TROUGH ----
sparse_data <- q_tf_met_tree_agg |> filter(trough_name == 'sparse') |>
  mutate(
    Temp_group = ifelse(t > t_th, "warm", "cold"),
    Wind_group = ifelse(u > u_th, "windy", "calm"),
    Tree_group = ifelse(weighed_tree_canopy_load_mm > w_th, 'heavy', 'light'),
    group = interaction(Temp_group, Wind_group, Tree_group)
  )

kruskal.test(IP ~ group, data = sparse_data)

pairwise.wilcox.test(sparse_data$IP, sparse_data$group,
                     p.adjust.method = "bonferroni")

## MIXED TROUGH ----
mixed_data <- q_tf_met_tree_agg |> filter(trough_name == 'mixed') |>
  mutate(
    Temp_group = ifelse(t > t_th, "warm", "cold"),
    Wind_group = ifelse(u > u_th, "windy", "calm"),
    Tree_group = ifelse(weighed_tree_canopy_load_mm > w_th, 'heavy', 'light'),
    group = interaction(Temp_group, Wind_group, Tree_group)
  )

kruskal.test(IP ~ group, data = mixed_data)

pairwise.wilcox.test(mixed_data$IP, mixed_data$group,
                     p.adjust.method = "bonferroni")

## CLOSED TROUGH ----

closed_data <- q_tf_met_tree_agg |> filter(trough_name == 'closed') |>
  mutate(
    Temp_group = ifelse(t > t_th, "warm", "cold"),
    Wind_group = ifelse(u > u_th, "windy", "calm"),
    Tree_group = ifelse(weighed_tree_canopy_load_mm > w_th, 'heavy', 'light'),
    group = interaction(Temp_group, Wind_group, Tree_group)
  )

kruskal.test(IP ~ group, data = closed_data)

pairwise.wilcox.test(closed_data$IP, closed_data$group,
                     p.adjust.method = "bonferroni")
