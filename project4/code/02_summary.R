```{r summary}
library(dplyr)
library(tidyr)

results <- readRDS("results/all_results.rds")

TRUE_BETA <- data.frame(
  variable  = paste0("X", 1:20),
  true_beta = c(c(0.5, 1, 1.5, 2, 2.5) / 3, rep(0, 15)),
  is_signal = c(rep(TRUE, 5), rep(FALSE, 15))
)

results <- left_join(results, TRUE_BETA, by = "variable")

# ============================================================
# 1. Per-variable selection rates
# ============================================================
selection_rates <- results %>%
  group_by(scenario, n, rho, method, variable, is_signal, true_beta) %>%
  summarise(n_reps = n(),
            selection_rate = mean(selected),
            .groups = "drop")

# ============================================================
# 2. Multiple TPR/FPR definitions
# ============================================================
# (a) Average TPR / FPR
avg_rates <- selection_rates %>%
  group_by(scenario, n, rho, method, is_signal) %>%
  summarise(rate = mean(selection_rate), .groups = "drop") %>%
  mutate(metric = ifelse(is_signal, "TPR_avg", "FPR_avg")) %>%
  select(-is_signal) %>%
  pivot_wider(names_from = metric, values_from = rate)

# (b) Family-wise FPR + exact recovery + model size
per_rep_summary <- results %>%
  group_by(scenario, n, rho, method, rep) %>%
  summarise(
    n_signal_selected = sum(selected[is_signal]),
    n_noise_selected  = sum(selected[!is_signal]),
    .groups = "drop"
  ) %>%
  mutate(
    any_fp        = n_noise_selected >= 1,
    exact_recover = (n_signal_selected == 5) & (n_noise_selected == 0)
  )

aggregate_rates <- per_rep_summary %>%
  group_by(scenario, n, rho, method) %>%
  summarise(
    FPR_fwer        = mean(any_fp),
    exact_recovery  = mean(exact_recover),
    mean_model_size = mean(n_signal_selected + n_noise_selected),
    .groups = "drop"
  )

overall_rates <- avg_rates %>%
  left_join(aggregate_rates, by = c("scenario", "n", "rho", "method")) %>%
  arrange(scenario, method)

# ============================================================
# 3. Per-variable TPR for X1..X5
# ============================================================
per_var_tpr <- selection_rates %>%
  filter(is_signal) %>%
  select(scenario, n, rho, method, variable, true_beta, selection_rate) %>%
  arrange(scenario, method, variable)

# ============================================================
# 4. Bias and 95% CI coverage (conditional on selection)
# ============================================================
inference_metrics <- results %>%
  filter(selected == 1) %>%
  group_by(scenario, n, rho, method, variable, is_signal, true_beta) %>%
  summarise(n_selected = n(),
            mean_estimate = mean(estimate, na.rm = TRUE),
            bias          = mean(estimate - true_beta, na.rm = TRUE),
            rmse          = sqrt(mean((estimate - true_beta)^2, na.rm = TRUE)),
            coverage      = mean(ci_lo <= true_beta & true_beta <= ci_hi,
                                 na.rm = TRUE),
            .groups = "drop")

# ============================================================
# 5. Master summary
#    Type I  = FPR_avg (unconditional)
#    Type II = 1 - TPR_avg
# ============================================================
summary_tables <- overall_rates %>%
  mutate(
    type_I  = FPR_avg,
    type_II = 1 - TPR_avg
  ) %>%
  left_join(
    inference_metrics %>%
      group_by(scenario, method) %>%
      summarise(
        mean_bias_signal = mean(bias[is_signal], na.rm = TRUE),
        mean_cov_signal  = mean(coverage[is_signal], na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("scenario", "method")
  )

# ============================================================
# 6. Save
# ============================================================
dir.create("results", showWarnings = FALSE)
saveRDS(selection_rates,   "results/selection_rates.rds")
saveRDS(overall_rates,     "results/overall_rates.rds")
saveRDS(inference_metrics, "results/inference_metrics.rds")
saveRDS(per_var_tpr,       "results/per_var_tpr.rds")
saveRDS(per_rep_summary,   "results/per_rep_summary.rds")
write.csv(summary_tables,  "results/summary_tables.csv", row.names = FALSE)

print(overall_rates)
print(summary_tables)
```
