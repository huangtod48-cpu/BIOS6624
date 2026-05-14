```{r test}
# ============================================================
# Quick test (5 reps, scenario 1a). ~30 seconds.
# ============================================================

set.seed(20260512)

test_results <- purrr::map_dfr(1:5, function(i) {
  one_rep(rep_id = i, scen_id = "1a", n = 250, rho = 0)
})

# Quick TPR / FPR snapshot
test_results$is_signal <- as.integer(sub("X", "", test_results$variable)) <= 5

test_snapshot <- test_results %>%
  group_by(method, is_signal) %>%
  summarise(rate = mean(selected), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = is_signal, values_from = rate,
                     names_prefix = "signal_")
names(test_snapshot) <- c("method", "FPR", "TPR")

print(test_snapshot)

# Estimate sanity check for X1..X5
test_sig <- test_results %>%
  filter(selected == 1, variable %in% paste0("X", 1:5)) %>%
  group_by(variable) %>%
  summarise(n_selected = n(),
            mean_estimate = mean(estimate, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(true_beta = c(0.5, 1, 1.5, 2, 2.5) / 3)

print(test_sig)
```
