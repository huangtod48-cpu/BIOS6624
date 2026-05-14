```{r simulation}
# ============================================================
# Full simulation: 6 scenarios x 10000 reps x 7 methods
# Expected runtime: ~45-90 min depending on cores
# Output: results/all_results.rds
# ============================================================

N_REPS <- 10000

plan(multisession, workers = max(1, parallel::detectCores() - 1))

set.seed(20260512)

all_results <- purrr::map_dfr(seq_len(nrow(SCENARIOS)), function(k) {
  scen <- SCENARIOS[k, ]
  run_scenario(scen$id, scen$n, scen$rho, n_reps = N_REPS)
})

plan(sequential)

dir.create("results", showWarnings = FALSE)
saveRDS(all_results, "results/all_results.rds")
```
