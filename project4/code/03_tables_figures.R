```{r}
# ============================================================
# 03_tables_figures.R (display-only version)
# Tables print to console, figures show in RStudio Plots panel
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ---- Read summary objects ----
overall_rates     <- readRDS("results/overall_rates.rds")
inference_metrics <- readRDS("results/inference_metrics.rds")
per_var_tpr       <- readRDS("results/per_var_tpr.rds")
selection_rates   <- readRDS("results/selection_rates.rds")
summary_master    <- read.csv("results/summary_tables.csv")

# ---- Method ordering and nice labels ----
METHOD_ORDER <- c("backward_p", "backward_aic", "backward_bic",
                  "lasso_min", "lasso_1se", "elnet_min", "elnet_1se")
METHOD_LABEL <- c(
  backward_p   = "Backward (p-value)",
  backward_aic = "Backward (AIC)",
  backward_bic = "Backward (BIC)",
  lasso_min    = "Lasso (lambda.min)",
  lasso_1se    = "Lasso (lambda.1se)",
  elnet_min    = "Elastic Net (lambda.min)",
  elnet_1se    = "Elastic Net (lambda.1se)"
)

scenario_label <- function(n, rho) sprintf("n=%d, rho=%.2f", n, rho)

theme_proj <- theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "grey95", color = NA),
        panel.grid.minor = element_blank())

# ============================================================
# TABLE 1: Master summary (42 rows)
# Note: TypeI is dropped because TypeI = FPR (redundant).
#       TypeII = 1 - TPR (also redundant) but kept for clarity.
# ============================================================
table1 <- summary_master %>%
  mutate(
    Scenario = sprintf("N=%d, rho=%.2f", n, rho),
    Method   = METHOD_LABEL[method]
  ) %>%
  transmute(
    Scenario,
    Method,
    TPR      = sprintf("%.3f", TPR_avg),
    FPR      = sprintf("%.3f", FPR_avg),       # = Type I error
    AnyFP    = sprintf("%.3f", FPR_fwer),
    Exact    = sprintf("%.3f", exact_recovery),
    ModelSz  = sprintf("%.2f", mean_model_size),
    Bias     = sprintf("%.4f", mean_bias_signal),
    Coverage = sprintf("%.3f", mean_cov_signal),
    TypeII   = sprintf("%.3f", type_II)
  ) %>%
  mutate(method_ord = factor(Method, levels = METHOD_LABEL[METHOD_ORDER])) %>%
  arrange(Scenario, method_ord) %>%
  select(-method_ord)

print(table1, n = 42)

# ============================================================
# TABLE 2: Per-variable TPR for X1..X5
# ============================================================
table2 <- per_var_tpr %>%
  mutate(
    Scenario   = sprintf("N=%d, rho=%.2f", n, rho),
    Method     = METHOD_LABEL[method],
    method_ord = factor(Method, levels = METHOD_LABEL[METHOD_ORDER])
  ) %>%
  select(Scenario, Method, method_ord, variable, selection_rate) %>%
  pivot_wider(names_from = variable, values_from = selection_rate) %>%
  arrange(Scenario, method_ord) %>%
  mutate(across(starts_with("X"), ~ sprintf("%.3f", .))) %>%
  select(-method_ord)

print(table2, n = 42)

# ============================================================
# FIGURE 1: TPR vs FPR trade-off scatter
# ============================================================
fig1_data <- overall_rates %>%
  mutate(scenario_lab = scenario_label(n, rho),
         scenario_lab = factor(scenario_lab,
                               levels = unique(scenario_lab[order(n, rho)])),
         method_lab   = METHOD_LABEL[method])

fig1 <- ggplot(fig1_data, aes(x = FPR_avg, y = TPR_avg,
                              color = method_lab, shape = method_lab)) +
  geom_point(size = 3, stroke = 1) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  scale_shape_manual(values = c(16, 17, 15, 1, 19, 2, 0)) +
  facet_wrap(~ scenario_lab, nrow = 2) +
  labs(title = "Figure 1: TPR vs FPR trade-off",
       x = "Average FPR (across 15 noise variables)",
       y = "Average TPR (across 5 signal variables)") +
  theme_proj +
  guides(color = guide_legend(nrow = 2))

print(fig1)

# ============================================================
# FIGURE 2: Per-variable TPR for X1..X5
# Simplified X axis: just "X1 ... X5"; betas explained in caption.
# ============================================================
fig2_data <- per_var_tpr %>%
  mutate(scenario_lab = scenario_label(n, rho),
         scenario_lab = factor(scenario_lab,
                               levels = unique(scenario_lab[order(n, rho)])),
         method_lab   = METHOD_LABEL[method],
         method_lab   = factor(method_lab, levels = METHOD_LABEL[METHOD_ORDER]),
         var_num      = as.integer(sub("X", "", variable)))

fig2 <- ggplot(fig2_data, aes(x = var_num, y = selection_rate,
                              color = method_lab, group = method_lab)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 1:5, labels = paste0("X", 1:5)) +
  scale_y_continuous(limits = c(0, 1), labels = percent_format(accuracy = 1)) +
  facet_wrap(~ scenario_lab, nrow = 2) +
  labs(title = "Figure 2: Per-variable TPR by signal strength",
       subtitle = "True beta: X1=0.17, X2=0.33, X3=0.50, X4=0.67, X5=0.83",
       x = "Signal variable (increasing true coefficient -->)",
       y = "Selection rate (TPR per variable)") +
  theme_proj +
  guides(color = guide_legend(nrow = 2))

print(fig2)

# ============================================================
# FIGURE 3: Family-wise FPR + Mean model size
# ============================================================
fig3_data <- overall_rates %>%
  mutate(scenario_lab = scenario_label(n, rho),
         scenario_lab = factor(scenario_lab,
                               levels = unique(scenario_lab[order(n, rho)])),
         method_lab   = METHOD_LABEL[method],
         method_lab   = factor(method_lab, levels = METHOD_LABEL[METHOD_ORDER]))

fig3_long <- fig3_data %>%
  select(scenario_lab, method_lab, FPR_fwer, mean_model_size) %>%
  pivot_longer(c(FPR_fwer, mean_model_size),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("FPR_fwer", "mean_model_size"),
                         labels = c("Family-wise FPR: P(>= 1 noise var)",
                                    "Mean # variables selected (truth = 5)")))

fig3 <- ggplot(fig3_long, aes(x = method_lab, y = value, fill = scenario_lab)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(data = data.frame(
                metric = factor("Mean # variables selected (truth = 5)",
                                levels = levels(fig3_long$metric)),
                yint = 5),
             aes(yintercept = yint), linetype = "dashed", color = "red") +
  facet_wrap(~ metric, scales = "free_y", nrow = 2) +
  labs(title = "Figure 3: Family-wise FPR and mean model size",
       x = NULL, y = NULL, fill = "Scenario") +
  theme_proj +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))

print(fig3)

# ============================================================
# FIGURE 4: Bias and Coverage for signal variables
# ============================================================
fig4_data <- inference_metrics %>%
  filter(is_signal) %>%
  mutate(scenario_lab = scenario_label(n, rho),
         scenario_lab = factor(scenario_lab,
                               levels = unique(scenario_lab[order(n, rho)])),
         method_lab   = METHOD_LABEL[method],
         method_lab   = factor(method_lab, levels = METHOD_LABEL[METHOD_ORDER]))

fig4_agg <- fig4_data %>%
  group_by(scenario_lab, method_lab) %>%
  summarise(mean_bias     = mean(bias, na.rm = TRUE),
            mean_coverage = mean(coverage, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_longer(c(mean_bias, mean_coverage),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("mean_bias", "mean_coverage"),
                         labels = c("Mean bias (signal vars, conditional on selection)",
                                    "Mean 95% CI coverage (signal vars)")))

fig4 <- ggplot(fig4_agg, aes(x = method_lab, y = value, fill = scenario_lab)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(data = data.frame(
                metric = factor(c("Mean bias (signal vars, conditional on selection)",
                                  "Mean 95% CI coverage (signal vars)"),
                                levels = levels(fig4_agg$metric)),
                yint = c(0, 0.95)),
             aes(yintercept = yint), linetype = "dashed", color = "red") +
  facet_wrap(~ metric, scales = "free_y", nrow = 2) +
  labs(title = "Figure 4: Bias and 95% CI coverage for signal variables",
       x = NULL, y = NULL, fill = "Scenario") +
  theme_proj +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(fill = guide_legend(nrow = 1))

print(fig4)
```
