```{r helpers}
# ============================================================
# Helpers - All function definitions for Project 4
# ============================================================

library(MASS)
library(glmnet)
library(future)
library(furrr)
library(dplyr)
library(purrr)

# ---- Parameters ----
P            <- 20
BETA_TRUE    <- c(c(0.5, 1, 1.5, 2, 2.5) / 3, rep(0, 15))
SIGMA        <- 1
ALPHA        <- 0.05

# 6 scenarios: n in {250, 500} x rho in {0, 0.35, 0.7}
SCENARIOS <- data.frame(
  id  = c("1a", "1b", "1c", "2a", "2b", "2c"),
  n   = c(250,  250,  250,  500,  500,  500),
  rho = c(0.0,  0.35, 0.7,  0.0,  0.35, 0.7)
)

# ---- Data generation ----
# Equivalent to hdrm::genData with corr = "exchangeable"
generate_data <- function(n, rho, p = P, beta = BETA_TRUE, sigma = SIGMA) {
  if (rho == 0) {
    X <- matrix(rnorm(n * p), n, p)
  } else {
    Sigma <- matrix(rho, p, p); diag(Sigma) <- 1
    X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  }
  colnames(X) <- paste0("X", seq_len(p))
  y <- as.numeric(X %*% beta + rnorm(n, 0, sigma))
  list(X = X, y = y)
}

# ---- Helpers ----
refit_on_selected <- function(df, vars_kept) {
  if (length(vars_kept) == 0) return(lm(y ~ 1, data = df))
  rhs <- paste(paste0("X", vars_kept), collapse = " + ")
  lm(as.formula(paste("y ~", rhs)), data = df)
}

extract_lm_results <- function(fit_lm, vars_kept, p = P) {
  out <- data.frame(
    variable = paste0("X", seq_len(p)),
    selected = as.integer(seq_len(p) %in% vars_kept),
    estimate = NA_real_, se = NA_real_,
    ci_lo = NA_real_, ci_hi = NA_real_, p_value = NA_real_
  )
  if (length(vars_kept) > 0) {
    cf <- summary(fit_lm)$coefficients
    keep_rows <- rownames(cf) != "(Intercept)"
    cf <- cf[keep_rows, , drop = FALSE]
    ci <- confint(fit_lm)[keep_rows, , drop = FALSE]
    for (i in seq_len(nrow(cf))) {
      r <- which(out$variable == rownames(cf)[i])
      out$estimate[r] <- cf[i, "Estimate"]
      out$se[r]       <- cf[i, "Std. Error"]
      out$ci_lo[r]    <- ci[i, 1]
      out$ci_hi[r]    <- ci[i, 2]
      out$p_value[r]  <- cf[i, "Pr(>|t|)"]
    }
  }
  out
}

# ---- Method functions ----
fit_backward_p <- function(X, y, alpha = ALPHA) {
  df <- data.frame(y = y, X)
  fit <- lm(y ~ ., data = df)
  current_vars <- seq_len(ncol(X))
  repeat {
    if (length(current_vars) == 0) break
    s <- summary(fit)$coefficients
    s <- s[rownames(s) != "(Intercept)", , drop = FALSE]
    pvals <- s[, "Pr(>|t|)"]
    worst <- which.max(pvals)
    if (pvals[worst] < alpha) break
    current_vars <- setdiff(current_vars,
                            as.integer(sub("X", "", rownames(s)[worst])))
    if (length(current_vars) == 0) { fit <- lm(y ~ 1, data = df); break }
    rhs <- paste(paste0("X", current_vars), collapse = " + ")
    fit <- lm(as.formula(paste("y ~", rhs)), data = df)
  }
  extract_lm_results(fit, current_vars)
}

fit_backward_aic <- function(X, y) {
  df <- data.frame(y = y, X)
  fit <- step(lm(y ~ ., data = df), direction = "backward", trace = 0, k = 2)
  vars_kept <- as.integer(sub("X", "",
                              setdiff(names(coef(fit)), "(Intercept)")))
  extract_lm_results(refit_on_selected(df, vars_kept), vars_kept)
}

fit_backward_bic <- function(X, y) {
  n <- length(y)
  df <- data.frame(y = y, X)
  fit <- step(lm(y ~ ., data = df), direction = "backward",
              trace = 0, k = log(n))
  vars_kept <- as.integer(sub("X", "",
                              setdiff(names(coef(fit)), "(Intercept)")))
  extract_lm_results(refit_on_selected(df, vars_kept), vars_kept)
}

fit_penalized <- function(X, y, alpha_val, lambda_choice) {
  cvfit <- cv.glmnet(X, y, alpha = alpha_val)
  b <- as.vector(coef(cvfit, s = lambda_choice))[-1]
  vars_kept <- which(b != 0)
  extract_lm_results(
    refit_on_selected(data.frame(y = y, X), vars_kept),
    vars_kept
  )
}

# ---- One simulation rep (7 methods x 20 vars = 140 rows) ----
one_rep <- function(rep_id, scen_id, n, rho) {
  dat <- generate_data(n = n, rho = rho)
  X <- dat$X; y <- dat$y

  methods <- list(
    backward_p   = function() fit_backward_p(X, y),
    backward_aic = function() fit_backward_aic(X, y),
    backward_bic = function() fit_backward_bic(X, y),
    lasso_min    = function() fit_penalized(X, y, 1,   "lambda.min"),
    lasso_1se    = function() fit_penalized(X, y, 1,   "lambda.1se"),
    elnet_min    = function() fit_penalized(X, y, 0.5, "lambda.min"),
    elnet_1se    = function() fit_penalized(X, y, 0.5, "lambda.1se")
  )

  res <- purrr::map_dfr(names(methods), function(m) {
    out <- tryCatch(methods[[m]](), error = function(e) NULL)
    if (is.null(out)) return(NULL)
    out$method <- m
    out
  })

  res$rep <- rep_id
  res$scenario <- scen_id
  res$n <- n
  res$rho <- rho
  res
}

# ---- Run one scenario in parallel ----
run_scenario <- function(scen_id, n, rho, n_reps) {
  future_map_dfr(
    seq_len(n_reps),
    function(i) one_rep(i, scen_id, n, rho),
    .options = furrr_options(seed = TRUE)
  )
}
```
