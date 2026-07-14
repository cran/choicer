## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6.5, fig.height = 4)
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_mnp_data(N = 2000, J = 3, seed = 1)
sim

## ----fit----------------------------------------------------------------------
set.seed(3)
fit <- run_mnprobit(
  data           = sim$data,
  id_col         = "id",
  alt_col        = "alt",
  choice_col     = "choice",
  covariate_cols = c("x1", "x2"),
  mcmc           = list(R = 4000, burn = 1000, thin = 2)
)
summary(fit)

## ----recovery-----------------------------------------------------------------
recovery_table(fit, sim$true_params)

## ----trace--------------------------------------------------------------------
beta_draws <- fit$draws$beta
plot(beta_draws[, "x2"], type = "l", col = "steelblue",
     xlab = "iteration", ylab = expression(beta[x2]),
     main = "Posterior trace: price coefficient")
abline(h = sim$true_params$beta[2], col = "red", lwd = 2)

## ----rhat---------------------------------------------------------------------
rhat(fit$draws$beta)
ess(fit$draws$beta)
mcse(fit$draws$beta)

