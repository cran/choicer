## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_mnl_data(N = 2000, J = 4, seed = 1)
sim

## ----fit----------------------------------------------------------------------
fit <- run_mnlogit(
  data           = sim$data,
  id_col         = "id",
  alt_col        = "alt",
  choice_col     = "choice",
  covariate_cols = c("x1", "x2")
)
summary(fit)

## ----recovery-----------------------------------------------------------------
recovery_table(fit, sim$true_params)

## ----post---------------------------------------------------------------------
predict(fit, type = "shares")        # aggregate fitted shares in these data
elasticities(fit, elast_var = "x2")  # own- and cross-price elasticities
diversion_ratios(fit)                # where demand goes
wtp(fit, price_var = "x2")           # willingness to pay, with delta-method SEs
gof(fit)                             # McFadden R2 and hit rate

