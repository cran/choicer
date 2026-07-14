## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_nl_data(N = 4000, seed = 1)
sim

## ----fit----------------------------------------------------------------------
fit <- run_nestlogit(
  data                   = sim$data,
  id_col                 = "id",
  alt_col                = "j",
  choice_col             = "choice",
  covariate_cols         = c("X", "W"),
  nest_col               = "nest",
  use_asc                = TRUE,
  include_outside_option = TRUE,
  outside_opt_label      = 0L
)
summary(fit)

## ----recovery-----------------------------------------------------------------
recovery_table(fit, sim$true_params)

## ----elast--------------------------------------------------------------------
elasticities(fit, elast_var = "W")
diversion_ratios(fit)

## ----blp----------------------------------------------------------------------
target_shares <- predict(fit, type = "shares")
head(blp(fit, target_shares, damping = 0.5))

