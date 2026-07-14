## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_mxl_data(N = 2000, J = 4, seed = 1)
sim

## ----fit----------------------------------------------------------------------
fit <- run_mxlogit(
  data            = sim$data,
  id_col          = "id",
  alt_col         = "alt",
  choice_col      = "choice",
  covariate_cols  = c("x1", "x2"),  # fixed coefficients
  random_var_cols = c("w1", "w2"),  # random coefficients
  rc_correlation  = TRUE,           # estimate their full covariance
  S               = 100L,           # Halton draws per person
  draws           = "generate",     # generate draws on the fly (low memory)
  seed            = 7L,
  scale_vars      = "sd",           # condition the Hessian across blocks
  se_method       = "bhhh"
)
summary(fit)

## ----recovery-----------------------------------------------------------------
recovery_table(fit, sim$true_params)

## ----diversion----------------------------------------------------------------
elasticities(fit, elast_var = "x2")
diversion_ratios(fit, wrt_var = "x2")

# For a random-coefficient attribute the perturbation coordinate matters.
elasticities(fit, elast_var = "w2", is_random_coef = TRUE)
diversion_ratios(fit, wrt_var = "w2", is_random_coef = TRUE)

