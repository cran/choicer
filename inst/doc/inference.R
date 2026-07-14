## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_hmnl_data(N = 500, T = 8, J = 4, seed = 3)

## ----fit----------------------------------------------------------------------
fit <- run_mnlogit(
  data                   = sim$data,
  id_col                 = "task",
  alt_col                = "alt",
  choice_col             = "choice",
  covariate_cols         = c("x1", "x2"),
  cluster_col            = "pid",
  include_outside_option = TRUE
)
summary(fit)

## ----compare------------------------------------------------------------------
se_of <- function(V) sqrt(diag(V))
tab <- cbind(
  hessian = se_of(vcov(fit, type = "hessian")),
  bhhh    = se_of(vcov(fit, type = "bhhh")),
  robust  = se_of(vcov(fit, type = "robust")),
  cluster = se_of(vcov(fit, type = "cluster"))
)
round(tab, 4)
round(tab[, "cluster"] / tab[, "hessian"], 2)

## ----posthoc------------------------------------------------------------------
fit0 <- run_mnlogit(
  data                   = sim$data,
  id_col                 = "task",
  alt_col                = "alt",
  choice_col             = "choice",
  covariate_cols         = c("x1", "x2"),
  include_outside_option = TRUE
)

# one cluster label per choice situation, named by choice-situation id
task_person <- unique(sim$data[, c("task", "pid")])
cl <- setNames(task_person$pid, task_person$task)
se_of(vcov(fit0, type = "cluster", cluster = cl))

