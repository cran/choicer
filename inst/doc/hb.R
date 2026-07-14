## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6.5, fig.height = 4)
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2)

## ----sim----------------------------------------------------------------------
sim <- simulate_hmnl_data(N = 250, T = 6, J = 6, seed = 42)
head(sim$data, 8)

## ----fit----------------------------------------------------------------------
set.seed(99)
fit <- run_hmnlogit(
  data               = sim$data,
  id_col             = "task",
  alt_col            = "alt",
  choice_col         = "choice",
  covariate_cols     = c("x1", "x2"),
  person_col         = "pid",
  alt_covariate_cols = "z1",
  chains             = 2,
  mcmc               = list(R = 24000, burn = 4000, thin = 8)
)
summary(fit)

## ----trace-b------------------------------------------------------------------
traceplot(fit, block = "b")

## ----trace-delta--------------------------------------------------------------
traceplot(fit, block = "delta")

## ----diagnostics--------------------------------------------------------------
b_chains <- lapply(fit$chains, function(ch) ch$b)
rhat(b_chains, rank = TRUE)
ess(b_chains)
mcse(b_chains)

## ----recovery-----------------------------------------------------------------
recovery_table(fit, sim)

## ----wtp----------------------------------------------------------------------
wtp(fit, price_var = "x2")

## ----shares-------------------------------------------------------------------
set.seed(7)
predict(fit, n_draws = 200)

## ----ppc----------------------------------------------------------------------
ppc_shares(fit, n_draws = 200)

## ----counterfactual-----------------------------------------------------------
cf <- sim$data
cf$x2[cf$alt == 1] <- cf$x2[cf$alt == 1] - 0.25

set.seed(7)
base_shares <- predict(fit, n_draws = 200)
set.seed(7)
cf_shares <- predict(fit, newdata = cf, n_draws = 200)

data.frame(
  alternative    = base_shares$alternative,
  baseline       = round(base_shares$share, 3),
  counterfactual = round(cf_shares$share, 3)
)

## ----welfare------------------------------------------------------------------
set.seed(7)
cs <- consumer_surplus(fit, price_var = "x2", newdata = cf, n_draws = 200)
attr(cs, "cv")

## ----entry--------------------------------------------------------------------
entrant <- sim$data[sim$data$alt == 1, ]
entrant$alt <- 99L
entrant$z1 <- 0.4
entrant$choice <- 0L
entry_data <- rbind(sim$data, entrant)

set.seed(7)
predict(fit, newdata = entry_data, n_draws = 200)

## ----hmnp---------------------------------------------------------------------
simp <- simulate_hmnp_data(N = 250, T = 5, J = 6, seed = 42)

set.seed(99)
fitp <- run_hmnprobit(
  data               = simp$data,
  id_col             = "task",
  alt_col            = "alt",
  choice_col         = "choice",
  covariate_cols     = c("x1", "x2"),
  person_col         = "pid",
  alt_covariate_cols = "z1",
  chains             = 2,
  mcmc               = list(R = 30000, burn = 5000, thin = 10)
)
summary(fitp)

## ----scale--------------------------------------------------------------------
rbind(
  probit         = coef(fitp),
  "logit scale"  = coef(fitp) * pi / sqrt(6)
)

