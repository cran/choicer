## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
library(data.table)
set_num_threads(2)

## ----population---------------------------------------------------------------
sim <- simulate_mxl_data(
  N               = 3000,
  J               = 4,
  Sigma           = diag(c(1.0, 1.5)),  # two uncorrelated random coefficients
  seed            = 11,
  outside_option  = FALSE,
  vary_choice_set = FALSE
)

pop <- as.data.table(sim$data)
Q <- prop.table(table(pop[choice == 1, alt]))
round(Q, 3)

## ----sample-------------------------------------------------------------------
cb <- sample_by_choice(
  pop,
  id_col     = "id",
  alt_col    = "alt",
  choice_col = "choice",
  n_per_alt  = 300L,
  seed       = 12L
)

strata <- sort(names(attr(cb, "Q")))
rbind(
  population = attr(cb, "Q")[strata],
  sample     = attr(cb, "H")[strata]
) |> round(3)

cb[choice == 1, .(id, chosen_alt = alt, .wesml_weight)][1:8]

## ----fit----------------------------------------------------------------------
common <- list(
  data            = cb,
  id_col          = "id",
  alt_col         = "alt",
  choice_col      = "choice",
  covariate_cols  = c("x1", "x2"),  # fixed coefficients
  random_var_cols = c("w1", "w2"),  # random coefficients
  S               = 100L,
  draws           = "generate",
  seed            = 7L,
  scale_vars      = "sd"
)

# sample_by_choice() records WESML provenance, and choicer deliberately applies
# its attached weights automatically. Strip that provenance on a copy to create
# the deliberately misspecified unweighted benchmark.
cb_unweighted <- copy(cb)
attr(cb_unweighted, "choice_sampling") <- NULL
common_unweighted <- common
common_unweighted$data <- cb_unweighted

fit_unweighted <- do.call(
  run_mxlogit,
  c(common_unweighted, list(se_method = "bhhh"))
)

fit_wesml <- do.call(run_mxlogit, c(common, list(
  weights_col = ".wesml_weight",
  se_method   = "sandwich"
)))

## ----coef---------------------------------------------------------------------
round(cbind(
  unweighted = coef(fit_unweighted),
  wesml      = coef(fit_wesml)
), 3)

## ----shares-------------------------------------------------------------------
share_compare <- rbind(
  population = as.numeric(Q),
  wesml      = drop(predict(fit_wesml, type = "shares")),
  unweighted = drop(predict(fit_unweighted, type = "shares"))
)
colnames(share_compare) <- names(Q)
round(share_compare, 3)

## ----se-----------------------------------------------------------------------
summary(fit_wesml)

## ----attach-------------------------------------------------------------------
cb2 <- copy(cb)
cb2[, .wesml_weight := NULL]

cb2 <- wesml_weights(
  cb2,
  id_col     = "id",
  alt_col    = "alt",
  choice_col = "choice",
  Q          = attr(cb, "Q"),
  attach     = TRUE
)

attr(cb2, "choice_sampling")

## ----mode-choice, eval = FALSE------------------------------------------------
# data(mode_choice)
# 
# mc <- wesml_weights(
#   mode_choice,
#   id_col     = "id",
#   alt_col    = "mode",
#   choice_col = "choice",
#   Q          = c(air = Q_air, train = Q_train, bus = Q_bus, car = Q_car),
#   attach     = TRUE
# )
# 
# fit_w <- run_mnlogit(
#   data           = mc,
#   id_col         = "id",
#   alt_col        = "mode",
#   choice_col     = "choice",
#   covariate_cols = c("wait", "travel", "vcost"),
#   weights_col    = ".wesml_weight",
#   se_method      = "sandwich"
# )

