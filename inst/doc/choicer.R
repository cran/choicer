## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 4
)
options(digits = 4)

## ----setup--------------------------------------------------------------------
library(choicer)
set_num_threads(2) # set for CRAN compilation; raise this on your own machine

## ----data---------------------------------------------------------------------
data(mode_choice)
head(mode_choice, 4)

## ----fit----------------------------------------------------------------------
fit <- run_mnlogit(
  data           = mode_choice,
  id_col         = "id",
  alt_col        = "mode",
  choice_col     = "choice",
  covariate_cols = c("wait", "travel", "vcost")
)
summary(fit)

## ----shares-------------------------------------------------------------------
shares <- drop(predict(fit, type = "shares"))
names(shares) <- as.character(fit$alt_mapping[[2]])
round(shares, 3)

barplot(shares, col = "steelblue", ylab = "Predicted share",
        main = "Predicted mode shares")

## ----elast--------------------------------------------------------------------
elasticities(fit, elast_var = "vcost")

## ----diversion----------------------------------------------------------------
diversion_ratios(fit)

## ----wtp----------------------------------------------------------------------
wtp(fit, price_var = "vcost")

## ----counterfactual-----------------------------------------------------------
mc_cf <- mode_choice
mc_cf$vcost[mc_cf$mode == "train"] <- mc_cf$vcost[mc_cf$mode == "train"] * 0.75

shares_cf <- drop(predict(fit, type = "shares", newdata = mc_cf))
names(shares_cf) <- names(shares)

rbind(baseline = shares, counterfactual = shares_cf) |> round(3)

## ----welfare------------------------------------------------------------------
cs0 <- consumer_surplus(fit, price_var = "vcost")
cs1 <- consumer_surplus(fit, price_var = "vcost", newdata = mc_cf)

cs1$mean_cs - cs0$mean_cs # change in mean consumer surplus

