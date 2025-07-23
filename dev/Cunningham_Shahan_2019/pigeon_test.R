library(tidyverse)
source("dev/Cunningham_Shahan_2019/fit_SiGN.R")

params <- do.call(choice_params, as.list(subopt_avian[9:24]))

mod <- fit_SiGN(params, subopt_avian$cp,
                  b = 1, k_r = 1, k_d = 1)

preds <- mod$details
mod$mod_info
summary(mod$mod_info)


# Stats
# Residual sum of squares
ss_res_fit <- sum((preds$cp_obs - preds$cp_fit)^2)

# Total sum of squares
ss_tot_fit <- sum((preds$cp_obs - mean(preds$cp_obs))^2)

# R-squared
r_sq_fit <- 1 - (ss_res_fit / ss_tot_fit)

# Plot of Results
#-------------------------------------------------------------------------------
# Draw Plot
ggplot(preds, aes(x = cp_fit, y = cp_obs)) +
  geom_abline(intercept = 0, slope = 1, linetype = 3) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")


reg <- lm(preds$cp_obs ~ preds$cp_fit)
r_sq <- summary(reg)$r.squared
