fit_SiGN <- function(params, obs){

  # Throw error if obs is incorrect length
  if (max(lengths(params)) != length(obs)) {
    stop(
      paste0(
        "Model parameters and observed values are different lengths:",
         max(lengths(params)," vs. ", length(obs))
      )
    )
  }

  # Gen a priori values
  vals <- SiGN(params)$details

  fit <- minpack.lm::nlsLM(

    obs ~ (b * (vals$r_a^k_r * exp(vals$cr_a * k_d))) /
         ((b * (vals$r_a^k_r * exp(vals$cr_a * k_d))) +
          (     vals$r_b^k_r * exp(vals$cr_b * k_d))),
    data = preds,
    start = list(b = 1, s_r = 1, s_cr = 1),
    lower = c(0, 0, 0)
  )





}
