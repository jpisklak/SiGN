fit_SiGN <- function(params, obs,
                     b = 1, k_r = 1, k_d = 1,
                     epsilon = 1e-4, ...) {
  # Throw error if obs is incorrect length
  if (max(lengths(params)) != length(obs)) {
    stop(sprintf(
      "Model parameters and observed values are different lengths: %d vs. %d",
      max(lengths(params)), length(obs)
    ))
  }

  if (b == 1 && k_r == 1 && k_d == 1) {
    message("\u2139 Default starting values (1, 1, 1) are being used. For better results, consider setting values that match the patterns in your data — poor starting points can trip up the fitting algorithm.")
  }

  # Storage list
  vals <- as.list(SiGN(params)$details)
  vals[1] <- NULL
  vals$cp_obs <- obs
  vals$cr_a <- pmax(vals$cr_a, epsilon)
  vals$cr_b <- pmax(vals$cr_b, epsilon)

  # Function for generalized version's model
  SiGN_gen <- function(b, k_r, k_d, r_a, r_b, cr_a, cr_b) {
    (b * (r_a^k_r * cr_a^k_d)) /
      (b * (r_a^k_r * cr_a^k_d) + r_b^k_r * cr_b^k_d)
  }

  fit <- minpack.lm::nlsLM(
    cp_obs ~ SiGN_gen(b, k_r, k_d, r_a, r_b, cr_a, cr_b),
    data = vals,
    start = list(b = b, k_r = k_r, k_d = k_d),
    lower = c(0, 0, 0),
    control = nls.lm.control(
      maxiter = 1000,
      ftol = 1e-8,
      ptol = 1e-8,
      maxfev = 5000
    )
  )

  # Store values
  vals$cp_fit <- predict(fit) # predictions
  vals$b <- summary(fit)$parameters[1, 1]
  vals$k_r <- summary(fit)$parameters[2, 1]
  vals$k_d <- summary(fit)$parameters[3, 1]

  # Return structured output
  output <- list(
    details = as.data.frame(vals),
    mod_info = fit
  )

  class(output) <- "SiGN_fit"
  return(output)
}

#' @export
print.SiGN_fit <- function(x, ...) {
  cat("$details\n")
  print(x$details)

  cat("\nUse `$mod_info` for additional nlsLM() info.\n")
  invisible(x)
}


