#' @importFrom stats median optimize dbeta
#' @export
SiGN_eval <- function(observed, predicted, epsilon = 0.0001) {
  # Throw error if obs is incorrect length
  if (length(observed) != length(predicted)) {
    stop(sprintf(
      "Observed and predicted values are different lengths: %d vs. %d",
      length(observed), length(predicted)
    ))
  }


  residuals <- observed - predicted
  ss_res_fit <- sum(residuals^2)
  ss_tot_fit <- sum((observed - mean(observed))^2)

  # General Stats
  n = length(observed)
  rsq <- 1 - (ss_res_fit / ss_tot_fit)
  mean_bias <- mean(predicted - observed)
  rmse <- sqrt(mean(residuals^2))
  mean_ae <- mean(abs(residuals))
  med_ae <- median(abs(residuals))
  ccc_val <- ccc(predicted, observed)

  #Phi Calculation
  #-----------------------------------------------------------------------------
  # Continuity correction
  y <- pmin(pmax(observed, epsilon), 1 - epsilon)
  mu <- pmin(pmax(predicted, epsilon), 1 - epsilon)

  # Define negative log-likelihood
  neg_loglik <- function(phi, y, mu) {
    alpha <- mu * phi
    beta <- (1 - mu) * phi
    -sum(dbeta(y, alpha, beta, log = TRUE))
  }

  # create seperate function for log and negative log likelihood

  # Estimate phi
  optim_result <- optimize(neg_loglik, interval = c(1, 200), y = y, mu = mu)
  phi_est <- optim_result$minimum

  #-----------------------------------------------------------------------------





  stats_df <- data.frame(
    n = n,
    r_squared = rsq,
    mean_bias = mean_bias,
    rmse = rmse,
    mae = mean_ae,
    median_ae = med_ae,
    ccc = ccc_val
  )

  structure(
    list(summary = stats_df, residuals = residuals),
    class = "SiGN_eval"
  )

}

#' @export
print.SiGN_eval <- function(x, ...) {
  cat("$summary\n")
  print(x$summary)
  invisible(x)
  cat("\nUse `object$residuals` to access the residuals.\n")
}


