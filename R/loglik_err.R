#' Compute (Negative) Log-Likelihood for Beta Error Model
#'
#' Calculates the log-likelihood (or negative log-likelihood) of a set of
#' observed values under a Beta-distributed error model.
#'
#' @param observed A numeric vector of observed values (e.g., proportions),
#' each in the range `[0, 1]`.
#' @param predicted A numeric vector of predicted values from the model, also
#' in `[0, 1]`.
#' @param phi A scalar or numeric vector specifying the precision
#' (concentration) parameter(s) of the Beta distribution. If a vector, it must
#' be the same length as `observed` and `predicted`.
#' @param neg Logical. If `TRUE` (default), returns the negative
#' log-likelihood (useful for optimisation). If `FALSE`, returns the
#' log-likelihood.
#' @param epsilon A small numeric value used to constrain `observed` and
#' `predicted` away from 0 and 1, ensuring numerical stability (default
#' is `1e-4`).
#'
#' @returns A single numeric value: the negative log-likelihood or
#' log-likelihood of the data under the Beta error model.
#'
#' @details
#' This function is useful for evaluating model fit when prediction errors are
#' assumed to follow a Beta distribution, which is appropriate for bounded
#' continuous data such as proportions. The function computes the
#' log-likelihood by treating each observed value as a draw from a Beta
#' distribution with mean equal to the model's prediction and concentration
#' controlled by `phi`.
#'
#' @examples
#' # Scalar phi
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = 20)
#'
#' # Vectorised phi
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = c(15, 25))
#'
#' # Return log-likelihood instead of negative log-likelihood
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = 20, neg = FALSE)
#'
#' @export
#-------------------------------------------------------------------------------
loglik_err <- function(observed, predicted, phi, neg = TRUE, epsilon = 1e-4) {
  # Check lengths
  n <- length(observed)

  if (!(length(predicted) == n && (length(phi) == 1 || length(phi) == n))) {
    stop("`phi` must be either length 1 or the same length as `observed` and `predicted`.")
  }

  if (anyNA(c(observed, predicted, phi))) {
    stop("Inputs must not contain NA values.")
  }

  # Continuity correction
  y <- pmin(pmax(observed, epsilon), 1 - epsilon)
  mu <- pmin(pmax(predicted, epsilon), 1 - epsilon)

  # Recycle phi if needed
  if (length(phi) == 1) {
    phi <- rep(phi, n)
  }

  # Compute shape parameters
  alpha_shape <- mu * phi
  beta_shape  <- (1 - mu) * phi

  # Compute log-likelihood
  log_lik <- sum(dbeta(y, alpha_shape, beta_shape, log = TRUE))

  if (neg) {
    return(-log_lik)
  } else {
    return(log_lik)
  }
}
