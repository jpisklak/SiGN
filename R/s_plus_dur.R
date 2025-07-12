#' Duration of the S+ Terminal Link Stimulus
#'
#' Identifies the duration of the \eqn{S^{+}} terminal link stimulus, defined as
#' the link with the higher reinforcement rate, from two possible terminal
#' links on an alternative.
#'
#' @param sig Logical. Indicates whether the terminal link stimuli are assumed
#' to be discriminable. See [sig_check].
#' @param tl_dur1 Numeric. Duration of the first terminal link.
#' @param tl_dur2 Numeric. Duration of the second terminal link.
#' @param tr_p1 Numeric. Probability of terminal (primary/unconditional)
#' reinforcement following the first terminal link.
#' @param tr_p2 Numeric. Probability of terminal (primary/unconditional)
#' reinforcement following the second terminal link.
#'
#' @returns A numeric value giving the duration of the \eqn{S^{+}} stimulus, or `NA_real_` if `sig` is `FALSE`.
#'
#' @details
#' The function compares reinforcement rates (i.e., probability of
#' reinforcement per unit of time) between the two terminal links. If the
#' stimuli are signalled (`sig = TRUE`), the terminal link with the higher
#' reinforcement rate is assumed to function as \eqn{S^{+}}, and its duration
#' is returned. If \code{sig = FALSE}, the function returns `NA_real_`, as no
#' discriminable \eqn{S^{+}} stimulus is assumed to exist.
#'
#' @examples
#' s_plus_dur(TRUE, tl_dur1 = 10, tl_dur2 = 20, tr_p1 = 0.5, tr_p2 = 0.3)
#' s_plus_dur(FALSE, 10, 20, 0.5, 0.3)
#' @export
#-------------------------------------------------------------------------------
s_plus_dur <- function(sig, tl_dur1, tl_dur2, tr_p1, tr_p2) {
  invalid <- (tr_p1 == 0 & tr_p2 == 0) | (tl_dur1 == 0 | tl_dur2 == 0)
  pt1 <- tr_p1 / tl_dur1
  pt2 <- tr_p2 / tl_dur2

  result <- ifelse(pt1 > pt2, tl_dur1, tl_dur2)
  result[invalid | !sig] <- NA_real_
  result
}
