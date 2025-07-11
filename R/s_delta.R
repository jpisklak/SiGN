#' Terminal Link Duration Adjustment for S- Signals
#'
#' Adjusts the terminal link duration for signalled extinction.
#'
#' @param tl_dur Numeric vector. Duration of the terminal link.
#' @param tr_p Numeric vector. Probability of terminal reinforcement following
#' the terminal link.
#' @param s_delta Numeric scalar or vector. Duration of the S- signal to use
#' when terminal reinforcement probability is zero.
#'
#' @returns A numeric vector where elements of `tl_dur` are replaced by
#' `s_delta` if the terminal link has a non-zero duration and a
#' reinforcement probability of zero.
#'
#' @seealso [choice_params()]
#'
#' @examples
#' s_delta(
#'   tl_dur = c(10, 10, 10, 0),
#'   tr_p = c(0, 1, 0.5, 1),
#'   s_delta = 1
#'   )
#' @export
#-------------------------------------------------------------------------------
s_delta <- function(tl_dur, tr_p, s_delta){
  ifelse(tl_dur != 0 & tr_p == 0, s_delta, tl_dur)
}
