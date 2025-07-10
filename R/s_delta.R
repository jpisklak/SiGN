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
#' @details
#' *Signal Discrimination:*
#'
#' The `SiGN()` function assumes that terminal links which consistently lead to
#' extinction (i.e., no primary reinforcement) are fully discriminable by the
#' organism. In other words, the model treats terminal links that do and do not
#' lead to terminal reinforcement as completely distinct stimuli.
#'
#' For example, suppose selecting Alternative A results in two equally probable
#' terminal links, both lasting 10 seconds—one always leads to reinforcement,
#' the other never does. The `SiGN()` function assumes these two links are
#' completely discriminable (e.g., by colour or some other cue).
#'
#' If the organism cannot discriminate between them, this should be reflected
#' in how the parameters are specified. In such a case, rather than setting
#' `tr_p_a1 = 1` and `tr_p_a2 = 0`, you would model the ambiguity by setting
#' `tr_p_a1 = 0.5` and `tr_p_a2 = 0.5`.
#'
#' *Temporal Units:*
#'
#' Conventionally, concurrent-chain paradigms have measured time in seconds.
#' The default value of `s_delta` is chosen on the basis of that convention.
#' However, given the temporal relatively of the SiGN model, if other units are
#' used, it may be prudent to adjust `s_delta` accordingly.
#'
#' @export
#' @references
#' Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2023).
#' Suboptimal choice: A review and quantification of the signal for good news
#' (SiGN) model. *Psychological Review*. *131*(1), 58-78.
#' \url{https://doi.org/10.1037/rev0000416}
#'
#' @seealso [choice_params()]
#'
#' @examples
#' s_delta(
#'   tl_dur = c(10, 10, 10, 0),
#'   tr_p = c(0, 1, 0.5, 1),
#'   s_delta = 1
#'   )
s_delta <- function(tl_dur, tr_p, s_delta){
  ifelse(tl_dur != 0 & tr_p == 0, s_delta, tl_dur)
}
