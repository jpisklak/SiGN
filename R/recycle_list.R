recycle_list <- function(lst) {
  max_len <- max(lengths(lst))
  lapply(lst, function(x) rep_len(x, max_len))
}
