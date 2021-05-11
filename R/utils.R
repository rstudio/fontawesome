# nocov start

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

extract_group <- function(x, pattern, which = 1) {
  matches <- regmatches(x, regexec(pattern, x))
  na.omit(sapply(matches, "[", which + 1))
}

# nocov end
