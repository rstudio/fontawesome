# nocov start

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

extract_group <- function(x, pattern, which = 1) {
  matches <- regmatches(x, regexec(pattern, x))
  vapply(matches, function(x) x[which + 1], character(1))
}

# nocov end
