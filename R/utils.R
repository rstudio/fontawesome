# @staticimports pkg:staticimports
#   s3_register

# nocov start

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_na <- function(x) {
  isTRUE(is.na(x))
}

# nocov end
