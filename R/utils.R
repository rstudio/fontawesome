# nocov start

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

fa_write_file <- function(path,
                          lines,
                          append = FALSE,
                          line_ending = NULL) {

  stopifnot(is.character(path))
  stopifnot(is.character(lines))

  if (append) {
    file_mode <- "ab"
  } else {
    file_mode <- "wb"
  }

  # Create a file connection
  file_connection <- file(path, open = file_mode, encoding = "utf-8")

  on.exit(close(file_connection))

  # Obtain the appropriate line ending based on the platform
  if (.Platform$OS.type == "windows") {
    line_ending <- "\r\n"
  } else {
    line_ending <- "\n"
  }

  lines <- gsub("\r?\n", line_ending, lines)

  writeLines(
    text = enc2utf8(lines),
    con = file_connection,
    sep = line_ending,
    useBytes = TRUE
  )

  invisible(TRUE)
}

# nocov end
