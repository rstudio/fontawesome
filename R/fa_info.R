#' Generate a Font Awesome `<i>` tag
#'
#' @description
#' The `fa_info()` function will display an interactive table that contains all
#' Font Awesome icons included in the package. The first column displays the
#' icon itself and the second provides the classification of icon style where
#' `"S"` is solid, `"R"` is regular, and `"B"` signifies that the icon is for a
#' brand.
#'
#' The `"Icon Name"` column provides the short name and `"Full Name"` is the
#' fully-qualified, unique name for the icon (incorporating the type of icon,
#' since icons may have multiple styles). Both the short name and the full name
#' are acceptable inputs to [fa()], [fa_i()], and [fa_png()]. In the case where
#' an icon has multiple styles and a short name is supplied to any of the
#' aforementioned functions, the solid (`"S"`) style will be used.
#'
#' @return Nothing is returned.
#'
#' @examples
#' if (interactive()) {
#'
#' # View a table of all Font Awesome icons in the package
#' fa_info()
#'
#' }
#'
#' @export
fa_info <- function() {

  # Define temporary directory for output
  www_dir <- tempfile("viewhtml")
  dir.create(www_dir)

  # Copy table contents and dependencies to temp dir
  file.copy(
    from = system.file("info_table", package = "fontawesome"),
    to = www_dir,
    recursive = TRUE
  )
  # Get path to HTML file
  html_tbl_path <- file.path(www_dir, "info_table/fontawesome_table.html")

  # View the HTML table
  rstudioapi::viewer(html_tbl_path)
}
