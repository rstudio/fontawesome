fa_dependency_obj <- NULL

#' Use a Font Awesome `html_dependency`
#'
#' The `fa_html_dependency()` function adds a `html_dependency` object into a
#' Shiny or R Markdown context. This allows for the direct use of `<i>` tags
#' that refer to Font Awesome icons without having to use the [fa_i()] to create
#' these tags and also add the `html_dependency` to the document.
#'
#' The `html_dependency` object is created internally with the following
#' invocation:
#'
#' ```
#' htmltools::htmlDependency(
#'   name = "font-awesome",
#'   version = fa_version,
#'   src = "fontawesome",
#'   package = "fontawesome",
#'   stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
#' )
#' ```
#'
#' The `fa_version` object is an internal object that provides the released
#' version number for the Font Awesome icons. This can be inspected by using
#' `fa_metadata()$version`.
#'
#' @return An `html_dependency` object.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a Font Awesome `html_dependency`
#' fa_html_dependency()
#'
#' }
#'
#' @export
fa_html_dependency <- function() {
  fa_dependency_obj
}
