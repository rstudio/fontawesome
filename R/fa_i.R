#' Generate a Font Awesome `<i>` tag
#'
#' The `fa_i()` function creates a Font Awesome `<i>` tag and not an SVG as with
#' [fa()]. The primary use case for `fa_i()` is for legacy Shiny applications
#' that use the [shiny::icon()] function. This function is called within a
#' [shiny::icon()] call and all HTML dependencies to support icon generation are
#' hosted in the **fontawesome** package.
#'
#' @param name The name of the Font Awesome icon.
#' @param class Additional classes to customize the style of the icon (see the
#'   usage examples for details on supported styles).
#' @param ... Arguments passed to the `<i>` tag of [htmltools::tags].
#'
#' @return An icon element.
#'
#' @examples
#' if (interactive()) {
#'
#' # Create a Font Awesome icon object
#' fa_i(name = "r-project")
#'
#' }
#'
#' @export
fa_i <- function(name,
                 class = NULL,
                 ...) {

  prefix <- "fa"
  iconClass <- ""

  if (!is.null(name)) {
    prefix_class <- prefix
    if (prefix_class == "fa" && name %in% font_awesome_brands) {
      prefix_class <- "fab"
    }
    iconClass <- paste0(prefix_class, " ", prefix, "-", name)
  }

  if (!is.null(class)) {
    iconClass <- paste(iconClass, class)
  }

  icon_tag <-
    htmltools::tags$i(
      class = iconClass,
      role = "presentation",
      `aria-label` = paste(name, "icon"),
      ...
    )

  htmltools::htmlDependencies(icon_tag) <-
    htmltools::htmlDependency(
      name = "font-awesome",
      version = "5.13.0",
      src = "fontawesome",
      package = "fontawesome",
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    )

  htmltools::browsable(icon_tag)
}
