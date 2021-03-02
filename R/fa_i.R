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
#' @param htmlDependency Provides an opportunity to use a custom
#'   `html_dependency` object (created via a call to
#'   [htmltools::htmlDependency()]) instead of one supplied by the function
#'   (which uses Font Awesome's free assets and are bundled in the package). A
#'   custom `html_dependency` object is useful when you have paid icons from
#'   Font Awesome or would otherwise like to customize exactly which icon assets
#'   are used (e.g., woff, woff2, eot, etc.). By default, this is `NULL` where
#'   the function interally generates an `html_dependency`.
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
                 htmlDependency = NULL,
                 ...) {

  prefix <- "fa"
  iconClass <- ""

  # Determine if the `name` is a Font Awesome v4
  # icon name and provide a warning
  if (name %in% fa_tbl$v4_name && !(name %in% fa_tbl$name)) {

    # Obtain the version 5 `name` and `full_name`
    # for messaging purposes
    v5_name <- fa_tbl[fa_tbl$v4_name == name, ][1, "name"]
    v5_name_full <- fa_tbl[fa_tbl$v4_name == name, ][1, "full_name"]

    # Warn that the v4 icon name should be changed to a v5 one
    warning(
      "The `name` provided ('", name ,"') is deprecated in Font Awesome v5:\n",
      "* please consider using '", v5_name, "' or '", v5_name_full, "' instead",
      call. = FALSE
    )
  }

  # Provide a warning if the icon name can't be resolved, but
  # only do this if the user isn't supplying a custom `html-dependency`
  # (which is the common scenario)
  if (is.null(htmlDependency) &&
      !(name %in% fa_tbl$full_name) &&
      !(name %in% fa_tbl$name) &&
      !(name %in% fa_tbl$v4_name)
  ) {
    warning("This Font Awesome icon ('", name, "') does not exist", call. = FALSE)
    make_browsable <- FALSE
  } else {
    make_browsable <- TRUE
  }

  prefix_class <- prefix

  font_awesome_brands <-
    unique(fa_tbl$name[grepl("fab ", fa_tbl$full_name)])

  if (prefix_class == "fa" && name %in% font_awesome_brands) {
    prefix_class <- "fab"
  }

  iconClass <- paste0(prefix_class, " ", prefix, "-", name)

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

  if (!is.null(htmlDependency)) {

    if (!inherits(htmlDependency, "html_dependency")) {
      # Stop the function if the object isn't actually an `html_dependency`
      stop(
        "The object supplied to `htmlDependency` must be an object ",
        "of class `html_dependency`:\n",
        "* Use `htmltools::htmlDependency() to generate the object\n",
        "* Ensure that version number is set higher than any other dependency of the same type",
        call. = FALSE
      )
    }

    htmltools::htmlDependencies(icon_tag) <- htmlDependency

  } else {

    htmltools::htmlDependencies(icon_tag) <-
      htmltools::htmlDependency(
        name = "font-awesome",
        version = fa_version,
        src = "fontawesome",
        package = "fontawesome",
        stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
      )
  }

  if (make_browsable) {
    htmltools::browsable(icon_tag)
  }
}
