#' Generate a Font Awesome `<i>` tag
#'
#' The `fa_i()` function creates a Font Awesome `<i>` tag and not an SVG as with
#' [fa()]. The primary use case for `fa_i()` is for legacy Shiny applications
#' that use the `shiny::icon()` function. This function is called within a
#' `shiny::icon()` call and all HTML dependencies to support icon generation are
#' hosted in the **fontawesome** package.
#'
#' @param name The name of the Font Awesome icon. This could be as a short name
#'   (e.g., `"npm"`, `"drum"`, etc.), or, a full name (e.g., `"fab fa-npm"`,
#'   `"fas fa-drum"`, etc.). The names should correspond to current Version 5
#'   Font Awesome names. A list of short and full names can be accessed through
#'   the [fa_metadata()] function with `fa_metadata()$icon_names` and
#'   `fa_metadata()$icon_names_full`. If supplying a Version 4 icon name, it
#'   will be internally translated to the Version 5 icon name and a Version 5
#'   icon will be returned. A data frame containing the short names that changed
#'   from version 4 (`v4_name`) to version 5 (`v5_name`) can be obtained by
#'   using `fa_metadata()$v4_v5_name_tbl`.
#' @param class Additional classes to customize the style of the icon (see the
#'   usage examples for details on supported styles).
#' @param ... Arguments passed to the `<i>` tag of [htmltools::tags].
#' @param html_dependency Provides an opportunity to use a custom
#'   `html_dependency` object (created via a call to
#'   [htmltools::htmlDependency()]) instead of one supplied by the function
#'   (which uses Font Awesome's free assets and are bundled in the package). A
#'   custom `html_dependency` object is useful when you have paid icons from
#'   Font Awesome or would otherwise like to customize exactly which icon assets
#'   are used (e.g., woff, woff2, eot, etc.). By default, this is `NULL` where
#'   the function internally generates an `html_dependency`.
#' @param verify_fa An option to verify the provided icon `name`. If `TRUE` (the
#'   default), internal checks will take place and issue messages should the
#'   `name` is a Font Awesome 4 icon name (the message will provide the Version
#'   5 name), or, if the icon name cannot be found in either Font Awesome 4 or
#'   5.
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
                 ...,
                 html_dependency = fa_html_dependency(),
                 verify_fa = TRUE) {

  prefix <- "fa"
  iconClass <- ""

  prefix_class <- prefix

  # Change the `prefix_class` to `"fab"` if the `name` is found in
  # the internal vector of `font_awesome_brands`
  if (prefix_class == "fa" && name %in% font_awesome_brands) {
    prefix_class <- "fab"
  }

  if (grepl("^fa[a-z] fa-[a-z-]+$", name)) {
    # Case where fully-qualified icon name is provided

    iconClass <- name

  } else {
    # Case where short icon name is provided

    iconClass <- paste0(prefix_class, " ", prefix, "-", name)

    if (!is.null(class)) {
      iconClass <- paste(iconClass, class)
    }
  }

  icon_tag <- browsable(tags$i(
    class = iconClass,
    role = "presentation",
    `aria-label` = paste(gsub("^fa[a-z]* fa-", "", name), "icon"),
    ...
  ))

  icon_tag <- attachDependencies(icon_tag, html_dependency)

  # Perform verifications on `name` if `verify_fa` is TRUE
  if (verify_fa && identical(html_dependency, fa_html_dependency())) {

    # Determine if the `name` is a Font Awesome v4
    # icon name and provide a message
    if (name %in% fa_tbl$v4_name && !(name %in% fa_tbl$name)) {

      # Obtain the version 5 `name` and `full_name`
      # for messaging purposes
      v5_name <- fa_tbl[fa_tbl$v4_name == name, ][1, "name"]
      v5_name_full <- fa_tbl[fa_tbl$v4_name == name, ][1, "full_name"]

      # State that the v4 icon name should be changed to a v6 one
      message(
        "The `name` provided ('", name ,"') is deprecated in Font Awesome 6:\n",
        "* please consider using '", v5_name, "' or '", v5_name_full, "' instead\n",
        "* use the `verify_fa = FALSE` to deactivate these messages"
      )
    }

    # Provide a message if the icon name can't be resolved from
    # any Font Awesome 4 or 6 names
    if (!(name %in% fa_tbl$full_name) &&
        !(name %in% fa_tbl$name) &&
        !(name %in% fa_tbl$v4_name)
    ) {
      message(
        "This Font Awesome icon ('", name, "') does not exist:\n",
        "* if providing a custom `html_dependency` these `name` checks can \n",
        "  be deactivated with `verify_fa = FALSE`"
      )
    }
  }

  icon_tag
}
