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
#'   `"fas fa-drum"`, etc.). The names should correspond to current Font Awesome
#'   names. A list of short and full names can be accessed through the
#'   [fa_metadata()] function with `fa_metadata()$icon_names` and
#'   `fa_metadata()$icon_names_full`. If supplying a known alias to a short icon
#'   name (e.g., `"vcard"`, which is now `"address-card"`), it will be
#'   internally translated to the current icon name before returning the icon
#'   tag.
#' @param class Additional classes to customize the style of the icon.
#' @param ... Arguments passed to the `<i>` tag of [htmltools::tags].
#' @param prefer_type Chooses the type of icon returned if: (1) providing a
#'   short name, and (2) that icon has both solid and regular types.
#'   For example, using `name = "address-book"` will result in two types of
#'   icons for an Address Book. By default, this preference is set to
#'   `"regular"` and the other option is `"solid"`.
#' @param html_dependency Provides an opportunity to use a custom
#'   `html_dependency` object (created via a call to
#'   [htmltools::htmlDependency()]) instead of one supplied by the function
#'   (which uses Font Awesome's free assets and are bundled in the package). A
#'   custom `html_dependency` object is useful when you have paid icons from
#'   Font Awesome or would otherwise like to customize exactly which icon assets
#'   are used (e.g., `woff`, `woff2`, `eot`, etc.). By default, this is `NULL`
#'   where the function internally generates an `html_dependency`.
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
fa_i <- function(
    name,
    class = NULL,
    ...,
    prefer_type = c("regular", "solid"),
    html_dependency = fa_html_dependency()
) {

  prefer_type <- match.arg(prefer_type)

  # Ensure that the `name` value passes basic validation checks
  check_name_vec(name = name)

  iconClass <- ""

  if (grepl("^fa[a-z] fa-[a-z-]+$", name)) {
    # Case where fully-qualified icon name is provided

    iconClass <- paste(c(name, class), collapse = " ")

  } else {
    # Case where short icon name is provided

    # Get the icon index value in `fa_tbl`
    idx <-
      get_icon_idx(
        name = name,
        prefer_type = prefer_type,
        fail_on_unknown_name = FALSE,
        msg_on_unknown_name = identical(html_dependency, fa_html_dependency())
      )

    if (is_na(idx)) {

      # Generate `iconClass` value for name that results in an NA
      # value for `idx` (i.e., doesn't correspond to an entry in the
      # `fa_tbl`)

      iconClass <- paste0("fa", substr(prefer_type, 1, 1), " fa-", name)

    } else {

      # Get the fully-qualified icon name from `fa_tbl`
      name <- iconClass <- fa_tbl[idx, ][["full_name"]]
    }

    # Append any provided `class` values to the icon name
    if (!is.null(class)) {
      iconClass <- paste(name, class)
    }

  }

  icon_tag <-
    browsable(
      tags$i(
        class = iconClass,
        role = "presentation",
        `aria-label` = paste(gsub("^fa[a-z]* fa-", "", name), "icon"),
        ...
      )
    )

  attachDependencies(icon_tag, html_dependency)
}
