#' Get metadata on the included Font Awesome assets
#'
#' @description
#' This function provide some metadata about the included Font Awesome assets
#' in the **fontawesome** package. The list that is returned has the following
#' components:
#'
#' - `version`: The released version number for the Font Awesome icons
#' - `icon_count`: The total count of unique Font Awesome icons
#' - `icon_names`: A vector of short names (e.g., `"npm"`, `"drum"`, etc.) for
#' all included icons
#' - `icon_names_full`: A vector containing the full names (e.g., `"fab
#' fa-npm"`, `"fas fa-drum"`, etc.) for all included icons
#' - `icon_names_fa(r|s|b)`: Vectors of short names within the regular (`"r"`),
#' solid (`"s"`), and brand (`"b"`) groups
#' - `icon_names_full_fa(r|s|b)`: Vectors with the full names of icons within
#' the regular (`"r"`), solid (`"s"`), and brand (`"b"`) groups
#' - `v4_v5_name_tbl`: A data frame containing the short names that changed
#' from version 4 (`v4_name`) to version 5 (`v5_name`) of Font Awesome
#'
#' @return A list with metadata for the included Font Awesome assets.
#'
#' @examples
#' if (interactive()) {
#'
#' # Get information on the Font Awesome
#' # assets included in this package
#' fa_metadata()
#'
#' }
#'
#' @export
fa_metadata <- function() {

  icon_names <- unique(fa_tbl$name)
  icon_names_full <- unique(fa_tbl$full_name)
  icon_names_far <- unique(fa_tbl$name[grepl("far ", fa_tbl$full_name)])
  icon_names_fas <- unique(fa_tbl$name[grepl("fas ", fa_tbl$full_name)])
  icon_names_fab <- unique(fa_tbl$name[grepl("fab ", fa_tbl$full_name)])
  icon_names_full_far <- unique(fa_tbl$full_name[grepl("far ", fa_tbl$full_name)])
  icon_names_full_fas <- unique(fa_tbl$full_name[grepl("fas ", fa_tbl$full_name)])
  icon_names_full_fab <- unique(fa_tbl$full_name[grepl("fab ", fa_tbl$full_name)])

  # Generate a table that has the changes in short names
  # from version 4 to version 5 of FA
  v4_v5 <- fa_tbl[, c("v4_name", "name")]
  names(v4_v5) <- c("v4_name", "v5_name")
  v4_v5 <- v4_v5[v4_v5$v4_name != v4_v5$v5_name, ]
  v4_v5 <- v4_v5[!duplicated(v4_v5), ]

  list(
    version = fa_version,
    icon_count = length(icon_names),
    icon_names = icon_names,
    icon_names_full = icon_names_full,
    icon_names_far = icon_names_far,
    icon_names_fas = icon_names_fas,
    icon_names_fab = icon_names_fab,
    icon_names_full_far = icon_names_full_far,
    icon_names_full_fas = icon_names_full_fas,
    icon_names_full_fab = icon_names_full_fab,
    v4_v5_name_tbl = v4_v5
  )
}
