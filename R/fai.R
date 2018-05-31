#' Generate an inline FontAwesome icon
#'
#' Add a FontAwesome icon within inline text.
#' We can optionally set certain style attributes.
#' @param name the name of the FontAwesome icon.
#' @param fill an option to change the fill
#' color of the icon.
#' @return a character object that is the styled
#' FontAwesome icon within \code{svg} tags.
#' @examples
#' # Emit a FontAwesome icon (`r-project`) as
#' # SVG within `i` tags
#' fa(name = "r-project")
#' @importFrom glue glue
#' @importFrom dplyr filter pull rename
#' @importFrom stringr str_replace
#' @importFrom htmltools htmlDependency htmlDependencies
#' @export
fai <- function(name) {

  fa_tbl <- fontawesome:::fa_tbl

  if (name %in% (fa_tbl %>% dplyr::pull(full_name))) {

    tag <-
      glue::glue(
        "<i class=\"",
        name,
        "\"></i>") %>% as.character()

  } else if (name %in% (fa_tbl %>% dplyr::pull(name))) {

    tag <-
      glue::glue(
        "<i class=\"",
        (fa_tbl %>%
           dplyr::filter(name %in% rlang::UQ(rlang::enquo(name))) %>%
           dplyr::pull(full_name))[1],
        "\"></i>") %>% as.character()
  }

  # Construct `style` attributes --------------------------------------------

  style <- "style=\""

  if (!is.null(height)) {
    style <- glue::glue("{style}height:{height};")
  }

  if (!is.null(fill)) {
    style <- glue::glue("{style}fill:{fill};")
  }

  style <- glue::glue("{style}\"")

  if (!grepl(style, pattern = "style=\"\"")) {

    tag <- tag %>%
      stringr::str_replace(
        pattern = "^<i",
        replacement = glue::glue("<i {style}")) %>%
      as.character()
  }

  tag
}
