#' Generate a FontAwesome icon as an SVG
#'
#' Add a FontAwesome icon as SVG contained within
#' \code{<svg>...</svg>}. We can optionally set
#' certain style attributes.
#' @param name the name of the FontAwesome icon.
#' @param height the absolute height (px)
#' of the rendered SVG.
#' @param fill an option to change the fill
#' color of the icon.
#' @return a character object that is the styled
#' FontAwesome icon within \code{svg} tags.
#' @examples
#' # Emit a FontAwesome icon (`r-project`) as
#' # SVG within `svg` tags
#' fa_svg(name = "r-project")
#' @importFrom glue glue
#' @importFrom dplyr filter pull rename
#' @importFrom stringr str_replace
#' @export
fa <- function(name,
               height = NULL,
               fill = NULL) {

  fa_tbl <- fontawesome:::fa_tbl

  if (name %in% (fa_tbl %>% dplyr::pull(full_name))) {

    svg <-
      (fa_tbl %>%
         dplyr::filter(full_name %in% rlang::UQ(rlang::enquo(name))) %>%
         dplyr::pull(svg))[1]

  } else if (name %in% (fa_tbl %>% dplyr::pull(name))) {

    svg <-
      (fa_tbl %>%
         dplyr::filter(name %in% rlang::UQ(rlang::enquo(name))) %>%
         dplyr::pull(svg))[1]
  } else {

    return("")
  }

  # Construct `style` attributes --------------------------------------------

  style <- "style=\""

  if (is.null(height)) {
    style <- glue::glue("{style}height:0.8em;top:.04em;position:relative;")
  } else {
    style <- glue::glue("{style}height:{height};")
  }

  if (!is.null(fill)) {
    style <- glue::glue("{style}fill:{fill};")
  }

  style <- glue::glue("{style}\"")

  if (!grepl(style, pattern = "style=\"\"")) {

    svg <- svg %>%
      stringr::str_replace(
        pattern = "^<svg",
        replacement = glue::glue("<svg {style}")) %>%
      as.character()
  }

  svg
}
