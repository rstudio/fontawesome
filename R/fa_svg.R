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
#' #' @examples
#' # Emit a FontAwesome icon (`r-project`) as
#' # SVG within `svg` tags
#' fa_svg(name = "r-project")
#' @importFrom glue glue
#' @importFrom dplyr filter pull rename
#' @importFrom stringr str_replace
#' @export
fa_svg <- function(name,
                   height = 30,
                   fill = NULL) {

  fa_tbl <- gt:::fa_tbl

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
  }

  style <- glue::glue("style=\"height:{height};")

  if (!is.null(fill)) {
    style <- glue::glue("{style}fill:{fill};")
  }

  style <- glue::glue("{style}\"")

  stringr::str_replace(
    string = svg,
    pattern = "^<svg",
    replacement = glue::glue("<svg {style}")) %>%
    as.character()
}
