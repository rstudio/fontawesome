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
  } else {
    return("")
  }

  tag
}
