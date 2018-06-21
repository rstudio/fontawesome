#' Create a PNG version of a FontAwesome icon
#'
#' Get a FontAwesome icon as a PNG file. We can
#' optionally set the fill attribute before
#' writing the PNG. Additionally, there is control
#' over the output width and height (usually, icons
#' are 512 by 512 pixels).
#' @param name the name of the FontAwesome icon.
#' @param file the path to the output file. If \code{NULL},
#' then filename will take the short name of the icon (with
#' a \code{.png} extension).
#' @param fill an option to change the fill color of
#' the icon.
#' @param width the output width in pixels. If \code{NULL},
#' then the output width will match that of the SVG's
#' \code{viewBox}.
#' @param height the output height in pixels. If \code{NULL},
#' then the output height will match that of the SVG's
#' \code{viewBox}.
#' @export
fa_png <- function(name,
                   file = NULL,
                   fill = NULL,
                   width = NULL,
                   height = NULL) {

  # nocov start

  # Obtain the inline SVG as a vector
  svg <- fa(name = name, fill = fill)

  # Stop function if the name doesn't match a
  # FontAwesome icon name
  if (svg == "") {
    stop("The provided `name` doesn't correspond to that of a FontAwesome icon.",
         call. = FALSE)
  }

  # If `file` is `NULL` then construct a suitable
  # filename based on the icon's short name
  if (is.null(file)) {

    if (name %in% fa_tbl$full_name) {

      short_name <-
        fa_tbl[which(fa_tbl$full_name == name), ][1, 1]

    } else if (name %in% fa_tbl$name) {

      short_name <- name
    }

    file <- paste0(short_name, ".png")
  }

  # If the `rsvg` package is available, perform
  # the conversion from SVG to PNG
  if (requireNamespace("rsvg", quietly = TRUE)) {

    rsvg::rsvg_png(
      svg = charToRaw(svg),
      file = file,
      width = width,
      height = height)
  } # nocov end
}
