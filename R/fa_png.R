#' Create a PNG version of a FontAwesome icon
#'
#' Get a FontAwesome icon as a PNG file. We can optionally set the fill
#' attribute before writing the PNG. Additionally, there is control over the
#' output width and height (usually, icons are 512 by 512 pixels).
#'
#' @inheritParams fa
#' @param file the path to the output file. If `NULL`, then filename will
#'   take the short name of the icon and a `.png` extension will be
#'   applied.
#' @param height,width The output height and width of the rendered PNG.
#'   If nothing is provided then the output dimensions will match that of the
#'   input SVG viewBox.
#'
#' @return A PNG file written to disk.
#'
#' @examples
#' # Create a FontAwesome SVG icon as a
#' # PNG file on disk
#' fa_png(name = "r-project")
#'
#' @export
fa_png <- function(name,
                   file = NULL,
                   fill = NULL,
                   fill_opacity = NULL,
                   stroke = NULL,
                   stroke_width = NULL,
                   stroke_opacity = NULL,
                   height = NULL,
                   width = NULL) {

  # nocov start

  # Obtain the SVG
  svg <-
    fa(
      name = name,
      fill = fill,
      fill_opacity = fill_opacity,
      stroke = stroke,
      stroke_width = stroke_width,
      stroke_opacity = stroke_opacity
    )

  # If `file` is `NULL` then construct a suitable
  # filename based on the icon's short name
  if (is.null(file)) {
    if (name %in% fa_tbl$full_name) {
      short_name <- fa_tbl[which(fa_tbl$full_name == name), ][1, 1]
    } else if (name %in% fa_tbl$name) {
      short_name <- name
    }
    file <- paste0(short_name, ".png")
  }

  # If the `rsvg` package is available, perform
  # the conversion from SVG to PNG
  if (requireNamespace("rsvg", quietly = TRUE)) {

    rsvg::rsvg_png(
      svg = charToRaw(as.character(svg)),
      file = file,
      width = width,
      height = height
    )
  }

  # nocov end
}
