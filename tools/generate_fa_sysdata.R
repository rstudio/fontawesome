# This script is used for extracting the raw SVG from the `icons.json`
# file that exists in the `Font-Awesome` repo (address below) and that
# also ships with the official releases.
#
# We assume that icons.json is relatively self-contained and stable.
# We cannot assume that any of SVG extracted from this file in this
# location are considered final
library(tidyverse)
library(jsonlite)
library(usethis)

# Read in `icons.json` file
fa_list <-
  jsonlite::fromJSON(
    txt = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/metadata/icons.json")

# Generate an empty table
fa_tbl <-
  dplyr::tibble(
    name = character(0),
    style = character(0),
    full_name = character(0),
    svg = character(0)
  )

# Traverse through every top-level item in `fa_list`
# and build the `fa_tbl` object
for (i in seq_along(fa_list)) {

  fa_list_item <- fa_list[i]
  name <- fa_list_item %>% names()
  styles <- fa_list_item[[1]]$styles

  if ("brands" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$brands$raw %>%
      stringr::str_replace_all(
        pattern = fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "brands",
          full_name = glue::glue("fab fa-{name}") %>% as.character(),
          svg = svg
        )
      )
  }

  if ("solid" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$solid$raw %>%
      stringr::str_replace_all(
        pattern = fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "solid",
          full_name = glue::glue("fas fa-{name}") %>% as.character(),
          svg = svg
        )
      )
  }

  if ("regular" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$regular$raw %>%
      stringr::str_replace_all(
        pattern = fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "regular",
          full_name = glue::glue("far fa-{name}") %>% as.character(),
          svg = svg
        )
      )
  }

  fa_tbl <- as.data.frame(fa_tbl)
}

# Create `sysdata.rda`
if (fontawesome:::audit_fa_tbl(tbl = fa_tbl)) {
  usethis::use_data(fa_tbl, internal = TRUE, overwrite = TRUE)
}
