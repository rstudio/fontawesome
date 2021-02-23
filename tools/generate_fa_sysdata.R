# This script is used for extracting the raw SVG from the `icons.json`
# file that exists in the `Font-Awesome` repo (address below) and that
# also ships with the official releases.
#
# We assume that icons.json is relatively self-contained and stable.
# We cannot assume that any of SVG extracted from this file in this
# location are considered final
library(tidyverse)
library(stringr)
library(pointblank)
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

#
# Perform validation testing before writing the .rda file
#

# Expect that rows are distinct (with different
# groupings of columns)
expect_rows_distinct(fa_tbl)
expect_rows_distinct(fa_tbl, columns = vars(full_name))
expect_rows_distinct(fa_tbl, columns = vars(svg), threshold = 20)

# Expect that the names in the `brands` style match with
# our determined set
expect_col_vals_in_set(
  fa_tbl,
  columns = vars(name),
  set = font_awesome_brands,
  preconditions = ~ . %>% dplyr::filter(style == "brands")
)

# Expect that no values are missing
expect_col_vals_not_null(fa_tbl, vars(name))
expect_col_vals_not_null(fa_tbl, vars(style))
expect_col_vals_not_null(fa_tbl, vars(full_name))
expect_col_vals_not_null(fa_tbl, vars(svg))

# Expect that there is an SVG formed
# with `<svg>...</svg>` in the `svg` column
expect_col_vals_regex(fa_tbl, vars(svg), regex = "^<svg.*</svg>$")

# Expect that there is an SVG viewBox present and it
# has a specific pattern
expect_col_vals_regex(
  fa_tbl,
  columns = vars(svg),
  regex = "<svg.*?viewBox=\"0 0 [0-9]{3} [0-9]{3}\">",
  threshold = 10
)

# Expect that each SVG is composed of a single path
expect_col_vals_regex(
  fa_tbl,
  columns = vars(svg),
  regex = "<svg.*?><path d=.*></svg>$"
)

# Expect that the `style` column contains 3 types of values
expect_col_vals_in_set(
  fa_tbl,
  columns = vars(style),
  set = c("regular", "solid", "brands")
)

# Expect that the `name` columns only has a certain character set
expect_col_vals_regex(
  fa_tbl,
  columns = vars(name),
  regex = "^[a-z0-9-]*?$"
)

# Expect the `full_name` column to adhere to a specific pattern
expect_col_vals_regex(
  fa_tbl,
  columns = vars(full_name),
  regex = "^fa[brs] fa-[a-z0-9-]*?$"
)

# Expect that the prefix of `full_name` corresponds to the
# `style` value
expect_col_vals_expr(fa_tbl, ~ case_when(
  style == "regular" ~ grepl("^far", full_name),
  style == "solid" ~ grepl("^fas", full_name),
  style == "brands" ~ grepl("^fab", full_name)
))

# Expect that the `name` value is contained inside the
# `full_name` value
expect_col_vals_equal(
  fa_tbl,
  columns = vars(name),
  value = vars(full_name),
  preconditions = ~ . %>% dplyr::mutate(full_name = gsub("fa[rsb] fa-", "", full_name))
)

# Expect than a `name` is not repeated more than 2 times (except for the
# `font-awesome-logo-full` icon)
expect_col_vals_lte(
  fa_tbl %>%
    dplyr::group_by(name) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::filter(name != "font-awesome-logo-full"),
  columns = vars(n),
  value = 2
)

expect_col_vals_gt(
  fa_tbl %>% dplyr::count(),
  columns = vars(n),
  value = 1600
)

expect_col_vals_in_set(
  dplyr::tibble(col_names = colnames(fa_tbl)),
  columns = vars(col_names),
  set = c("name", "style", "full_name", "svg")
)

# Create `sysdata.rda`
usethis::use_data(fa_tbl, internal = TRUE, overwrite = TRUE)
