# This script is used for extracting the raw SVG from the `icons.json`
# file that exists in the `Font-Awesome` repo (address below) and that
# also ships with the official releases.
#
# We assume that icons.json is relatively self-contained and stable.
# We cannot assume that any of SVG extracted from this file in this
# location are considered final
library(tidyverse)
library(rprojroot)
library(stringr)
library(pointblank)
library(jsonlite)
library(yaml)
library(usethis)

# Read in the `icons.json` file
fa_list <-
  jsonlite::fromJSON(
    txt = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/metadata/icons.json"
  )

# Read in the `package.json` file
package_meta <-
  jsonlite::fromJSON(
    txt = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/js-packages/%40fortawesome/fontawesome-free/package.json"
  )

fa_version <- package_meta[["version"]]

# Read in the `shims.yml` file
shims <-
  yaml::read_yaml(
    file = "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/metadata/shims.yml"
  )

# Generate an empty table
fa_tbl <-
  dplyr::tibble(
    name = character(0),
    style = character(0),
    full_name = character(0),
    svg = character(0),
    path = character(0),
    min_x = numeric(0),
    min_y = numeric(0),
    width = numeric(0),
    height = numeric(0),
    label = character(0)
  )

get_viewbox_vals <- function(svg) {
  svg %>%
    stringr::str_extract("viewBox=\"(.*?)\"") %>%
    stringr::str_replace_all("viewBox.|\"", "") %>%
    str_split(pattern = " ") %>%
    unlist() %>%
    as.numeric()
}

get_svg_path <- function(svg) {
  svg %>%
    gsub("<svg.*?>", "", .) %>%
    gsub("</svg>", "", .)
}

# Traverse through every top-level item in `fa_list`
# and build the `fa_tbl` object
for (i in seq_along(fa_list)) {

  fa_list_item <- fa_list[i]
  name <- fa_list_item %>% names()
  styles <- fa_list_item[[1]]$styles
  label <- fa_list_item[[1]]$label

  if ("brands" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$brands$raw %>%
      stringr::str_replace_all(
        pattern = stringr::fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    viewBox_vals <- get_viewbox_vals(svg)
    path <- get_svg_path(svg)

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "brands",
          full_name = glue::glue("fab fa-{name}") %>% as.character(),
          svg = svg,
          path = path,
          min_x = viewBox_vals[1],
          min_y = viewBox_vals[2],
          width = viewBox_vals[3],
          height = viewBox_vals[4],
          label = label
        )
      )
  }

  if ("solid" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$solid$raw %>%
      stringr::str_replace_all(
        pattern = stringr::fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    viewBox_vals <- get_viewbox_vals(svg)
    path <- get_svg_path(svg)

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "solid",
          full_name = glue::glue("fas fa-{name}") %>% as.character(),
          svg = svg,
          path = path,
          min_x = viewBox_vals[1],
          min_y = viewBox_vals[2],
          width = viewBox_vals[3],
          height = viewBox_vals[4],
          label = label
        )
      )
  }

  if ("regular" %in% styles) {

    svg <-
      fa_list_item[[1]]$svg$regular$raw %>%
      stringr::str_replace_all(
        pattern = stringr::fixed("xmlns=\"http://www.w3.org/2000/svg\" "),
        replacement = ""
      )

    viewBox_vals <- get_viewbox_vals(svg)
    path <- get_svg_path(svg)

    fa_tbl <-
      dplyr::bind_rows(
        fa_tbl,
        dplyr::tibble(
          name = name,
          style = "regular",
          full_name = glue::glue("far fa-{name}") %>% as.character(),
          svg = svg,
          path = path,
          min_x = viewBox_vals[1],
          min_y = viewBox_vals[2],
          width = viewBox_vals[3],
          height = viewBox_vals[4],
          label = label
        )
      )
  }

  fa_tbl <- as.data.frame(fa_tbl)
}

# Generate an empty table for the v4/v5 names in `shims.yml`
fa_tbl_shims <-
  dplyr::tibble(
    v4_name = character(0),
    name = character(0)
  )

for (i in seq_along(shims)) {

  v4_name <- names(shims[i])
  name <- shims[[i]][["name"]]

  if (!is.null(name)) {

    fa_tbl_shims <-
      dplyr::bind_rows(
        fa_tbl_shims,
        dplyr::tibble(
          v4_name = v4_name,
          name = name
        )
      )
  }
}

# Join `fa_tbl_shims` to `fa_tbl`
fa_tbl <-
  fa_tbl %>%
  dplyr::left_join(fa_tbl_shims, by = "name") %>%
  dplyr::mutate(v4_name = dplyr::case_when(
    is.na(v4_name) ~ name,
    TRUE ~ v4_name
  ))

# Generate a table that has the changes in short names
# from version 4 to version 5 of FA
fa_v4_v5 <-
  fa_tbl %>%
  dplyr::select(v4_name, v5_name = name) %>%
  dplyr::filter(v4_name != v5_name) %>%
  dplyr::distinct()

# Generate the `font_awesome_brands` vector for faster retrieval
# in `fa_i()`
font_awesome_brands <- unique(fa_tbl$name[grepl("fab ", fa_tbl$full_name)])

#
# Perform validation testing before writing the .rda file
#

# Expect that rows are distinct (with different
# groupings of columns)
expect_rows_distinct(fa_tbl)

# Expect that no values are missing
expect_col_vals_not_null(fa_tbl, vars(name))
expect_col_vals_not_null(fa_tbl, vars(style))
expect_col_vals_not_null(fa_tbl, vars(full_name))
expect_col_vals_not_null(fa_tbl, vars(svg))
expect_col_vals_not_null(fa_tbl, vars(path))
expect_col_vals_not_null(fa_tbl, vars(min_x))
expect_col_vals_not_null(fa_tbl, vars(min_y))
expect_col_vals_not_null(fa_tbl, vars(width))
expect_col_vals_not_null(fa_tbl, vars(height))
expect_col_vals_not_null(fa_tbl, vars(v4_name))
expect_col_vals_not_null(fa_tbl, vars(label))

# Expect that there is an SVG formed
# with `<svg>...</svg>` in the `svg` column
expect_col_vals_regex(fa_tbl, vars(svg), regex = "^<svg.*</svg>$")

# Expect that there is an SVG path formed
# with `<path>...</path>` in the `path` column
expect_col_vals_regex(fa_tbl, vars(path), regex = "^<path.*/>$")

# Expect that there is always just an SVG path within
# the `<svg>` tag
expect_col_vals_regex(fa_tbl, vars(svg), regex = "^<svg.*?><path.*/></svg>$")

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

# Expect that the `name` column only has a certain character set
expect_col_vals_regex(
  fa_tbl,
  columns = vars(name),
  regex = "^[a-z0-9-]*?$"
)

# Expect that the `v4_name` column only has a certain character set
expect_col_vals_regex(
  fa_tbl,
  columns = vars(v4_name),
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

# Expect there to be more than 1600 rows to the table
expect_col_vals_gt(
  fa_tbl %>% dplyr::count(),
  columns = vars(n),
  value = 1600
)

# Expect these column names in the table
expect_col_vals_in_set(
  dplyr::tibble(col_names = colnames(fa_tbl)),
  columns = vars(col_names),
  set = c(
    "name", "style", "full_name", "svg", "path",
    "min_x", "min_y", "width", "height",
    "v4_name", "label"
  )
)

# Expect that columns relating to the SVG
# viewBox have constant values
expect_col_vals_equal(fa_tbl, vars(min_x), 0)
expect_col_vals_equal(fa_tbl, vars(min_y), 0)
expect_col_vals_equal(fa_tbl, vars(height), 512)

# Expect that certain columns are numeric
expect_col_is_numeric(fa_tbl, vars(min_x, min_y, width, height))

# Create `sysdata.rda`; this adds the `fa_tbl` data frame
# and the `fa_version` length-1 character vector
usethis::use_data(
  fa_tbl, fa_v4_v5, fa_version, font_awesome_brands,
  internal = TRUE, overwrite = TRUE
)

# Write the `fa_tbl` and `fa_v4_v5` tables to CSV files in `data-raw`
readr::write_csv(fa_v4_v5, file = "data-raw/fa_v4_v5.csv")
readr::write_csv(fa_tbl, file = "data-raw/fa_tbl.csv")

# Write the Font Awesome version number to an .R file
fa_version_text <-
  paste0(
    "# Generated by fontawesome: do not edit by hand\n\n",
    "version_fontawesome <- \"", fa_version, "\"",
    collapse = ""
  )

# Write the `fa_version_text` to the 'version_fontawesome.R' file
fontawesome:::fa_write_file(
  path = "R/version_fontawesome.R",
  lines = fa_version_text,
  append = FALSE
)

# Download FA files and add them to 'inst/fontawesome/css'
# and 'inst/fontawesome/webfonts'

# Download and unzip to temp dir

zip_file <- file.path(tempdir(), paste0("font-awesome-", fa_version, ".zip"))

url <- paste0(
  "https://github.com/FortAwesome/Font-Awesome/releases/download/",
  fa_version, "/fontawesome-free-", fa_version, "-web.zip"
)
download.file(url, zip_file)

unzip(zip_file, exdir = tempdir())
source_dir <- file.path(tempdir(), paste0("fontawesome-free-", fa_version, "-web"))

# Remove the previous files

dest_dir <- find_package_root_file("inst", "fontawesome")
unlink(dest_dir, recursive = TRUE)

# Copy the downloaded files

copy_files <- function(srcdir, destdir, filenames) {

  # Create needed directories
  dest_subdirs <- file.path(destdir, unique(dirname(filenames)))
  for (dest_subdir in dest_subdirs) {
    dir.create(dest_subdir, recursive = TRUE)
  }

  res <- file.copy(
    from = paste0(srcdir,  "/", filenames),
    to   = paste0(destdir, "/", filenames)
  )

  if (!all(res)) {
    message("Problem copying ", sum(!res), " files: \n  ",
            paste0(filenames[!res], collapse = "\n  ")
    )
  }
}

filenames <- c(
  "css/all.css",
  "css/all.min.css",
  "css/v4-shims.css",
  "css/v4-shims.min.css",
  file.path("webfonts", dir(file.path(source_dir, "webfonts")))
)

copy_files(source_dir, dest_dir, filenames)
