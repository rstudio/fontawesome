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
library(readr)

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
    min_x = integer(0),
    min_y = integer(0),
    width = integer(0),
    height = integer(0),
    label = character(0)
  )

get_viewbox_vals <- function(svg) {
  svg %>%
    stringr::str_extract("viewBox=\"(.*?)\"") %>%
    stringr::str_replace_all("viewBox.|\"", "") %>%
    str_split(pattern = " ") %>%
    unlist() %>%
    as.integer()
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


# ==============================================================================
# Perform validation testing before writing the .rda file
# ==============================================================================

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

# Expect that certain columns are integer
expect_col_is_integer(fa_tbl, vars(min_x, min_y, width, height))

# ==============================================================================
# Generate code
# ==============================================================================

# Generate code that creates a data frame via a CSV textConnection.
gen_csv_df_code <- function(varname, data) {

  # Generate the CSV content (actually semicolon delimited due to difficulties
  # with escaping commas).
  tc <- textConnection("csv_content", "w")
  write.table(data, tc, sep = ";", row.names = FALSE, quote = FALSE)
  close(tc)

  outfile <- here::here(sprintf("R/%s.R", varname))

  # Escape quotes and unicode characters
  csv_content <- stringi::stri_escape_unicode(csv_content)
  # Split into multiple lines and wrap each line with leading quote and trailing
  # quote plus comma. We need to split it into multiple strings because if we
  # don't, when R runs the generated code, it complains:
  #   Error in source("R/fa_tbl.R") :
  #    string at line 1626 containing Unicode escapes not in this locale
  #    is too long (max 10000 chars)
  # This seems to happen only if escaped unicode chars (like "\u00e9") are
  # present in the string.
  csv_content <- as.character(strsplit(csv_content, "\n"))
  csv_content <- paste0('    "', csv_content, '",')
  # Remove last trailing comma
  csv_content[length(csv_content)] <- sub(",$", "", csv_content[length(csv_content)])


  # Generate the code in R/<varname>.R
  cat(
    sprintf(
'# Generated by fontawesome-update.R; do not edit by hand

%s <- local({
  csv_content <- c(\n', varname),
    file = outfile
  )
  cat(csv_content, file = outfile, sep = "\n", append = TRUE)
  cat(
'  )
  tc <- textConnection(csv_content)
  on.exit(close(tc))
  read.table(tc, header = TRUE, quote = "", sep = ";", stringsAsFactors = FALSE)
})
',
    file = outfile,
    append = TRUE
  )

  # Check that input data is the same as output data
  local({
    source(outfile, local = TRUE)
    testthat::expect_identical(data, get(varname))
  })
}


gen_csv_df_code(varname = "fa_tbl",   data = fa_tbl)
gen_csv_df_code(varname = "fa_v4_v5", data = fa_v4_v5)


# Write the Font Awesome version number to an .R file
cat(
  "# Generated by fontawesome-update.R: do not edit by hand\n\n",
  "fa_version <- \"", fa_version, "\"\n",
  sep = "",
  file = here::here("R/fa_version.R")
)


#
# Write `font_awesome_brands` content to `font_awesome_brands.R`
#
outfile <- here::here("R/fa_brands.R")
cat(
"# Generated by fontawesome-update.R: do not edit by hand

font_awesome_brands <- c(
",
file = outfile
)
cat(
  paste(paste0('  "', font_awesome_brands, '"'), collapse = ",\n"),
  file = outfile,
  append = TRUE
)
cat("\n)\n", file = outfile, append = TRUE)

# ==============================================================================
# Download FA files and add them to 'inst/fontawesome/css'
# and 'inst/fontawesome/webfonts'
# ==============================================================================

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

# Copy the complete set of CSS and font files to `inst/fontawesome`
copy_files(source_dir, dest_dir, filenames)

# Remove font files that won't be supported in this package
file.remove(
  "inst/fontawesome/webfonts/fa-brands-400.eot",
  "inst/fontawesome/webfonts/fa-brands-400.svg",
  "inst/fontawesome/webfonts/fa-brands-400.ttf",
  "inst/fontawesome/webfonts/fa-brands-400.woff2",
  "inst/fontawesome/webfonts/fa-regular-400.eot",
  "inst/fontawesome/webfonts/fa-regular-400.svg",
  "inst/fontawesome/webfonts/fa-regular-400.ttf",
  "inst/fontawesome/webfonts/fa-regular-400.woff2",
  "inst/fontawesome/webfonts/fa-solid-900.eot",
  "inst/fontawesome/webfonts/fa-solid-900.svg",
  "inst/fontawesome/webfonts/fa-solid-900.ttf",
  "inst/fontawesome/webfonts/fa-solid-900.woff2"
)

# Patch the `all.css` file to remove entries for all but `woff` icon files
all_css <-
  readr::read_file(file = "inst/fontawesome/css/all.css") %>%
  gsub(
    "src: url\\(.../webfonts/fa-([^.]+).*?}",
    "src: url(\"../webfonts/fa-\\1.woff\") format(\"woff\"); }",
    .
  )
readr::write_file(all_css, file = "inst/fontawesome/css/all.css")

# Patch the `all.min.css` file to remove entries for all but `woff` icon files
all_css_min <-
  readr::read_file(file = "inst/fontawesome/css/all.min.css") %>%
  gsub(
    "src:url\\(../webfonts/fa-([^.]+).*?}",
    "src:url(\"../webfonts/fa-\\1.woff\") format(\"woff\");}",
    .
  )
readr::write_file(all_css_min, file = "inst/fontawesome/css/all.min.css")
