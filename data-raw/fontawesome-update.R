# This script is used for extracting the raw SVG from the `icons.json`
# file that exists in the `Font-Awesome` repo (address below) and that
# also ships with the official releases.
#
# We assume that `icons.json` is relatively self-contained and stable.
# We cannot assume that any of SVG extracted from this file in this
# location are considered final
library(rprojroot)
library(pointblank)
library(dplyr)
library(purrr)
library(tibble)
library(withr)

version_tag <- "6.5.2"

base_url <-
  file.path(
    "https://raw.githubusercontent.com/FortAwesome/Font-Awesome", version_tag
  )

# FA4 -> FA6 shims
shims <- yaml::read_yaml(file.path(base_url, "metadata/shims.yml"))

# All icon info
icons <- jsonlite::fromJSON(file.path(base_url, "metadata/icons.json"))

# Reported version (should match `version_tag`)
fa_version <-
  jsonlite::fromJSON(
    file.path(base_url, "js-packages/@fortawesome/fontawesome-free/package.json")
  )$version

# Tidy the `icons` table
fa_tbl <-
  icons %>%
  tibble::enframe("name", "info") %>%
  dplyr::mutate(
    label = purrr::map_chr(info, "label"),
    svg = purrr::map(info, function(x) tibble::enframe(x$svg, "style", "svg_info"))
  ) %>%
  dplyr::select(-info) %>%
  tidyr::unnest(svg) %>%
  dplyr::mutate(
    path = map_chr(svg_info, "path"),
    min_x =  purrr::map_int(svg_info, ~as.integer(.x$viewBox[1])),
    min_y =  purrr::map_int(svg_info, ~as.integer(.x$viewBox[2])),
    width =  purrr::map_int(svg_info, ~as.integer(.x$viewBox[3])),
    height = purrr::map_int(svg_info, ~as.integer(.x$viewBox[4]))
  ) %>%
  dplyr::select(-svg_info) %>%
  dplyr::mutate(full_name = paste0("fa", substr(style, 1, 1), " fa-", name)) %>%
  dplyr::mutate(prefix = paste0("fa", substr(style, 1, 1))) %>%
  dplyr::select(name, prefix, full_name, everything())

# Create a table of alias names
alias_tbl <-
  dplyr::tibble(
    alias = character(0),
    name = character(0)
  )

for (ico in names(icons)) {

  if (!is.null(icons[[ico]][["aliases"]][["names"]])) {

    alias_tbl_i <-
      dplyr::tibble(
        alias = icons[[ico]][["aliases"]][["names"]],
        name = ico
      )

    alias_tbl <- dplyr::bind_rows(alias_tbl, alias_tbl_i)
  }
}

# Generate the `font_awesome_brands` vector for faster retrieval in `fa_i()`
font_awesome_brands <- unique(fa_tbl$name[grepl("fab ", fa_tbl$full_name)])

# ==============================================================================
# Perform validation testing before writing data
# ==============================================================================

# Expect that rows are distinct (with different
# groupings of columns)
expect_rows_distinct(fa_tbl)

# Expect that no values are missing
expect_col_vals_not_null(fa_tbl, "name")
expect_col_vals_not_null(fa_tbl, "style")
expect_col_vals_not_null(fa_tbl, "full_name")
expect_col_vals_not_null(fa_tbl, "path")
expect_col_vals_not_null(fa_tbl, "min_x")
expect_col_vals_not_null(fa_tbl, "min_y")
expect_col_vals_not_null(fa_tbl, "width")
expect_col_vals_not_null(fa_tbl, "height")
expect_col_vals_not_null(fa_tbl, "label")

# Expect that the `style` column contains 3 types of values
expect_col_vals_in_set(
  fa_tbl,
  columns = "style",
  set = c("regular", "solid", "brands")
)

# Expect that the `name` column only has a certain character set
expect_col_vals_regex(
  fa_tbl,
  columns = "name",
  regex = "^[a-z0-9-]*?$"
)

# Expect values in the `full_name` column to adhere to a specific pattern
expect_col_vals_regex(
  fa_tbl,
  columns = "full_name",
  regex = "^fa[brs] fa-[a-z0-9-]*?$"
)

# Expect that the prefix of `full_name` corresponds to the `style` value
expect_col_vals_expr(fa_tbl, ~ case_when(
  style == "regular" ~ grepl("^far", full_name),
  style == "solid" ~ grepl("^fas", full_name),
  style == "brands" ~ grepl("^fab", full_name)
))

# Expect that the `name` value is contained inside the `full_name` value
expect_col_vals_equal(
  fa_tbl,
  columns = "name",
  value = vars(full_name),
  preconditions = ~ . %>% mutate(full_name = gsub("fa[rsb] fa-", "", full_name))
)

# Expect there to be more than 2000 rows in the table
expect_col_vals_gt(
  dplyr::count(fa_tbl),
  columns = vars(n),
  value = 2000
)

# Expect these column names in the table
expect_col_vals_make_set(
  tibble(col_names = colnames(fa_tbl)),
  columns = vars(col_names),
  set = c(
    "name", "prefix", "full_name", "label", "style",
    "path", "min_x", "min_y", "width", "height"
  )
)

# Expect that columns relating to the SVG
# viewBox have constant values
expect_col_vals_equal(fa_tbl, "min_x", 0)
expect_col_vals_equal(fa_tbl, "min_y", 0)
expect_col_vals_equal(fa_tbl, "height", 512)

# Expect that certain columns are of the integer class
expect_col_is_integer(fa_tbl, vars(min_x, min_y, width, height))

# ==============================================================================
# Save the icon and alias info to disk
# ==============================================================================

# Write the `fa_tbl` and `alias_tbl` tables to internal data ('R/sysdata.rda')
usethis::use_data(
  fa_tbl,
  alias_tbl,
  overwrite = TRUE, internal = TRUE
)

# Write a CSV to the `data-raw` folder for other projects to consume
readr::write_csv(
  fa_tbl,
  rprojroot::find_package_root_file("data-raw/fa_tbl.csv")
)

# Write the `fa_version.R` and `fa_brands.R` files to the `R` dir
withr::with_dir(
  rprojroot::find_package_root_file("R"), {

    cat(
      "# Generated by fontawesome-update.R: do not edit by hand\n\n",
      "fa_version <- ",
      paste(capture.output(dput(fa_version)), collapse = ""),
      sep = "",
      file = "fa_version.R"
    )

    cat(
      "# Generated by fontawesome-update.R: do not edit by hand\n\n",
      "font_awesome_brands <- ",
      paste(capture.output(dput(font_awesome_brands)), collapse = ""),
      sep = "",
      file = "fa_brands.R"
    )
  }
)

# ==============================================================================
# Copy over 'css' and 'webfonts' assets
# ==============================================================================

zip_file <- file.path(tempdir(), paste0("font-awesome-", fa_version, ".zip"))

url <-
  paste0(
    "https://github.com/FortAwesome/Font-Awesome/releases/download/",
    fa_version, "/fontawesome-free-", fa_version, "-web.zip"
  )

download.file(url, zip_file)

unzip(zip_file, exdir = tempdir())
source_dir <- file.path(tempdir(), paste0("fontawesome-free-", fa_version, "-web"))

dest_dir <- rprojroot::find_package_root_file("inst/fontawesome")
unlink(dest_dir, recursive = TRUE)

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

filenames <-
  c(
    "css/all.css",
    "css/all.min.css",
    "css/v4-shims.css",
    "css/v4-shims.min.css",
    file.path("webfonts", dir(file.path(source_dir, "webfonts")))
  )

# Copy the complete set of CSS and font files to `inst/fontawesome`
copy_files(source_dir, dest_dir, filenames)

# Remove some font files that won't be supported in this package
# Note: v6+ discontinues support for .woff in favor of .woff2, ttf is retained
withr::with_dir(dest_dir, {

  # Patch the `all.css` file to remove entries for all but `woff2`
  # and `ttf` icon files
  readr::read_file(file = "css/all.css") %>%
    gsub(
      "src: url\\(.../webfonts/fa-([^.]+).*?}",
      'src: url("../webfonts/fa-\\1.woff2") format("woff2"), url("../webfonts/fa-\\1.ttf") format("truetype"); }',
      .
    ) %>%
    readr::write_file(file = "css/all.css")

 # Patch the `all.min.css` file to remove entries for all but `woff2`
  # and `ttf` icon files
 readr::read_file(file = "css/all.min.css") %>%
   gsub(
     "src:url\\(../webfonts/fa-([^.]+).*?}",
     'src: url("../webfonts/fa-\\1.woff2") format("woff2"), url("../webfonts/fa-\\1.ttf") format("truetype"); }',
     .
   ) %>%
   readr::write_file(file = "css/all.min.css")
})
