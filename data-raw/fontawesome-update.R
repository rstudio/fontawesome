# This script is used for extracting the raw SVG from the `icons.json`
# file that exists in the `Font-Awesome` repo (address below) and that
# also ships with the official releases.
#
# We assume that icons.json is relatively self-contained and stable.
# We cannot assume that any of SVG extracted from this file in this
# location are considered final
library(rprojroot)
library(pointblank)
library(dplyr)
library(purrr)
library(tibble)
library(withr)

if (Sys.which("yarn") == "") {
  stop("The yarn CLI must be installed and in your PATH")
}

with_dir(
  find_package_root_file("inst"), {
    system("yarn install --production")
    with_dir(
      "node_modules/@fortawesome/fontawesome-free", {
        # Installed version
        fa_version <- jsonlite::fromJSON("package.json")$version
        # FA4 -> FA5 shims
        shims <- yaml::read_yaml("metadata/shims.yml")
        # Unfortunately the installed package doesn't distribute this
        # JSON file, which contains all the SVG info we need
        icons <- jsonlite::fromJSON(sprintf(
            "https://raw.githubusercontent.com/FortAwesome/Font-Awesome/%s/metadata/icons.json", fa_version
        ))
      }
    )
})


# Tidy shims
fa_tbl_shims <- shims %>%
  enframe("v4_name", "name") %>%
  mutate(
    name = map_chr(name, function(x) x[["name"]] %||% NA)
  ) %>%
  filter(!is.na(name))


# Tidy icon info
fa_tbl <- icons %>%
  enframe("name", "info") %>%
  mutate(
    label = map_chr(info, "label"),
    svg = map(info, function(x) enframe(x$svg, "style", "svg_info"))
  ) %>%
  select(-info) %>%
  tidyr::unnest(svg) %>%
  mutate(
    path = map_chr(svg_info, "path"),
    min_x = as.integer(map_chr(svg_info, ~.x$viewBox[1])),
    min_y = as.integer(map_chr(svg_info, ~.x$viewBox[2])),
    width = as.integer(map_chr(svg_info, ~.x$viewBox[3])),
    height = as.integer(map_chr(svg_info, ~.x$viewBox[4]))
  ) %>%
  select(-svg_info) %>%
  mutate(full_name = paste0("fa", substr(style, 1, 1), " fa-", name))


# Add shim info
fa_tbl <- fa_tbl %>%
  left_join(fa_tbl_shims, by = "name") %>%
  mutate(v4_name = ifelse(is.na(v4_name), name, v4_name)) %>%
  as_tibble()

# Generate the `font_awesome_brands` vector for faster retrieval
# in `fa_i()`
font_awesome_brands <- unique(
  fa_tbl$name[grepl("fab ", fa_tbl$full_name)]
)


# ==============================================================================
# Perform validation testing before writing data
# ==============================================================================

# Expect that rows are distinct (with different
# groupings of columns)
expect_rows_distinct(fa_tbl)

# Expect that no values are missing
expect_col_vals_not_null(fa_tbl, vars(name))
expect_col_vals_not_null(fa_tbl, vars(style))
expect_col_vals_not_null(fa_tbl, vars(full_name))
expect_col_vals_not_null(fa_tbl, vars(path))
expect_col_vals_not_null(fa_tbl, vars(min_x))
expect_col_vals_not_null(fa_tbl, vars(min_y))
expect_col_vals_not_null(fa_tbl, vars(width))
expect_col_vals_not_null(fa_tbl, vars(height))
expect_col_vals_not_null(fa_tbl, vars(v4_name))
expect_col_vals_not_null(fa_tbl, vars(label))

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
  preconditions = ~ . %>% mutate(full_name = gsub("fa[rsb] fa-", "", full_name))
)

# Expect there to be more than 1600 rows to the table
expect_col_vals_gt(
  count(fa_tbl),
  columns = vars(n),
  value = 1600
)

# Expect these column names in the table
expect_col_vals_in_set(
  tibble(col_names = colnames(fa_tbl)),
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
# Save the icon info to disk
# ==============================================================================

usethis::use_data(fa_tbl, overwrite = TRUE, internal = TRUE)
# For other projects to consume, if they wish
readr::write_csv(fa_tbl, find_package_root_file("data-raw/fa_tbl.csv"))


with_dir(
  find_package_root_file("R"), {

    cat(
      "# Generated by fontawesome-update.R: do not edit by hand\n\n",
      "fa_version <- ",
      paste(capture.output(dput(fa_version)), collapse = ""),
      file = "fa_version.R"
    )

    cat(
      "# Generated by fontawesome-update.R: do not edit by hand\n\n",
      "font_awesome_brands <-",
      paste(capture.output(dput(font_awesome_brands)), collapse = ""),
      file = "fa_brands.R"
    )

  }
)

# ==============================================================================
# Copy over 'css' and 'webfonts' assets
# ==============================================================================

dest_dir <- find_package_root_file("inst/fontawesome")
unlink(dest_dir, recursive = TRUE)

source_dir <- find_package_root_file("inst/node_modules/@fortawesome/fontawesome-free")

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
withr::with_dir(dest_dir, {
  file.remove(
    "webfonts/fa-brands-400.eot",
    "webfonts/fa-brands-400.svg",
    ## Remove this line once phantomjs is not supported
    # "inst/fontawesome/webfonts/fa-brands-400.ttf",
    "webfonts/fa-brands-400.woff2",
    "webfonts/fa-regular-400.eot",
    "webfonts/fa-regular-400.svg",
    ## Remove this line once phantomjs is not supported
    # "inst/fontawesome/webfonts/fa-regular-400.ttf",
    "webfonts/fa-regular-400.woff2",
    "webfonts/fa-solid-900.eot",
    "webfonts/fa-solid-900.svg",
    ## Remove this line once phantomjs is not supported
    # "inst/fontawesome/webfonts/fa-solid-900.ttf",
    "webfonts/fa-solid-900.woff2"
  )

  # Patch the `all.css` file to remove entries for all but `woff` icon files
  readr::read_file(file = "css/all.css") %>%
    gsub(
      "src: url\\(.../webfonts/fa-([^.]+).*?}",
      'src: url("../webfonts/fa-\\1.woff") format("woff"), url("../webfonts/fa-\\1.ttf") format("truetype"); }',
      .
    ) %>%
    readr::write_file(file = "css/all.css")

 # Patch the `all.min.css` file to remove entries for all but `woff` icon files
 readr::read_file(file = "css/all.min.css") %>%
   gsub(
     "src:url\\(../webfonts/fa-([^.]+).*?}",
     'src: url("../webfonts/fa-\\1.woff") format("woff"), url("../webfonts/fa-\\1.ttf") format("truetype"); }',
     .
   ) %>%
   readr::write_file(file = "css/all.min.css")
})
