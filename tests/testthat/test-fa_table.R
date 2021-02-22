skip_on_cran()

test_that("The `fa_tbl` object fits expectations", {

  library(pointblank)

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
})
