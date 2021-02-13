# nocov start

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

audit_fa_tbl <- function(tbl) {

  row_count_chk <-
    pointblank::test_col_vals_gt(
      tbl %>% dplyr::count(),
      columns = vars(n),
      value = 1600
    )

  col_count_chk <- ncol(tbl) == 4

  col_names_chk <-
    pointblank::test_col_vals_in_set(
      dplyr::tibble(col_names = colnames(tbl)),
      columns = vars(col_names),
      set = c("name", "style", "full_name", "svg")
    )

  col_names_chk & col_names_chk & col_names_chk
}

# nocov end
