library(fontawesome)
library(dplyr)

# Ensure we are using the `gtr-table` branch of the gt package;
# This contains support for interactive table generation which is more
# suited toward larger tables

library(gt)

# Generation of fontawesome info tables

fa_data <-
  fontawesome:::fa_tbl %>%
  dplyr::mutate(
    icon = full_name,
    style = toupper(substr(style, 1, 1))
  ) %>%
  dplyr::select(icon, style, name, full_name) %>%
  dplyr::mutate(index = dplyr::case_when(
    grepl("^[0-9]", name) ~ "0-9",
    TRUE ~ toupper(substr(name, 1, 1))
  )) %>%
  dplyr::select(-index)

fa_info_tbl <-
  fa_data %>%
  gt(id = "info", i_html = TRUE) %>%
  tab_header(
    title = md("Icons available in the `fontawesome` package"),
    subtitle = md("All **free** icons from the `v6.1.1` release are included here.")
  ) %>%
  text_transform(
    locations = cells_body(columns = icon),
    fn = function(x) vapply(x, FUN.VALUE = character(1), FUN = function(x) fa(x, a11y = "sem"))
  ) %>%
  cols_width(
    1 ~ px(50),
    2 ~ px(30),
    3 ~ px(320)
  ) %>%
  cols_label(
    icon = "",
    style = "",
    name = "Icon Name",
    full_name = "Full Name"
  ) %>%
  cols_align(align = "center", columns = icon) %>%
  # tab_style(
  #   style = "overflow:hidden;height:14px;white-space:nowrap;text-overflow:ellipsis;",
  #   locations = cells_body()
  # ) %>%
  # tab_style(
  #   style = cell_text(font = google_font("IBM Plex Mono"), size = px(12)),
  #   locations = cells_body(columns = full_name)
  # ) %>%
  # tab_style(
  #   style = "letter-spacing:-0.6px;",
  #   locations = cells_body(columns = full_name, rows = nchar(full_name) > 22)
  # ) %>%
  # tab_style(
  #   style = cell_text(font = google_font("IBM Plex Sans"), size = px(12)),
  #   locations = cells_body(columns = name)
  # ) %>%
  # tab_style(
  #   style = cell_text(align = "center"),
  #   locations = cells_body(columns = icon)
  # ) %>%
  opt_all_caps() %>%
  opt_align_table_header(align = "left") %>%
  tab_options(
    ihtml.use_pagination = TRUE,
    ihtml.use_search = TRUE,
    ihtml.use_sorting = TRUE,
    ihtml.use_filters = FALSE,
    ihtml.use_resizers = FALSE,
    ihtml.use_highlight = TRUE,
    ihtml.use_compact_mode = TRUE
  ) %>%
  opt_css(
    css = "
    #info .gt_row {
      overflow: hidden;
      height: 14px;
      white-space: nowrap;
      text-overflow: ellipsis;
    }

    #info gt_heading gt_subtitle gt_bottom_border {
      border-bottom-style: hidden;
    }
    "
  )


gtsave(fa_info_tbl, filename = "fontawesome_table.html", path = "inst/info_table")

