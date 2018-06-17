context("Getting FA icons")

test_that("getting a basic FA icon works", {

  # Emit a FontAwesome icon (`file`) as
  # SVG within `svg` tags; refer to the icon
  # with the 'short' name
  expect_equal(
    fa(name = "file") %>% as.character(),
    "<svg style=\"height:0.8em;top:.04em;position:relative;\" viewBox=\"0 0 384 512\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>")

  # Emit a FontAwesome icon (`file`) as
  # SVG within `svg` tags; refer to the icon
  # with the 'long' name
  expect_equal(
    fa(name = "fas fa-file") %>% as.character(),
    "<svg style=\"height:0.8em;top:.04em;position:relative;\" viewBox=\"0 0 384 512\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>")

  # In that case that an icon cannot be
  # retrieved as an SVG, ensure that an empty
  # string is returned
  expect_equal(
    fa(name = "fas fa-files") %>% as.character(),
    "")
})

test_that("inserting the fill attribute works for an FA icon works", {

  library(stringr)

  # Emit a FontAwesome icon (`file`) as
  # SVG within `svg` tags; use height of 30px
  expect_true(
    fa(name = "file", fill = "purple") %>% as.character() %>% stringr::str_detect("fill:purple;"))

})

test_that("inserting the height attribute works for an FA icon works", {

  library(stringr)

  # Emit a FontAwesome icon (`file`) as
  # SVG within `svg` tags; use height of 30px
  expect_true(
    fa(name = "file", height = "30em") %>% as.character() %>% stringr::str_detect("height:30em;"))
})
