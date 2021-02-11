test_that("getting a basic FA icon works", {

  # Emit a FontAwesome icon (`file`) as SVG within `svg` tags;
  # refer to the icon with the 'short' name
  expect_equal(
    fa(name = "file") %>% as.character(),
    "<svg class=\"rfa\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>"
  )

  # Emit a FontAwesome icon (`file`) as SVG within `<svg>` tags;
  # refer to the icon with the 'long' name
  expect_equal(
    fa(name = "fas fa-file") %>% as.character(),
    "<svg class=\"rfa\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>"
  )

  # In that case that an icon cannot be retrieved,
  # expect that the function stops
  expect_error(fa(name = "fas fa-files"))
})

test_that("inserting the fill attribute works for an FA icon works", {

  # Expect that the `fill = "purple"` CSS rule is rendered
  expect_match(
    fa(name = "file", fill = "purple") %>% as.character(),
    "fill:purple;"
  )
})

test_that("inserting the height attribute works for an FA icon works", {

  # Expect that the `height = "30em"` CSS rule is rendered
  expect_match(
    fa(name = "file", height = "30em") %>% as.character(),
    "height:30em;"
  )
})
