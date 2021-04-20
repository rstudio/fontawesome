test_that("getting a basic FA icon works", {

  # Emit a Font Awesome icon (`file`) as SVG within `svg` tags;
  # refer to the icon with the 'short' name
  expect_equal(
    as.character(fa(name = "file")),
    "<svg aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 384 512\" style=\"height:1em;width:0.75em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>"
  )

  # Emit a Font Awesome icon (`file`) as SVG within `<svg>` tags;
  # refer to the icon with the 'long' name
  expect_equal(
    as.character(fa(name = "fas fa-file")),
    "<svg aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 384 512\" style=\"height:1em;width:0.75em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;\"><path d=\"M224 136V0H24C10.7 0 0 10.7 0 24v464c0 13.3 10.7 24 24 24h336c13.3 0 24-10.7 24-24V160H248c-13.2 0-24-10.8-24-24zm160-14.1v6.1H256V0h6.1c6.4 0 12.5 2.5 17 7l97.9 98c4.5 4.5 7 10.6 7 16.9z\"/></svg>"
  )

  # In that case that an icon cannot be retrieved,
  # expect that the function stops
  expect_error(fa(name = "fas fa-files"))
})

test_that("inserting the fill attribute works for an FA icon works", {

  # Expect that the `fill = "purple"` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", fill = "purple")),
    "fill:purple;"
  )
})

test_that("inserting the height attribute works for an FA icon works", {

  # Expect that the `height = "30em"` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", height = "30em")),
    "height:30em;"
  )
})

test_that("the `fa_i()` function returns an icon object", {

  icon <- fa_i(name = "r-project")

  expect_equal(
    as.character(icon),
    "<i class=\"fab fa-r-project\" role=\"presentation\" aria-label=\"r-project icon\"></i>"
  )

  # Expect that the `icon` object is a `shiny.tag`
  expect_is(icon, "shiny.tag")

  # Expect that the `icon` object is a list with
  # specific element names
  expect_equal(names(icon), c("name", "attribs", "children"))

  # For this object, expect certain values within
  # the list components
  expect_equal(icon$name, "i")
  expect_equal(icon$attribs$class, "fab fa-r-project")
  expect_equal(icon$attribs$role, "presentation")
  expect_equal(icon$attribs$`aria-label`, "r-project icon")
  expect_equal(icon$children, list())

  # Expect there are certain attributes available
  icon_attributes <- attributes(icon)

  expect_equal(
    names(icon_attributes),
    c("names", "class", "html_dependencies", "browsable_html")
  )

  # Add a style rule to the icon
  icon_2 <- fa_i(name = "r-project", height = "20px")

  # Expect the style property to render in the `<i>` tag
  expect_equal(
    as.character(icon_2),
    "<i class=\"fab fa-r-project\" role=\"presentation\" aria-label=\"r-project icon\" height=\"20px\"></i>"
  )
})

test_that("the user can quell messages in `fa_i()`", {

  # There are messages when using FA 4 short names or invalid names
  expect_message(regexp = "deprecated in Font Awesome 5", fa_i("eur"))
  expect_message(regexp = "does not exist", fa_i("euroz"))

  # If using a FA 5 name then no message is seen
  expect_message(regexp = NA, fa_i("euro-sign"))
  expect_message(regexp = NA, fa_i("fas fa-euro-sign"))

  # If using `verify_fontawesome = FALSE`, no messages will be shown
  expect_message(regexp = NA, fa_i("eur", verify_fa = FALSE))
  expect_message(regexp = NA, fa_i("euroz", verify_fa = FALSE))

  # Providing an `html_dependency` object will also avoid checks that
  # would otherwise yield messages
  expect_message(regexp = NA, fa_i("eur", html_dependency = fa_html_dependency()))
  expect_message(regexp = NA, fa_i("euroz", html_dependency = fa_html_dependency()))
})
