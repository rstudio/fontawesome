test_that("getting a basic FA icon works", {

  # Emit a Font Awesome icon (`file`) as SVG within `svg` tags;
  # refer to the icon with the 'short' name
  expect_snapshot(fa(name = "file"))

  # Emit a Font Awesome icon (`file`) as SVG within `<svg>` tags;
  # refer to the icon with the 'long' name
  expect_snapshot(fa(name = "fas fa-file"))

  # In that case that an icon cannot be retrieved,
  # expect that the function stops
  expect_error(fa(name = "fas fa-files"))
})

test_that("inserting attributes and styles works for FA icons", {

  # Expect that the `fill = "purple"` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", fill = "purple")),
    "fill:purple;"
  )

  # Expect that the `fill_opacity = 0.5` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", fill = "purple", fill_opacity = 0.5)),
    "fill:purple;overflow:visible;fill-opacity:0.5;"
  )

  # Expect that the stroke CSS rules are rendered
  expect_match(
    as.character(
      fa(
        name = "file",
        stroke = "blue",
        stroke_width = "2px",
        stroke_opacity = 0.5
        )
      ),
    "stroke:blue;stroke-width:2px;stroke-opacity:0.5;"
  )

  # Expect that the `height = "30em"` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", height = "30em")),
    "height:30em;"
  )

  # Expect a default height of 1em
  expect_match(
    as.character(fa(name = "file")),
    "height:1em;"
  )

  # Expect that the `width = "1em"` CSS rule is rendered
  expect_match(
    as.character(fa(name = "file", width = "1em")),
    "width:1em;"
  )

  # Expect that fractional width values are rendered properly
  expect_match(
    as.character(fa(name = "file", width = "0.75em")),
    "width:0.75em;"
  )
  expect_match(
    as.character(fa(name = "file", width = ".75em")),
    "width:0.75em;"
  )

  expect_match(
    as.character(fa(name = "file", width = ".756789em")),
    "width:0.756789em;"
  )

  # Expect that not supplying a width value will result in an error
  expect_error(
    as.character(fa(name = "file", width = "em"))
  )

  # Expect that not giving a length unit will result in an error
  expect_error(
    as.character(fa(name = "file", width = "1"))
  )
  expect_error(
    as.character(fa(name = "file", width = 1))
  )

  # Expect that supplying an empty string will result in an error
  expect_error(
    as.character(fa(name = "file", width = ""))
  )

  # Expect that ending with a dot should result in an error
  expect_error(
    as.character(fa(name = "file", width = "1.em"))
  )

  # Expect that supplying a dot for the width value will result in an error
  expect_error(
    as.character(fa(name = "file", width = ".em"))
  )

  # Expect a default width of 0.75em
  expect_match(
    as.character(fa(name = "file")),
    "width:0.75em;"
  )

  # Expect that the `margin_left = "1em"` and `margin_right = "1em"`
  # CSS rules are rendered
  expect_match(
    as.character(fa(name = "file", margin_left = "1em", margin_right = "1em")),
    "margin-left:1em;margin-right:1em;"
  )
  # Expect default values of "auto" for both rules
  expect_match(
    as.character(fa(name = "file")),
    "margin-left:auto;margin-right:auto;"
  )
})

test_that("the `fa_i()` function returns an icon object", {

  icon <- fa_i(name = "r-project")

  expect_snapshot(cat(as.character(icon)))

  # Expect that the `icon` object is a `shiny.tag`
  expect_s3_class(icon, "shiny.tag")

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

  # Expect the style property to render in the `<i>` tag
  expect_snapshot(
    cat(as.character(fa_i(name = "r-project", height = "20px", title = "R project")))
  )

  # Use a valid, fully-qualified icon name
  icon <- fa_i(name = "fab fa-r-project")

  # Expect that the same icon tag is produced with the fully-qualified
  # name compared to the short name
  expect_equal(
    as.character(icon),
    "<i class=\"fab fa-r-project\" role=\"presentation\" aria-label=\"r-project icon\"></i>"
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
