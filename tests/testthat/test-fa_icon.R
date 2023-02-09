library(dplyr)

test_that("Getting a basic FA icon works", {

  # Emit a Font Awesome icon (`file`) as SVG within `svg` tags;
  # refer to the icon with the "short" name
  expect_equal(
    as.character(fa(name = "file", prefer_type = "solid")),
    '<svg aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 384 512\" style=\"height:1em;width:0.75em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;\"><path d=\"M0 64C0 28.7 28.7 0 64 0H224V128c0 17.7 14.3 32 32 32H384V448c0 35.3-28.7 64-64 64H64c-35.3 0-64-28.7-64-64V64zm384 64H256V0L384 128z\"/></svg>'
  )

  expect_equal(
    as.character(fa(name = "file", prefer_type = "regular")),
    '<svg aria-hidden=\"true\" role=\"img\" viewBox=\"0 0 384 512\" style=\"height:1em;width:0.75em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;\"><path d=\"M320 464c8.8 0 16-7.2 16-16V160H256c-17.7 0-32-14.3-32-32V48H64c-8.8 0-16 7.2-16 16V448c0 8.8 7.2 16 16 16H320zM0 64C0 28.7 28.7 0 64 0H229.5c17 0 33.3 6.7 45.3 18.7l90.5 90.5c12 12 18.7 28.3 18.7 45.3V448c0 35.3-28.7 64-64 64H64c-35.3 0-64-28.7-64-64V64z\"/></svg>'
  )

  # Emit a Font Awesome icon (`file`) as SVG within `<svg>` tags;
  # refer to the icon with the "long" name
  expect_equal(
    as.character(fa(name = "fas fa-file")),
    as.character(fa(name = "file", prefer_type = "solid"))
  )
  expect_equal(
    as.character(fa(name = "far fa-file")),
    as.character(fa(name = "file", prefer_type = "regular"))
  )

  # Expect that every entry in the `fa_tbl` table will produce SVG text and,
  # only for the icons with a single style, the results are the same whether
  # the full name or short name is used

  fa_names_counts <-
    fontawesome:::fa_tbl %>%
    dplyr::select(name) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(n = n())

  fa_names_single <-
    fa_names_counts %>%
    dplyr::filter(n == 1) %>%
    dplyr::pull(name)

  fa_names_multiple <-
    fa_names_counts %>%
    dplyr::filter(n > 1) %>%
    dplyr::pull(name)

  fa_tbl_single_i <- which(fontawesome:::fa_tbl$name %in% fa_names_single)

  for (i in fa_tbl_single_i) {

    expect_true(
      grepl("^.svg.*svg.$", as.character(fa(name = fontawesome:::fa_tbl[[i, "name"]])))
    )

    expect_equal(
      as.character(fa(fontawesome:::fa_tbl[[i, "name"]])),
      as.character(fa(fontawesome:::fa_tbl[[i, "full_name"]]))
    )
  }

  # In that case that an icon cannot be retrieved,
  # expect that the function stops
  expect_error(fa(name = "fas fa-files"))
})

test_that("Inserting attributes and styles works for FA icons", {

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
    "width:.75em;"
  )

  expect_match(
    as.character(fa(name = "file", width = ".756789em")),
    "width:.756789em;"
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

  expect_match(
    as.character(fa(name = "file", height = "4em", width = "4em")),
    "width:4em;"
  )

  expect_match(
    as.character(fa(name = "file", height = "4em", width = "4em")),
    "height:4em;"
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

  expect_equal(
    as.character(icon),
    "<i class=\"fab fa-r-project\" role=\"presentation\" aria-label=\"r-project icon\"></i>"
  )

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

  expect_setequal(
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

  # Use a valid, fully-qualified icon name
  icon <- fa_i(name = "fab fa-r-project")

  # Expect that the same icon tag is produced with the fully-qualified
  # name compared to the short name
  expect_equal(
    as.character(icon),
    "<i class=\"fab fa-r-project\" role=\"presentation\" aria-label=\"r-project icon\"></i>"
  )

  # Expect an error if providing invalid input (non-character, length not one)
  # for `name`
  expect_error(fa_i(1))
  expect_error(fa_i(TRUE))
  expect_error(fa_i(c("0", "1")))
  expect_error(fa_i(character(0)))

  # There are *no* messages when using short name aliases or valid short
  # or long names
  expect_message(regexp = NA, fa_i("eur"))
  expect_message(regexp = NA, fa_i("vcard"))
  expect_message(regexp = NA, fa_i("euro-sign"))
  expect_message(regexp = NA, fa_i("fas fa-euro-sign"))

  # There is a message given when using an invalid name
  expect_message(regexp = "does not correspond to a known icon", fa_i("euroz"))

  # Providing an `html_dependency` object will also avoid checks that
  # would otherwise yield messages
  fake_dep <- htmltools::htmlDependency("fa", "1.0", "")
  expect_message(regexp = NA, fa_i("eur", html_dependency = fake_dep))
  expect_message(regexp = NA, fa_i("euroz", html_dependency = fake_dep))
})

test_that("Known alias names (for short name) result in retrival of icons", {

  # Get the complete set of known alias names for the included icons
  alias_names <- alias_tbl$alias

  # Expect that using each known alias with `fa()` will result in
  # an SVG string returned
  for (a_name in alias_names) {

    expect_true(
      grepl("^.svg.*svg.$", as.character(fa(name = a_name)))
    )
  }
})

test_that("The `fa_metadata()` function returns a list of metadata elements", {

  metadata <- fa_metadata()

  expect_type(metadata, "list")
})
