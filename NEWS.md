# fontawesome 0.2.2

* .ttf font files (and associated CSS) have been added back to the pared down selection of included webfonts (includes .woff and .ttf); this was to re-enable compatibility with the webshot package in Windows. (#61)

# fontawesome 0.2.1

* Closed #53: The `margin_right` argument of `fa()` is now functional, defaulting to the `"auto"` margin; a `margin_left` argument was also added with the same default. (#54)

* The `"desc"` option in `fa()`'s `a11y` argument is now changed to `"deco"`.

# fontawesome 0.2.0

* Closed #42: Support for additional R Markdown output formats with `fa()`: `pdf_document`, `word_document`, `github_document`, `slidy_presentation`, `beamer_presentation`, and `ioslides_presentation`. (#43)

* Closed #38: Add accessibility features to SVG icons produced by `fa()` with its new `a11y` argument. (#41)

* Fixes a rendering issue with SVG icons in IE11. (#40)

* Tooltips can be added to SVG icons prepared by `fa()` by using the new `title` argument.

* Closed #44: The collection of font files that support the use of `fa_i()` has been trimmed down to just the '.woff' variety. (#45)

* Closed #32, #33: Package dependencies have been greatly reduced. (#35)

# fontawesome 0.1.0

* Added functions `fa()`, `fa_i()`, and `fa_png()` for preparing Font Awesome icons in three ways.
