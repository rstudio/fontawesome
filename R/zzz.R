#nocov start

.onLoad <- function(...) {
  s3_register("knitr::knit_print", "fontawesome")

  fa_dependency_obj <<-
    htmltools::htmlDependency(
      name = "font-awesome",
      version = fa_version,
      src = "fontawesome",
      package = "fontawesome",
      stylesheet = c("css/all.min.css", "css/v4-shims.min.css")
    )
}

#nocov end
