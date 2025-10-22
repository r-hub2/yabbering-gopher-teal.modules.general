## ----library, echo=TRUE, message=FALSE, warning=FALSE, results="hide"---------
library(teal.modules.general) # used to create the app

## ----data, echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
  ADLB <- teal.data::rADLB
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

## ----app, echo=TRUE, message=FALSE, warning=FALSE, results="hide"-------------
# configuration for the two-datasets example
mod1 <- tm_data_table(
  label = "Two datasets",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADTTE = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "ARM", "ARMCD", "AVAL", "CNSR"
    )
  )
)

# configuration for the subsetting or changing order of datasets
mod2 <- tm_data_table(
  label = "Datasets order",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADLB = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
    )
  ),
  datanames = c("ADTTE", "ADLB", "ADSL")
)

# configuration for the advanced usage of DT options and extensions
mod3 <- tm_data_table(
  label = "Advanced DT usage",
  dt_args = list(extensions = c("Buttons", "ColReorder", "FixedHeader")),
  dt_options = list(
    searching = FALSE,
    pageLength = 30,
    lengthMenu = c(5, 15, 25, 50, 100),
    scrollX = FALSE,
    dom = "lBrtip",
    buttons = c("copy", "csv", "excel", "pdf", "print"),
    colReorder = TRUE,
    fixedHeader = TRUE
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    mod1,
    mod2,
    mod3
  )
)

## ----shinyapp, eval=FALSE-----------------------------------------------------
# shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))

## ----shinylive_url, echo = FALSE, results = 'asis', eval = requireNamespace("roxy.shinylive", quietly = TRUE)----
code <- paste0(c(
  knitr::knit_code$get("library"),
  knitr::knit_code$get("data"),
  knitr::knit_code$get("app"),
  knitr::knit_code$get("shinyapp")
), collapse = "\n")

url <- roxy.shinylive::create_shinylive_url(code)
cat(sprintf("[Open in Shinylive](%s)\n\n", url))

## ----shinylive_iframe, echo = FALSE, out.width = '150%', out.extra = 'style = "position: relative; z-index:1"', eval = requireNamespace("roxy.shinylive", quietly = TRUE) && knitr::is_html_output() && identical(Sys.getenv("IN_PKGDOWN"), "true")----
# knitr::include_url(url, height = "800px")

