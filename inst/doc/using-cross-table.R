## ----library, echo=TRUE, message=FALSE, warning=FALSE, results="hide"---------
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets
library(rtables)

## ----data, echo=TRUE, message=FALSE, warning=FALSE, results="hide", echo=2:6----
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADLB <- teal.data::rADLB %>%
    mutate(CHGC = as.factor(case_when(
      CHG < 1 ~ "N",
      CHG > 1 ~ "P",
      TRUE ~ "-"
    )))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

## ----app, echo=TRUE, message=FALSE, warning=FALSE, results="hide"-------------
# configuration for the single wide dataset
mod1 <- tm_t_crosstable(
  label = "Single wide dataset",
  x = data_extract_spec(
    "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = names(data[["ADSL"]])[5],
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE
    )
  ),
  y = data_extract_spec(
    "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = names(data[["ADSL"]])[6],
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (different subsets)
mod2 <- tm_t_crosstable(
  label = "Same long datasets (different subsets)",
  x = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
      selected = levels(data[["ADLB"]]$PARAMCD)[1],
      multiple = FALSE
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]]),
      selected = "AVISIT",
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
      selected = levels(data[["ADLB"]]$PARAMCD)[1],
      multiple = FALSE
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]]),
      selected = "LOQFL",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Cross table",
      mod1,
      mod2
    )
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

