## ----library, echo=TRUE, message=FALSE, warning=FALSE, results="hide"---------
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets

## ----data, echo=TRUE, message=FALSE, warning=FALSE, results="hide"------------
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADRS <- teal.data::rADRS
  ADLB <- teal.data::rADLB
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

## ----app, echo=TRUE, message=FALSE, warning=FALSE, results="hide"-------------
# configuration for the single wide dataset
mod1 <- tm_outliers(
  label = "Single wide dataset",
  outlier_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = "AGE",
      fixed = FALSE
    )
  ),
  categorical_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(
        data[["ADSL"]],
        subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
      ),
      selected = "RACE",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod2 <- tm_outliers(
  label = "Wide and long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
        selected = "AGE",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var =
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADSL"]],
          subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
)

# configuration for the multiple long datasets
mod3 <- tm_outliers(
  label = "Multiple long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADRS"]], c("ADY", "EOSDY")),
        selected = "ADY",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(data[["ADRS"]], c("ARM", "ACTARM")),
        selected = "ARM",
        multiple = FALSE,
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADLB"]],
          subset = names(Filter(isTRUE, sapply(data[["ADLB"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_outliers ----
    modules(
      label = "Outliers module",
      mod1,
      mod2,
      mod3
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

